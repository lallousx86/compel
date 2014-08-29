#include "fnc_thread.h"
#include "../interpreter_helper.h"
#include "../parse_util.h"

#pragma warning (disable:4312)

static const char *s_ns_name = "thread.";
const char *fnc_thread::fnc_name = "thread";
static const char *s_create = "create";
static const char *s_wait = "wait";
static const char *s_exit = "exit";
static const char *s_now = "now";

fnc_thread::thread_context_list_t fnc_thread::_threads_list;

fnc_thread::fnc_thread(interpreter_t *interpreter, op_e op)
{
  set_interpreter(interpreter);
  _op = op;

  switch (_op)
  {
  case op_create:
    set_namedesc(s_create, "$result_thread_obj user_command [params...] <- creates a new thread that runs the user command");
    set_minmaxargs(2, 0);
    break;
  case op_wait:
    set_namedesc(s_wait, "$thread_obj [wait_val] <- waits for the given thread to terminate");
    set_minmaxargs(1, 0);
    break;
  case op_exit:
    set_namedesc(s_exit, "$thread_obj [NOW]<- send terminate signal to a given thread or terminates it abruptly");
    set_minmaxargs(1, 0);
    break;
  }
}

fnc_thread::fnc_thread()
{
  _op = op_unknown;
}

parse_errors_e fnc_thread::execute()
{
  switch (_op)
  {
  case op_create:
    return execute_create();
  case op_wait:
    return execute_wait();
  case op_exit:
    return execute_exit();
  }
  return parse_error_function_expected;
}

parse_errors_e fnc_thread::register_function(interpreter_t *interpreter)
{
  fnc_thread *functions[] =
  {
    new fnc_thread(interpreter, op_create),
    new fnc_thread(interpreter, op_wait),
    new fnc_thread(interpreter, op_exit)
  };

  std::string name;
  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
  {
    name = s_ns_name + functions[i]->_name;
    interpreter->add_symbol(name.c_str(), functions[i]);
  }

  return parse_error_none;
}

/*!
\brief Creates a new thread and runs it
When called as:
thread.create mythreadproc param1 param2 
Internally what it does is this:
1. take a copy of the script lines
2. add this line to the the end of the script lines:
mythreadproc param1 param2
3. creates new interpreter
4. assigns the lines to new_int
5. binds the symbol table (all interpreters has same symtbl)
6. new_int->src_line_no = last line
7. create a thread context
8. save all variables in it
9. create a windows thread
10. resume the thread passing it the context
11. using the context, the thread can use the new_int to run the script
*/
parse_errors_e fnc_thread::execute_create()
{
  static const int PAR_TOBJ = 1;
  static const int PAR_CMDCALL = 2;

  interpreter_t *_int = get_interpreter();

  interpreter_helper_t hlp(_int);

  value_t *val = _int->get_value_at(PAR_TOBJ);
  if (val == 0)
    return parse_error_variable_expected;
  
  // get a copy of the source lines from the initial interpreter
  lines_t lines = lines_t(hlp.get_interpreter_lines());

  // insert new entry
  thread_context_t *ctx;
  int tid = (int) _threads_list.size();
  _threads_list.push_back(thread_context_t());
  ctx = &_threads_list.back();

  // create new interpreter
  ctx->linked_int = new interpreter_t();

  // link the symbol tables
  ctx->linked_int->set_symbol_table(hlp.get_interpreter_symtbl());

  // assign the main class instance
  ctx->_fnc_thread = this;

  // get the last line_no
  size_t new_srclineno = lines.count();

  // Form the thread command-call line
  std::string cmd_call;
  for (size_t i=2;i<_int->get_fnc_arg_count();i++)
  {
    cmd_call += _int->get_const_at(i);
    cmd_call.push_back(' ');
  }

  // add a new line that will call the user command
  lines.add(cmd_call);

  // load lines into new interpreter
  ctx->linked_int->load_from_lines(lines, true);

  // adjust the source line number
  ctx->linked_int->set_cur_src_line(new_srclineno);

  // Create the thread
  ctx->hthread = ::CreateThread(0, 0, 
    (LPTHREAD_START_ROUTINE) InterpreterThread, 
    reinterpret_cast<LPVOID>(tid), 
    CREATE_SUSPENDED, 
    &ctx->tid);

  // ;!
  interpreter_helper_t hlpnew(ctx->linked_int);
  hlpnew.get_bind_struct()->tid = ctx->tid;
  hlpnew.get_bind_struct()->int_bind = _int;

  val->set_int_value(tid);

  // Run the thread
  ::ResumeThread(ctx->hthread);

  return parse_error_none;
}

/*!
\brief Win32 thread that will step by step the usercommand
When the user command returns, the script will finish since it will be @ the
last line
\returns -1 when some missing pointers are there
\returns 0 if script ended
\returns 1 if script exit signaled
*/
DWORD WINAPI fnc_thread::InterpreterThread(int tid)
{
  // get the thread context given the index
  thread_context_list_t::iterator it = _threads_list.begin();
  std::advance(it, tid);
  thread_context_t *ctx = &*it;

  // get pointer to 'this'
  fnc_thread *_this = ctx->_fnc_thread;

  if (ctx->linked_int == 0)
    return -1;

  parse_errors_e err = parse_error_none;
  while (err == parse_error_none)
  {
    if (ctx->b_exit_signaled)
      break;

    err = ctx->linked_int->interpret_line();
    if (err == parse_stop_parsing)
      break;

    switch (err)
    {
    case parse_error_line_is_comment:
    case parse_error_line_is_void:
    case parse_line_is_empty:
      err = parse_error_none;
      break;
    }

    /*
    if (err != parse_error_none)
    {
      printf("err %d @ %d\n", (int) err, _int.get_cur_source_lineno());
    }
    */
  }

  // dispose the linked interpreter
  delete ctx->linked_int;
  ctx->linked_int = 0;

  return ctx->b_exit_signaled ? 1 : 0;
}

/*!
\brief Returns a thread_context_t from the interpeter parameter pointed by 'idx'
*/
fnc_thread::thread_context_t *fnc_thread::parse_tctx(int idx)
{
  thread_context_t *ctx = 0;
  fnc_thread *_this = 0;
  const char *str = get_interpreter()->evaluate_at(idx);

  try
  {
    // check index boundaries
    idx = (int) parse_util::parse_number(str);
    if (idx < 0 || idx > (int)_threads_list.size())
      return 0;

    // get the structure
    thread_context_list_t::iterator it = _threads_list.begin();
    std::advance(it, idx);
    ctx = &*it;

    // see if this is a valid structure (we rely on C++'s RTTI)
    _this = dynamic_cast<fnc_thread *>(ctx->_fnc_thread);
  }
  catch(...)
  {
    ctx = 0;
  }

  if (_this == 0)
    return 0;

  return ctx;
}

/*!
\brief Waits for the given thread to terminate
If the wait was satisfied, this function closes the thread handle
\param tid thread id
\param [optional] wait_milli_secs
*/
parse_errors_e fnc_thread::execute_wait()
{
  static const int PAR_TCTX = 1;
  static const int PAR_NWAIT = 2;

  interpreter_t *_int = get_interpreter();

  DWORD nWait = INFINITE;

  if (_int->get_fnc_arg_count() > PAR_NWAIT)
    nWait = parse_util::parse_number(_int->evaluate_at(PAR_NWAIT));

  thread_context_t *ctx = parse_tctx(PAR_TCTX);
  if (ctx == 0)
    return parse_error_symbol_type_mismatch;

  // No thread was created?
  if (ctx->hthread == 0)
    return parse_error_wrong_context;

  if (ctx->hthread != 0)
  {
    if (::WaitForSingleObject(ctx->hthread, nWait) == WAIT_OBJECT_0)
    {
      ::CloseHandle(ctx->hthread);
      ctx->hthread = 0;
    }
  }
  return parse_error_none;
}

/*!
\brief Signals termination signal to the thread and waits for it to finish its last command
\param tid The thread id (or index)
\param [optional] NOW - to tell whether to terminate the thread abruptly instead of waiting it
*/
parse_errors_e fnc_thread::execute_exit()
{
  /*
  for (size_t i=0;i<_threads_list.size();i++)
  {
    thread_context_t *ctx = &_threads_list[i];
    ResumeThread(ctx->hthread);
  }
  return parse_error_none;
  */
  static const int PAR_TCTX = 1;
  static const int PAR_NOW = 2;

  thread_context_t *ctx = parse_tctx(PAR_TCTX);
  if (ctx == 0)
    return parse_error_symbol_type_mismatch;

  interpreter_t *_int = get_interpreter();
  
  if (ctx->hthread != 0)
  {
    // abrupt termination?
    if (
      _int->get_fnc_arg_count() > PAR_NOW && 
      (stricmp(_int->get_const_at(PAR_NOW), s_now) == 0)
      )
    {
      ::TerminateThread(ctx->hthread, -2);
    }
    // should we signal and wait?
    else
    {
      ctx->b_exit_signaled = true;
      ::WaitForSingleObject(ctx->hthread, INFINITE);
    }
    ::CloseHandle(ctx->hthread);
    ctx->hthread = 0;
  }
  return parse_error_none;
}
