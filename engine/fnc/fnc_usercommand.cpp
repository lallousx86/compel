#include "fnc_usercommand.h"
#include "../function_helper.h"

using namespace compel_fnc_scopes;

static const LPVOID c_tls_signature = (LPVOID)'CMPL';

static const char *s_str_localvar = "$fargs";
static const char *s_str_count = "_count";
static const char *s_str_arg_zero = "0";
static const char *s_str_level = "_level";
static const char *s_str_version = "_version";

criticalsection_class fnc_usercommand::_prot;

const char *fnc_usercommand::fnc_name = "usercommand";

// call stack
fnc_usercommand::tid_callstack_map_t fnc_usercommand::_per_thread_callstack;

/*! 
\brief Returns the user command callstack for the given thread.
This method is tricky because even if we have created a map entry for the given threadID
it might happen that a thread dies and another thread takes its ID
thus we might inherit callstack of another thread. Unless we put some special
value explicitly into each new thread, we can't tell whether an ID is new or recycled.
*/
usercommand_call_stack_t &fnc_usercommand::get_usercommand_call_stack()
{
  DWORD tid = ::GetCurrentThreadId();
  tid_callstack_map_t::_Pairib ib;
  
  ib.first = _per_thread_callstack.find(tid);

  // entry for this thread exists?
  if (ib.first != _per_thread_callstack.end())
  {
    usercommand_call_stack_ctx_t &ctx = ib.first->second;

    LPVOID val = ::TlsGetValue(ctx.TlsIndex);

    // correct TLS value?
    if (val == c_tls_signature)
      return ctx.callstack;
    // erase incorrect (or recycled thread callstack)
    else
      _per_thread_callstack.erase(ib.first);
  }

  // entry does not exist
  usercommand_call_stack_ctx_t dum;

  // Allocate a TLS
  dum.TlsIndex = ::TlsAlloc();
  
  // Store magic value for this thread so that we know we initiated it
  ::TlsSetValue(dum.TlsIndex, c_tls_signature);

  ib = _per_thread_callstack.insert(tid_callstack_map_t::value_type(tid, dum));

  return ib.first->second.callstack;
}

void fnc_usercommand::clear_usercommand_call_stack()
{
  _per_thread_callstack.clear();
  /*
  for (tid_callstack_map_t::iterator it=_per_thread_callstack.begin();
    it != _per_thread_callstack.end();
    ++it)
  {
    usercommand_call_stack_t &callstack = it->second.callstack;

    // Clear call stack
    while (!callstack.empty())
      callstack.pop();
  }
  */
}

// functions list
fnc_usercommand::function_scope_map_t fnc_usercommand::_fnc_list;

fnc_usercommand::fnc_usercommand(interpreter_t *interpreter)
{
  set_interpreter(interpreter);
  version = 0;
}

fnc_usercommand::fnc_usercommand()
{
}

fnc_usercommand::~fnc_usercommand()
{
}

parse_errors_e fnc_usercommand::register_functions(ufnc_context_list_t &fncs)
{
  ufnc_context_list_t::iterator it;
  interpreter_t *_int = get_interpreter();
  bool bFailed = false;

  for (it=fncs.begin(); it != fncs.end();++it)
  {
    // register function
    fnc_usercommand *uf = new fnc_usercommand(_int);  
    if (_int->add_symbol(it->fnc_name.c_str(), uf) == 0)
    {
      delete uf;
      // get existing registered command
      uf = static_cast<fnc_usercommand *>(_int->find_symbol(it->fnc_name.c_str()));
    }

    uf->version++;

    // register variable of function
    std::string fnc_var = '$' + it->fnc_name;

    // replace the '.' in the function name with '_'
    {
      using namespace std;
      string::size_type pos;
      while ((pos = fnc_var.find('.')) != string::npos)
      {
        fnc_var[pos] = '_';
      }
    }

    value_t *val = new value_t;
    if (_int->add_symbol(fnc_var.c_str(), val) == 0)
    {
      delete val;
      val = static_cast<value_t *>(_int->find_symbol(fnc_var.c_str()));
      /*
      std::string err_str = "function variable '" + fnc_var + "' alredy defined!";
      _int->set_msg_error(err_str.c_str());
      bFailed = true;
      break;
      */
    }

    // save this function in the user-function list
    _fnc_list[it->scope_id] = uf;

    // save some info from the context
    uf->_scopeid = it->scope_id;
    uf->_fnc_name = it->fnc_name;

    // set params
    uf->_paramslist.parse(it->fnc_param.c_str());

    // least param count is the ones declared in the function definition
    uf->set_minmaxargs(uf->_paramslist.parsed_count(), 0);
  }

  // clean up exit
  if (bFailed)
  {
    return parse_error_symbol_redefinition;
  }

  // clear all callstacks
  clear_usercommand_call_stack();

  return parse_error_none;
}

parse_errors_e fnc_usercommand::execute()
{
  usercommand_call_t call;
  interpreter_t *_int = get_interpreter();

  // find the scope of the addressed function
  scope_context_t *scopectx = fnc_scopes::find_scope(_scopeid);
  if (scopectx == 0)
    return parse_error_function_expected;

  // set return line
  call.return_line = _int->get_cur_source_lineno() + 1;
  
  // parse actual passed params - can be more than the ones defined in the function
  std::string str_temp;
  function_helper::eval_from_to(_int, 1, _int->get_fnc_arg_count(), str_temp, true);

  // replace all the '&$' with '$'
  {
    std::string::size_type pos;
    std::string str_ref = "&";
    str_ref.push_back(interpreter_t::get_variable_prefix());
    while ((pos = str_temp.find(str_ref)) != std::string::npos)
    {
      str_temp.erase(pos, 1);
    }
  }
  compel_string_tokenizer_t actualparams;
  size_t nActualParams = actualparams.parse(str_temp.c_str());

  // insert - fargs.count - attribute
  value_t temp_val;

  temp_val.set_int_value((long) nActualParams);
  call.params.insert_attribute(s_str_count, temp_val);
  
  // insert function name
  temp_val.set_str_value(_fnc_name.c_str());
  call.params.insert_attribute(s_str_arg_zero, temp_val);

  // increment function call nesting level
  usercommand_call_stack_t &callstack = get_usercommand_call_stack();
  if (callstack.empty())
    call.level = 1;
  else
    call.level = callstack.top().level + 1;

  // register the level variable
  temp_val.set_int_value((long)call.level);
  call.params.insert_attribute(s_str_level, temp_val);

  // register the command's version (or count of redifinition)
  temp_val.set_int_value((long)version);
  call.params.insert_attribute(s_str_version, temp_val);

  // register the rest of parameters
  for (size_t i=0;i<nActualParams;i++)
  {
    // if param is within bounds of defined parameters, then register by name
    if (i < _paramslist.parsed_count())
    {
      str_temp = _paramslist.get_string(i);
      if (interpreter_t::is_variable_name(str_temp.c_str()))
        str_temp = str_temp.substr(1); // skip the '$' sign
      call.params.insert_attribute(str_temp.c_str(), value_t(actualparams.get_string(i)));
    }
    // register additional params by number
    else
    {
      char temp[15];
      sprintf(temp, "%d", i+1);
      call.params.insert_attribute(temp, value_t(actualparams.get_string(i)));
    }
  }
  
  // update old FARGS if it was present
  if (!callstack.empty())
  {
    criticalsection_scope_protect_class prot(_prot);
    object_t *old_fargs = static_cast<object_t *>(_int->find_symbol(s_str_localvar));
    if (old_fargs)
    {
      //;!printf("found local symbol by: %08X @ %08X\n", ::GetCurrentThreadId(), ::GetTickCount());
      callstack.top().params = *old_fargs;
    }
  }

  // now push the call info to the callstack
  callstack.push(call);

  // ...and go to function body
  _int->set_cur_src_line(scopectx->begin);

  return parse_branch_to;

#if 0
  // debug
  call.params.to_string(str_temp);
  printf("[%s:%d](%s)\n", _fnc_name.c_str(), actualparams.parsed_count(), str_temp.c_str());
#endif

  return parse_error_none;
}

parse_errors_e fnc_usercommand::register_top_fargs(interpreter_t *_int)
{
  criticalsection_scope_protect_class prot(_prot);

  // remove old FARGS
  _int->remove_symbol(s_str_localvar);

  //;!printf("removed local symbol by: %08X @ %08X\n", ::GetCurrentThreadId(), ::GetTickCount());

  usercommand_call_stack_t &callstack = get_usercommand_call_stack();

  if (callstack.empty())
    return parse_error_wrong_context;

  // register new FARGS (bring it forward)
  object_t *fargs = new object_t(callstack.top().params);

  _int->add_symbol(s_str_localvar, fargs);

  //;!printf("added local symbol by: %08X @ %08X\n", ::GetCurrentThreadId(), ::GetTickCount());

  return parse_error_none;
}

parse_errors_e fnc_usercommand::enter_function(interpreter_t *_int)
{
  // The steps we need to do:
  // ---------------------------
  // . check if we have a callstack
  // . unregister old local FARGS variable
  // . register the top fargs value

  // do we have a call stack?
  usercommand_call_stack_t &callstack = get_usercommand_call_stack();
  if (callstack.empty())
    return parse_error_function_expected;

  // register variable
  return register_top_fargs(_int);
}

parse_errors_e fnc_usercommand::leave_function(interpreter_t *_int, bool b_ret_value)
{
  // should we set return value?
  if (b_ret_value)
  {
    // get the function's scope
    size_t scope_id = compel_string_tokenizer_t::parse_number(_int->get_const_at(1));

    // try to find the function's struct
    function_scope_map_t::iterator it = _fnc_list.find(scope_id);
    if (it != _fnc_list.end())
    {
      // form the function's return value variable name
      std::string fnc_varname = '$' + it->second->_fnc_name;

      if (_int->get_fnc_arg_count() >= 3)
      {
        std::string retval;
        function_helper::eval_from_to(_int, 2, _int->get_fnc_arg_count(), retval, false, 0);
        
        // find the return variable associated with the function's name
        if (value_t *val = _int->get_value(fnc_varname.c_str()))
        {
          val->set_str_value(retval.c_str());
        }
      }
    }
  }
  // remove the FARGS symbol
  _int->remove_symbol(s_str_localvar);
  
  // get TOP call info
  usercommand_call_stack_t &callstack = get_usercommand_call_stack();
  usercommand_call_t call = callstack.top();

  // pop CALL stack info
  callstack.pop();

  register_top_fargs(_int);

  _int->set_cur_src_line(call.return_line);

  return parse_branch_to;
}