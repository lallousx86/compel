#ifndef __FNC_THREAD_08052006__
#define __FNC_THREAD_08052006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"
#include <list>
#include <windows.h>

class fnc_thread : public function_t
{
public:
  enum op_e
  {
    op_unknown,
    op_create,
    op_wait,
    op_exit
  };
private:
  op_e _op;

  struct thread_context_t
  {
    DWORD tid;
    HANDLE hthread;
    bool b_exit_signaled;
    interpreter_t *linked_int;
    fnc_thread *_fnc_thread;

    thread_context_t &operator=(const thread_context_t &rhs)
    {
      assign(rhs);
      return *this;
    }

    thread_context_t(const thread_context_t &rhs)
    {
      assign(rhs);
    }

    void assign(const thread_context_t &rhs)
    {
      tid = rhs.tid;
      hthread = rhs.hthread;
      b_exit_signaled = rhs.b_exit_signaled;
      linked_int = rhs.linked_int;
      _fnc_thread = rhs._fnc_thread;
    }

    thread_context_t()
    {
      tid = 0;
      hthread = 0;
      linked_int = 0;
      b_exit_signaled = false;
      _fnc_thread = 0;
    }
  };

  typedef std::list<thread_context_t> thread_context_list_t;
  
  static thread_context_list_t _threads_list;

  parse_errors_e execute_create();
  parse_errors_e execute_wait();
  parse_errors_e execute_exit();

  static DWORD WINAPI InterpreterThread(int tid);
  thread_context_t *parse_tctx(int idx);
public:
  static const char *fnc_name;

  fnc_thread(interpreter_t *, op_e);

  fnc_thread();

  parse_errors_e execute();
  parse_errors_e register_function(interpreter_t *);
};

#endif