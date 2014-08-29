#ifndef __FNC_USERCOMMAND_INC__03222006__
#define __FNC_USERCOMMAND_INC__03222006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"
#include "../lib/synch_class.h"

#include "fnc_scopes.h"
#include <windows.h>

// User command calls structure
// It should store the stack values needed for nested function calls
struct usercommand_call_t
{
  size_t return_line;
  object_t params;
  size_t level;

  void assign(const usercommand_call_t &rhs)
  {
    return_line = rhs.return_line;
    params = rhs.params;
    level = rhs.level;
  }

  usercommand_call_t(const usercommand_call_t &rhs)
  {
    assign(rhs);
  }

  void operator=(const usercommand_call_t &rhs)
  {
    assign(rhs);
  }

  usercommand_call_t()
  {
    return_line = COMPEL_BAD_INSTRUCTION_LINE;
    level = 0;
  }
};

typedef std::stack<usercommand_call_t> usercommand_call_stack_t;

class fnc_usercommand : public function_t
{
  friend class fnc_scopes;
private:
  typedef std::map<size_t, fnc_usercommand *> function_scope_map_t;

  static function_scope_map_t _fnc_list;
  struct usercommand_call_stack_ctx_t 
  {
    DWORD TlsIndex;
    usercommand_call_stack_t callstack;

    usercommand_call_stack_ctx_t() { }

    usercommand_call_stack_ctx_t &operator=(const usercommand_call_stack_ctx_t &rhs) 
    {
      assign(rhs);
    }

    usercommand_call_stack_ctx_t(const usercommand_call_stack_ctx_t &rhs) 
    { 
      assign(rhs);
    }

    void assign(const usercommand_call_stack_ctx_t &rhs)
    {
      callstack = rhs.callstack;
      TlsIndex = rhs.TlsIndex;
    }
  };
  typedef std::map<DWORD, usercommand_call_stack_ctx_t> tid_callstack_map_t;

  static tid_callstack_map_t _per_thread_callstack;

  static usercommand_call_stack_t &get_usercommand_call_stack();
  static void clear_usercommand_call_stack();

  fnc_usercommand(const fnc_usercommand &) { }
  void operator =(const fnc_usercommand &) { }

  static criticalsection_class _prot;

  //compel_fnc_scopes::ufnc_context_t _ctx;
  size_t _scopeid;
  std::string _fnc_name;
  compel_string_tokenizer_t _paramslist;

  static parse_errors_e enter_function(interpreter_t *);
  static parse_errors_e leave_function(interpreter_t *, bool b_ret_value = false);
  static parse_errors_e register_top_fargs(interpreter_t *);
public:
  fnc_usercommand(interpreter_t *);
  fnc_usercommand();
  ~fnc_usercommand();

  parse_errors_e execute();
  parse_errors_e register_functions(compel_fnc_scopes::ufnc_context_list_t &fncs);
  size_t version;
  static const char *fnc_name;
};

#endif