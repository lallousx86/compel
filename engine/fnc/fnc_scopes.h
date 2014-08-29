#ifndef __FNC_SCOPES__02152006__
#define __FNC_SCOPES__02152006__

#ifndef _COMPEL_DEBUG_SCOPE
  #include "../fwd.h"
  #include "../function.h"
  #include "../interpreter.h"
  #include "../symbol_table.h"
  #include <stack>
#endif

namespace compel_fnc_scopes
{
  struct scope_binding_t; // fwd

  struct scope_context_t
  {
    size_t id;
    size_t level;
    size_t begin;
    size_t end;
    scope_binding_t *binding;
    scope_context_t()
    {
      id = 0;
      binding = 0;
    }
  };

  struct if_context_t
  {
    size_t if_line, else_line;
    size_t begin1, begin2;
    size_t end1, end2;
    size_t scope_id1, scope_id2;
    size_t level;
    std::string opr1, opr2, op;
  };

  struct brkcnt_context_t
  {
    size_t cmd_line;
    size_t level;
    std::string misc;
  };

  struct for_context_t
  {
    size_t scope_id;
    size_t level;
    size_t for_line;
    std::string control_var, bound1, bound2, direction;
  };

  struct ufnc_context_t
  {
    size_t scope_id;
    size_t fnc_line;
    size_t level;
    std::string fnc_name, fnc_param;

    void assign(const ufnc_context_t &rhs)
    {
      if (&rhs == this)
        return;

      scope_id  = rhs.scope_id;
      level     = rhs.level;
      fnc_line  = rhs.fnc_line;
      fnc_name  = rhs.fnc_name;
      fnc_param = rhs.fnc_param;
    }

    ufnc_context_t()
    {
      scope_id = fnc_line = level = 0;
    }

    ufnc_context_t(const ufnc_context_t &rhs)
    {
      assign(rhs);
    }

    void operator=(const ufnc_context_t &rhs)
    {
      assign(rhs);
    }
  };

  typedef std::list<scope_context_t>        scope_context_list_t;
  typedef std::list<if_context_t>           if_context_list_t;
  typedef std::list<ufnc_context_t>         ufnc_context_list_t;
  typedef std::list<for_context_t>          for_context_list_t;

  typedef std::stack<if_context_t>          if_context_stack_t;
  typedef std::stack<ufnc_context_t>        ufnc_context_stack_t;
  typedef std::stack<scope_context_t>       scope_context_stack_t;
  typedef std::stack<for_context_t>         for_context_stack_t;

  typedef std::map<size_t, scope_context_t> scope_context_map_t;
  typedef std::multimap<size_t, brkcnt_context_t> brkcnt_context_map_t;

  struct scope_preparse_t
  {
    int start_or_failing_line;
    ufnc_context_list_t commands;
    scope_context_map_t scopes;
  };
}

#ifndef _COMPEL_DEBUG_SCOPE

class fnc_scopes : public function_t
{
  friend class fnc_usercommand;

public:
  enum sc_operation
  { 
    sc_begin,
    sc_end,
    sc_break,
    sc_continue,
    sc_command,
    sc_if,
    sc_else,
    sc_return,
    sc_for,
    sc_while,
    sc_dowhile
  };
private:
  sc_operation _op;

  parse_errors_e execute_begin();
  parse_errors_e execute_end();
  parse_errors_e execute_break();
  parse_errors_e execute_continue();
  parse_errors_e execute_command();
  parse_errors_e execute_if();
  parse_errors_e execute_else();
  parse_errors_e execute_return();
  parse_errors_e execute_for();
  parse_errors_e execute_while();
  parse_errors_e execute_dowhile();

  static compel_fnc_scopes::scope_context_map_t &_scopes_map;
  static compel_fnc_scopes::scope_preparse_t _preparse;

  static compel_fnc_scopes::scope_context_t *fnc_scopes::find_scope(size_t);

  fnc_scopes(const fnc_scopes &rhs);
  void operator=(const fnc_scopes &rhs);
public:
  parse_errors_e execute();
  parse_errors_e prepare(size_t passno, int &start_or_failing_line);
  fnc_scopes(interpreter_t *, sc_operation op);
  fnc_scopes();
  ~fnc_scopes();
  parse_errors_e register_function(interpreter_t *);

  static const char *fnc_name;
};

#endif

#endif