#pragma once

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

// ===================================================
class fnc_unconditional_branching : public function_t
{
public:
  enum ub_operation
  { 
    ub_goto,
    ub_gotoline,
    ub_end,
    ub_label
  };
private:
  ub_operation _op;

  parse_errors_e execute_goto();
  parse_errors_e execute_label();
  parse_errors_e execute_end();
  parse_errors_e execute_gotoline();

public:
  parse_errors_e execute();
  parse_errors_e prepare(size_t passno, int &start_or_failing_line);
  fnc_unconditional_branching(interpreter_t *, ub_operation op);
  fnc_unconditional_branching();

  parse_errors_e register_function(interpreter_t *);

  static const char *fnc_name;
};

// ===================================================
class fnc_binary_comparison : public function_t
{
public:
  enum binary_comparison 
  { 
    bc_eq = 0, 
    bc_neq, 
    bc_gt,
    bc_lt,
    bc_gte,
    bc_lte
  };
private:
  binary_comparison _op;

public:

  static bool do_comparison(interpreter_t *interpreter, int op, int at_opr1, int at_opr2);
  static int oprstr_to_opr(const char *oprstr);

  fnc_binary_comparison(interpreter_t *, binary_comparison);

  parse_errors_e execute();
  fnc_binary_comparison();
  parse_errors_e register_function(interpreter_t *);

  static const char *fnc_name;
};