#include "fnc_arith.h"
#include "../parse_util.h"
#include "../lib/ExpressionEvaluator.h"
#include "../function_helper.h"

const static char *s_ns_name = "math.";

const char *s_cmd_add  = "add";
const char *s_cmd_sub  = "sub";
const char *s_cmd_mul  = "mul";
const char *s_cmd_div  = "div";
const char *s_cmd_expr = "expr";

static const char 
 *s_str_arith_help = 
    "Substracts a value from a variable\n"
    "syntax: (ADD|SUB|MUL|DIV|EXPR) $var1 var2\n"
    "On return $var1 will contain the result\n",
 *s_str_expr_help = "Executes a complex mathematical expression\n";

fnc_binary_arith::fnc_binary_arith(bool bUseNameSpace)
{
  _bUseNameSpace = bUseNameSpace;
}

const char *fnc_binary_arith::fnc_name = "binary_arith";

parse_errors_e fnc_binary_arith::register_function(interpreter_t *interpreter)
{
  fnc_binary_arith *functions[] =
  {
    new fnc_binary_arith(interpreter, fnc_binary_arith::ba_add),
    new fnc_binary_arith(interpreter, fnc_binary_arith::ba_mul),
    new fnc_binary_arith(interpreter, fnc_binary_arith::ba_div),
    new fnc_binary_arith(interpreter, fnc_binary_arith::ba_expr),
    new fnc_binary_arith(interpreter, fnc_binary_arith::ba_sub)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
  {
    std::string fnc_name = functions[i]->_name;
    if (_bUseNameSpace)
      fnc_name = s_ns_name + fnc_name;
    interpreter->add_symbol(fnc_name.c_str(), functions[i]);
  }

  return parse_error_none;
}

fnc_binary_arith::fnc_binary_arith(interpreter_t *interpreter, binary_arith op)
{
  // save the operation code
  _op = op;

  set_interpreter(interpreter);
  switch (op)
  {
  case ba_sub:
    set_namedesc(s_cmd_sub, s_str_arith_help);
    break;
  case ba_mul:
    set_namedesc(s_cmd_mul, s_str_arith_help);
    break;
  case ba_div:
    set_namedesc(s_cmd_div, s_str_arith_help);
    break;
  case ba_add:
    set_namedesc(s_cmd_add, s_str_arith_help);
    break;
  case ba_expr:
    set_namedesc(s_cmd_expr, s_str_expr_help);
    set_minmaxargs(2, 0);
    return;
  }
  set_minmaxargs(2, 2);
}

parse_errors_e fnc_binary_arith::execute_expr()
{
  interpreter_t *_int = get_interpreter();

  value_t *var_dest = _int->get_value_at(1);

  if (var_dest == 0)
    return parse_error_symbol_not_in_table;

  long r;
  std::string expr;
  function_helper::eval_from_to(_int, 2, _int->get_fnc_arg_count(), expr, false, 0);

  if (ExpressionEvaluator::calculateLong(expr, r) != ExpressionEvaluator::eval_ok)
    return parse_error_wrong_syntax;

  var_dest->set_int_value(r);

  return parse_error_none;
}

parse_errors_e fnc_binary_arith::execute()
{
  if (_op == ba_expr)
    return execute_expr();

  interpreter_t *_int = get_interpreter();

  value_t *var_dest = _int->get_value_at(1);
  if (var_dest == 0)
    return parse_error_symbol_not_in_table;

  long n_src = compel_string_tokenizer_t::parse_number(_int->evaluate_at(2));
  long n_dest = var_dest->get_int_value();

  long result;

  switch (_op)
  {
  case ba_sub:
    result = n_dest - n_src;
    break;
  case ba_mul:
    result = n_dest * n_src;
    break;
  case ba_div:
    if (n_dest == 0)
      return parse_error_divide_by_zero_detected;
    result = n_src / n_dest;
  case ba_add:
    result = n_src + n_dest;
    break;
  }

  var_dest->set_int_value(result);

  return parse_error_none;
}