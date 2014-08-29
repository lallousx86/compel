#include "fnc_branching.h"
#include "../interpreter_helper.h"

static const char *s_cmd_label   = "label";
static const char *s_cmd_if_gt   = "if_gt";
static const char *s_cmd_if_gte  = "if_gte";
static const char *s_cmd_if_lt   = "if_lt";
static const char *s_cmd_if_lte  = "if_lte";
static const char *s_cmd_if_eq   = "if_eq";
static const char *s_cmd_if_neq  = "if_neq";

static const char *s_bin_operators[] = 
{
  "==",  // bc_eq
  "!=",  // bc_neq
  ">",   // bc_gt
  "<",   // bc_lt
  ">=",  // bc_gte
  "<="   // bc_lte
};

// ==================================================================================
//
//                             LABEL
//
// ==================================================================================

const char *fnc_unconditional_branching::fnc_name = "unconditional_branching";

fnc_unconditional_branching::fnc_unconditional_branching()
{

}

parse_errors_e fnc_unconditional_branching::register_function(interpreter_t *interpreter)
{
  fnc_unconditional_branching *fnc_label = new fnc_unconditional_branching(interpreter, ub_label);

  fnc_unconditional_branching *functions[] =
  {
    new fnc_unconditional_branching(interpreter, ub_end),
    new fnc_unconditional_branching(interpreter, ub_goto),
    new fnc_unconditional_branching(interpreter, ub_gotoline),
    fnc_label
  };

  fnc_label->set_flags(fnc_label->get_flags() | function_flag_pass3);

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
    interpreter->add_symbol(functions[i]->_name.c_str(), functions[i]);

  return parse_error_none;
}

fnc_unconditional_branching::fnc_unconditional_branching(interpreter_t *interpreter, ub_operation op)
{
  _op = op;

  switch (op)
  {
  case ub_goto:  
    set_namedesc("goto", "branches to an already defined label");
    set_minmaxargs(1, 1);
    break;
  case ub_gotoline:  
    set_namedesc("gotoline", "branches to source code line");
    set_minmaxargs(1, 1);
    break;
  case ub_end:
    set_namedesc("end", "Ends the execution of the program");
    set_minmaxargs(0, 0);
    break;
  case ub_label:
    set_namedesc(s_cmd_label, "Creates a label at the given line");
    set_minmaxargs(1, 2);
    break;
  }

  set_interpreter(interpreter);
}

parse_errors_e fnc_unconditional_branching::prepare(size_t passno, int &start_or_failing_line)
{
  if (passno != cc_nbpasses)
    return parse_error_none;

  interpreter_t *_int = get_interpreter();

  interpreter_helper_t inthlp(_int);

  lines_container_type_t &lines = inthlp.get_interpreter_lines().get_string_list();

  compel_string_tokenizer_t slp;

	lines_container_type_t::iterator it = lines.begin();
  size_t lineno = start_or_failing_line;

	std::advance(it, lineno);

	for (;
      it != lines.end();++it, lineno++)
  {
    std::string &line = *it;
    size_t pcount = slp.parse(line.c_str());
    if (pcount <= 1)
      continue;

    if (stricmp(slp.get_string(0), s_cmd_label) != 0)
      continue;

    parse_errors_e err = _int->interpreter_line_at(lineno);

    if (err != parse_error_none)
    {
      start_or_failing_line = (int) lineno;
      return err;
    }
  }

  return parse_error_none;
}

parse_errors_e fnc_unconditional_branching::execute_label()
{
  interpreter_t *_int = get_interpreter();

  const char *labelname = _int->get_const_at(1);

  if (!_int->is_variable_name(labelname))
    return parse_error_variable_expected;

  size_t curlineno = _int->get_cur_source_lineno();

  if (_int->is_deferred_line())
  {
    int deferredline = _int->find_deferred_line();
    if (deferredline != -1)
    {
      parse_errors_e err = _int->interpreter_line_at(deferredline);
      _int->void_line(curlineno);
      return err;
    }
  }

  // check if label already defined
  // set mark label starting at next line
  // ;! beware: setting a label at the end of file!
  // ;! beware: if someone branches to a location above the label definition
  value_t *newval = new value_t((long)curlineno+1);
  symbol_t *sym = _int->add_symbol(labelname, newval);
  if (sym == 0)
  {
    delete newval;
    return parse_error_symbol_redefinition;
  }
  _int->void_line(curlineno);
  return parse_error_none;
}

parse_errors_e fnc_unconditional_branching::execute_gotoline()
{
  static const int PAR_LINENO = 1;

  interpreter_t *_int = get_interpreter();

  long lineno = compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_LINENO));
  
  if (lineno > 0)
    lineno--;
  else
    return parse_stop_parsing;

  _int->set_cur_src_line(lineno);

  return parse_branch_to;
}

parse_errors_e fnc_unconditional_branching::execute_goto()
{
  interpreter_t *_int = get_interpreter();

  const char *labelname = _int->get_const_at(1);

  if (!_int->is_variable_name(labelname))
    return parse_error_variable_expected;

  // check if label already defined
  // if not so, then error!
  value_t *val = _int->get_value_at(1);
  if (val == 0)
    return parse_error_symbol_not_in_table;

  _int->set_cur_src_line(val->get_int_value());

  return parse_branch_to;
}

parse_errors_e fnc_unconditional_branching::execute_end()
{
  return parse_stop_parsing;
}

parse_errors_e fnc_unconditional_branching::execute()
{
  switch (_op)
  {
  case ub_label:
    return execute_label();
  case ub_goto:
    return execute_goto();
  case ub_end:
    return execute_end();
  case ub_gotoline:
    return execute_gotoline();
  }
  return parse_error_function_expected;
}

// ==================================================================================
//
//                             IF_EQUALITY (if_eq, if_neq, if_gt, if_lt)
//
// ==================================================================================

const char *fnc_binary_comparison::fnc_name = "binary_comparison";

parse_errors_e fnc_binary_comparison::register_function(interpreter_t *interpreter)
{
  fnc_binary_comparison *functions[] =
  {
    new fnc_binary_comparison(interpreter, fnc_binary_comparison::bc_lte),
    new fnc_binary_comparison(interpreter, fnc_binary_comparison::bc_lt),
    new fnc_binary_comparison(interpreter, fnc_binary_comparison::bc_gt),
    new fnc_binary_comparison(interpreter, fnc_binary_comparison::bc_eq),
    new fnc_binary_comparison(interpreter, fnc_binary_comparison::bc_neq),
    new fnc_binary_comparison(interpreter, fnc_binary_comparison::bc_gte)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
    interpreter->add_symbol(functions[i]->_name.c_str(), functions[i]);

  return parse_error_none;
}

fnc_binary_comparison::fnc_binary_comparison()
{

}

fnc_binary_comparison::fnc_binary_comparison(interpreter_t *interpreter, binary_comparison bc)
{
  // save comparison operator
  _op = bc;

  switch (bc)
  {
  case bc_gt:
    set_namedesc(s_cmd_if_gt, "greater than: branches to an already defined label");
    break;
  case bc_gte:
    set_namedesc(s_cmd_if_gte, "greater than or equal: branches to an already defined label");
    break;
  case bc_lte:
    set_namedesc(s_cmd_if_lte, "less than or equal: branches to an already defined label");
    break;
  case bc_lt:
    set_namedesc(s_cmd_if_lt, "less than: branches to an already defined label");
    break;
  case bc_eq:
    set_namedesc(s_cmd_if_eq, "inequality: branches to an already defined label");
    break;
  case bc_neq:
    set_namedesc(s_cmd_if_neq, "equality: branches to an already defined label");
    break;
  }

  set_minmaxargs(3, 0);
  set_interpreter(interpreter);
}

bool fnc_binary_comparison::do_comparison(
    interpreter_t *_int, 
    int op, 
    int at_opr1, 
    int at_opr2)
{
  bool bBranch = false;

  const char *opr1 = _int->evaluate_at(at_opr1);
  const char *opr2 = _int->evaluate_at(at_opr2);

  long opr1n = compel_string_tokenizer_t::parse_number(opr1), 
    opr2n = compel_string_tokenizer_t::parse_number(opr2);

  switch (op)
  {
  case bc_eq:
  case bc_neq:
    {
      // Truth table for EQ and NEQ
      // E = _bEquality (when EQ or NEQ)
      // Test = if they are equal
      // Result -> the outcome we want (whether to branch or not)
      // E   TEST   RESULT
      // --- -----  ------
      // T   T      T
      // T   F      F
      // F   T      F
      // F   F      T
      //
      // Thus the result is to branch if test==equality
      bool bEqual = strcmp(opr1, opr2) == 0;
      bool bEquality = (op == bc_eq);
      bBranch = bEqual == bEquality;
      break;
    }
  case bc_gt:
    bBranch = opr1n > opr2n;
    break;
  case bc_gte:
    bBranch = opr1n >= opr2n;
    break;
  case bc_lt:
    bBranch = opr1n < opr2n;
    break;
  case bc_lte:
    bBranch = opr1n <= opr2n;
    break;
  }
  return bBranch;
}

int fnc_binary_comparison::oprstr_to_opr(const char *oprstr)
{
  for (size_t i=0;i<sizeof(s_bin_operators)/sizeof(s_bin_operators[0]);i++)
  {
    if (stricmp(s_bin_operators[i], oprstr) == 0)
      return (int) i;
  }
  return -1;
}

parse_errors_e fnc_binary_comparison::execute()
{
  static const int PAR_OPR1 = 1;
  static const int PAR_OPR2 = 2;
  static const int PAR_ACTION = 3;

  interpreter_t *_int = get_interpreter();

  const char *symname = _int->get_const_at(PAR_ACTION);

  bool bBranch;

  bBranch = do_comparison(_int, _op, PAR_OPR1, PAR_OPR2);

  if (!bBranch)
    return parse_error_none;

  // Label?
  if (_int->is_variable_name(symname))
  {
    // check if label already defined, if not so then error!
    value_t *val = _int->get_value_at(PAR_ACTION);
    if (val == 0)
      return parse_error_symbol_not_in_table;

    if (bBranch)
    {
      // branch!
      _int->set_cur_src_line(val->get_int_value());
      return parse_branch_to;
    }
  }

  if (function_t *fnc = _int->get_function(symname))
  {
    // we need a helper
    interpreter_helper_t helper(_int);

    // save old tokenizer
    compel_string_tokenizer_t *old_tokenizer = helper.get_tokenizer();

    // create new tokenizer for the action passed to the if_xx
    compel_string_tokenizer_t new_tokenizer;

    // get the action
    const char *action_str = old_tokenizer->join_from(PAR_ACTION);

    // parse the action
    new_tokenizer.parse(action_str);

    // set new tokenizer (temp)
    helper.set_tokenizer(&new_tokenizer);

    // execute the function
    parse_errors_e err = fnc->execute();

    // restore old tokenizer
    helper.set_tokenizer(old_tokenizer);

    return err;
  }
  else
    return parse_error_function_expected;

  // do not branch, execute next line
  return parse_error_none;
}
