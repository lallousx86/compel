#include "interpreter.h"

static const char *s_str_deferred = "deferred";
static const char ch_variable_prefix = '$';
static const char *str_object_separator = ".";

bool interpreter_errors_sorting_functor::operator()(const interpreter_errors_i *lhs, const interpreter_errors_i *rhs) const
{
  if (lhs == rhs)
    return false;

  return !(*lhs < *rhs);
}

void interpreter_t::set_msg_warning(const char *msg)
{
  _msg_warning = msg;
}

void interpreter_t::set_msg_error(const char *msg)
{
  _msg_error = msg;
}

const char *interpreter_t::get_msg_error()
{
  return _msg_error.c_str();
}

const char *interpreter_t::get_msg_warning()
{
  return _msg_warning.c_str();
}


//
//
//
interpreter_t::interpreter_t()
{
  _cursourceline = 0;
  _slp = new compel_string_tokenizer_t;
  _cur_function = 0;
  _int_bind.tid = ::GetCurrentThreadId();
  _int_bind.int_bind = this;
}

interpreter_t::~interpreter_t()
{
  delete _slp;
}

symbol_t *interpreter_t::find_symbol(const char *symname)
{
  return _symtbl->find_symbol(symname);
}

symbol_t *interpreter_t::add_symbol(const char *symname, symbol_t *sym)
{
  return _symtbl->add_symbol(symname, sym);
}

bool interpreter_t::remove_symbol(const char *symname)
{
  return _symtbl->remove_symbol(symname);
}

// Looks in the symbol table for a given variable
variable_t *interpreter_t::get_variable_raw(const char *varname)
{
//  _err = parse_error_none;
  symbol_t *sym = find_symbol(varname);
  if (sym == 0)
  {
//    _err = parse_error_symbol_not_in_table;
    return 0;
  }
  else if (sym->get_sym_kind() != symbol_variable)
  {
//    _err = parse_error_symbol_type_mismatch;
    return 0;
  }
  return static_cast<variable_t *>(sym);
}

bool interpreter_t::is_deferred_line(compel_string_tokenizer_t *slp)
{
  if (slp == 0)
    slp = _slp;

  return (stricmp(slp->get_string(slp->parsed_count() - 1), s_str_deferred) == 0);
}

int interpreter_t::find_deferred_line(size_t line_to_find)
{
  compel_string_tokenizer_t search_line;
  search_line.parse((char *)_lines.getline(line_to_find));

  if (search_line.parsed_count() == 0)
    return -1;

  return find_deferred_line(&search_line);
}

int interpreter_t::find_deferred_line(compel_string_tokenizer_t *search_line)
{
  if (search_line == 0)
    search_line = _slp;

  size_t nsearchlinetokens = search_line->parsed_count();

  if (nsearchlinetokens == 0)
    return -1;

  if (!is_deferred_line(search_line))
    return -1;

  size_t nlines = _lines.count();

  compel_string_tokenizer_t cur_line;

  for (size_t nline=0;nline<nlines;nline++)
  {
    cur_line.parse((char *)_lines.getline(nline));

    size_t ncurlinetokens = cur_line.parsed_count();

    // do they have same arg count, but with the "deferred" token
    if (ncurlinetokens != nsearchlinetokens - 1)
      continue;

    // is this the same line? token by token?
    size_t i;
    for (i=0;i<ncurlinetokens;i++)
    {
      if (stricmp(cur_line.get_string(i), search_line->get_string(i)) != 0)
        break;
    }

    if (i == ncurlinetokens)
      return (int) nline;
  }
  return -1;
}

//
// checks whether the given line is a void line (has no effect any more)
// void lines are lines as definition lines, labels ...
//
bool interpreter_t::is_void_line(compel_string_tokenizer_t *slp)
{
  // no string passed, use currently parsed one
  if (slp == 0)
    slp = _slp;

  const char *s = _slp->get_string(0);
  return (*s == '@');
}

//
// marks a given line as void
//
void interpreter_t::void_line(size_t lineno)
{
  std::string &line = _lines.line(lineno);
  line = "@ " + line;
}

//
// Returns the current source line
//
size_t interpreter_t::get_cur_source_lineno() const
{
  return _cursourceline;;
}

const char *interpreter_t::get_cur_source_line_str()
{
  return get_source_line_str(_cursourceline);
}

bool interpreter_t::set_src_line_str(size_t lineno, const char *value)
{
  if (lineno >= _lines.count())
    return false;
  _lines.line(lineno) = value;
  return true;
}

const char *interpreter_t::get_source_line_str(size_t lineno)
{
  if (lineno >= _lines.count())
    return 0;
  return _lines.line(lineno).c_str();
}

void interpreter_t::set_cur_src_line(size_t lineno)
{
  _cursourceline = lineno;
}

char interpreter_t::get_object_separator()
{
  return str_object_separator[0];
}

char interpreter_t::get_variable_prefix()
{
  return ch_variable_prefix;
}

//
// is this variable name a string?
//
bool interpreter_t::is_variable_name(const char *varname)
{
  return *varname == ch_variable_prefix;
}

// ;! returns the number of arguments
// it is actually the number of parsed elements in the given line
// so if you expect 3 parameters then you must check for 4
size_t interpreter_t::get_fnc_arg_count() const
{
  return _slp->parsed_count();
}

//
// returns the string pointed by nAt
//
const char *interpreter_t::get_const_at(size_t nAt, compel_string_tokenizer_t *slp)
{
  if (slp == 0)
    slp = _slp;
  return slp->get_string(nAt);
}

// This will try to retrieve the variable's value first
// if not a variable then the string as-is is returned
const char *interpreter_t::evaluate_at(
  size_t nAt, 
  compel_string_tokenizer_t *slp)
{
  if (slp == 0)
    slp = _slp;

  const char *s = slp->get_string(nAt);

  // Not a variable?
  if (!is_variable_name(s))
    return s;

  std::string out;
  variable_t *var = get_var_ref(s, out);

  // no variable
  if (var == 0)
    return s;

  // Is this a value?
  if (var->get_var_kind() == variable_value)
  {
    return static_cast<value_t *>(var)->get_str_value();
  }
  else if (var->get_var_kind() == variable_object)
  {
    static_cast<object_t *>(var)->to_string(_temp_str);
    return _temp_str.c_str();
  }

  return "";
}

// 
// RET = 0 -> out = varname
// RET != 0 -> RET is OBJ -> out = obj attr name
// RET != 0 -> RET is VAL -> out = val value
variable_t *interpreter_t::get_var_ref(
  const char *expr, 
  std::string &out)
{
  static const char ch_var = ch_variable_prefix;
  static const char *ch_sep = str_object_separator;
  compel_string_tokenizer_t tok;

  size_t pcnt = tok.parse(expr, ch_sep);
  size_t i = pcnt;
  variable_t *var_ref = 0;
  do 
  {
    i--;
    const char *var_i = tok.get_string(i);

    if (var_i == 0)
      break;

    while (var_i[0] == ch_var)
    {
      // an empty "$" symbol
      if (var_i[1] == ch_var)
        break;

      const char *inner_var_i = strrchr(var_i, ch_var);
      symbol_t *sym = _symtbl->find_symbol(inner_var_i);
      if (sym == 0 || sym->get_sym_kind() != symbol_variable)
        break;

      variable_t *var = static_cast<variable_t *>(sym);
      variable_kind_e var_kind = var->get_var_kind();

      var_ref = var;

      if (var_kind == variable_value)
      {
        value_t *val = static_cast<value_t *>(var);
        const char *val_str = val->get_str_value();
        std::string new_var = std::string(var_i).substr(0, inner_var_i - var_i);
        new_var += val_str;
        var_i = tok.set_string(i, new_var.c_str());
        continue;
      }
      else if (var_kind == variable_object)
      {
        object_t *obj = static_cast<object_t *>(sym);

        if (i+1 >= pcnt)
          break;

        value_t *attr_val = obj->find_attribute(tok.get_string(i+1));
        if (attr_val == 0)
        {
          break;
        }

        tok.set_string(i, attr_val->get_str_value());
        var_ref = attr_val;
        pcnt--;
        i = pcnt;
        break;
      }
      else
      {
        break;
      }
    }
  } while (i != 0);

  if (var_ref && var_ref->get_var_kind() == variable_object)
  {
    const char *t = tok.get_string(1);
    if (t == 0)
      t = tok.get_string(0);
    out = t ? t : expr;
  }
  else
  {
    out = tok.join_from_to(0, pcnt, ch_sep);
  }
  return var_ref;
}

//
// Given a variable name it returns a variable_t or 0 if not found
//
variable_t *interpreter_t::get_variable(const char *name)
{
  variable_t *var;
  std::string out;

  // all variables must be preceeded by '$'
  if (!is_variable_name(name) || ((var = get_var_ref(name, out)) == 0))
    return 0;

  return var;
}

//
// Returns a variable at the given parameter index
// it fails if the index does not point to a valid variable
//
variable_t *interpreter_t::get_variable_at(size_t nAt, compel_string_tokenizer_t *slp)
{
  if (slp == 0)
    slp = _slp;

  // get variable name
  const char *s = _slp->get_string(nAt);
  return get_variable(s);
}

//
// Fetchs an object from the symbol table
//
object_t *interpreter_t::get_object(const char *objname)
{
  variable_t *var = get_variable(objname);
  if (var == 0 || var->get_var_kind() != variable_object)
    return 0;
  return static_cast<object_t *>(var);
}

//
// returns an object in a given argument index
// even if the argument value was: $obj1.xyz
// it will trim the .xyz and return a reference to $obj1
//
object_t *interpreter_t::get_object_at(size_t nAt, compel_string_tokenizer_t *slp)
{
  if (slp == 0)
    slp = _slp;

  variable_t *var = get_variable_at(nAt, slp);
  if (var == 0 || (var->get_var_kind() != variable_object))
    return 0;

  return static_cast<object_t *>(var);
}

//
//
//
value_t *interpreter_t::get_value(const char *name)
{
  // do we have a variable in the first place?
  variable_t *var = get_variable(name);
  if (var == 0 || (var->get_var_kind() != variable_value))
    return 0;

  return static_cast<value_t *>(var);
}


//
// Returns a value_t at the given parameter
// If the parameter designates a valid object, it returns the value of its attribute
// It returns 0 if no value can be parsed
//
value_t *interpreter_t::get_value_at(size_t nAt, compel_string_tokenizer_t *slp)
{
  if (slp == 0)
    slp = _slp;

  return get_value(_slp->get_string(nAt));
}

void interpreter_t::set_symbol_table(symbol_table_t *symtbl)
{
  _symtbl = symtbl;
}

const function_t *interpreter_t::get_cur_function() const
{
  return _cur_function;
}

bool interpreter_t::load_lines_from_file(
	const char *filename, 
	bool b_clear_old_lines)
{
  if (!_lines.read_from_file(filename, b_clear_old_lines))
    return false;

  return true;
}

//
//
//
parse_errors_e interpreter_t::interpret_new_line(const char *str)
{
  if (str == 0)
    str = "";
  _lines.add(str);

  set_cur_src_line(_lines.count()-1);

  return interpret_line();
}

//
// Interprets a line at a given lineno
// (it simply changes cursrcline temporarily then restores it)
// This function is used to execute deferred statements
//
parse_errors_e interpreter_t::interpreter_line_at(size_t lineno)
{
  size_t curlineno = get_cur_source_lineno();
  set_cur_src_line(lineno);
  parse_errors_e err = interpret_line();
  set_cur_src_line(curlineno);
  return err;
}

// Looks in the symbol table for a given function
function_t *interpreter_t::get_function(const char *fncname)
{
  symbol_t *sym = find_symbol(fncname);
  if (sym == 0)
  {
//    _err = parse_error_symbol_not_in_table;
    return 0;
  }
  else if (sym->get_sym_kind() != symbol_function)
  {
//    _err = parse_error_symbol_type_mismatch;
    return 0;
  }
  return static_cast<function_t *>(sym);
}

//
// parses and interprets a given line
//
parse_errors_e interpreter_t::interpret_line()
{
  const char *str;

  // get the current source line
  size_t curline = get_cur_source_lineno();

  if (curline < _lines.count())
    str = _lines.getline(curline);
  // if curline is greater than lines count then we must stop execution!
  else
//  if (curline >= _lines.count())
    return parse_stop_parsing;

  // parse the string, check if empty
  if (_slp->parse((char *)str) == 0)
  {
    set_cur_src_line(curline+1); // advance
    return parse_line_is_empty;
  }
  // is it a void line?
  else if (is_void_line())
  {
    set_cur_src_line(curline+1);
    return parse_error_line_is_void;
  }

  // did you find the function?
  const char *fnc_name = _slp->get_string(0);

  _cur_function = get_function(fnc_name);

  if (_cur_function == 0)
  {
    // look for error handler(s)
    _last_err = on_parse_error(curline, parse_error_function_expected);

    if (_last_err == parse_error_wrong_syntax)
    {
      set_cur_src_line(curline+1); // advance
      return _last_err;
    }
    else if (_last_err == parse_stop_parsing)
    {
      return parse_stop_parsing;
    }
    else if (_last_err == parse_error_none)
    {
      // user doesn't want to execute anything else
      if (_cur_function == 0)
      {
        set_cur_src_line(curline+1); // advance
        return parse_error_none;
      }
    }
    else
    {
      // 
      return _last_err;
    }
  }

  // Get argument restrictions
  size_t 
    nargs = _slp->parsed_count() - 1, // minus one because first param is function
    minargs = _cur_function->get_minargs(), 
    maxargs = _cur_function->get_maxargs();

  // Check function min/max parameters
  if (minargs != 0 && nargs < minargs)
    return parse_error_less_param;
  else if (maxargs != 0  && nargs > maxargs)
    return parse_error_more_param;

  // always make sure that the current function executes in the context
  // of the current interpreter
  interpreter_t *fnc_int = _cur_function->get_interpreter();
  if (fnc_int != this)
    _cur_function->set_interpreter(this);
  else
    fnc_int = 0;

  parse_errors_e err = _cur_function->execute();

  // restore old interpreter
  if (fnc_int != 0)
    _cur_function->set_interpreter(fnc_int);

  // if it was not a branching instruction, 
  // then we need to advance the source lines
  if (err == parse_branch_to)
  {
    err = parse_error_none;
  }
  else
  {
    set_cur_src_line(curline+1); // advance
  }
  return err;
}

parse_errors_e interpreter_t::on_parse_error(size_t lineno, parse_errors_e err)
{
  for (interpreter_errors_clients_t::iterator it = _error_clients.begin();
    it != _error_clients.end();
    ++it)
  {
    parse_errors_e ret_err = (*it)->ie_on_parse_error(lineno, err);
    if (ret_err == parse_error_handler_continue_search)
      continue;
    return ret_err;
  }
  return parse_stop_parsing;
}

parse_errors_e interpreter_t::prepare(int &starting_or_failing_line)
{
  parse_errors_e err;
  strsymbol_map_t &table = _symtbl->get_symtbl_raw();

  strsymbol_map_t::iterator it;

  for (size_t passno=2;passno<= cc_nbpasses;passno++)
  {
    // for each function
    for (it = table.begin(); it != table.end(); ++it)
    {
      if (it->second->get_sym_kind() != symbol_function)
        continue;

      function_t *curfnc = static_cast<function_t *>(it->second);

      int fflags = curfnc->get_flags();
      if (((fflags & function_flag_pass2) == function_flag_pass2) && (passno == 2))
      {
        int failing_line = starting_or_failing_line;
        err = curfnc->prepare(passno, failing_line);
        if (err != parse_error_none)
        {
          starting_or_failing_line = failing_line;
          return err;
        }
      }
      if (((fflags & function_flag_pass3) == function_flag_pass3) && (passno == 3))
      {
        int failing_line = starting_or_failing_line;
        err = curfnc->prepare(passno, failing_line);
        if (err != parse_error_none)
        {
          starting_or_failing_line = failing_line;
          return err;
        }
      }
    }
  }
  return parse_error_none;
}

bool interpreter_t::error_client_register(interpreter_errors_i *client)
{
  return _error_clients.insert(client).second;
}

bool interpreter_t::error_client_unregister(interpreter_errors_i *client)
{
  return _error_clients.erase(client) > 0;
}

bool interpreter_t::error_client_get_priorities(int &highest, int &lowest)
{
  if (_error_clients.empty())
    return false;

  highest = (*_error_clients.begin())->ie_get_priority();
  lowest  = (*_error_clients.rbegin())->ie_get_priority();

  return true;
}

bool interpreter_t::load_from_lines(lines_t &lines, bool b_clear_old_lines)
{
	// set all lines (while clearing old ones)
	if (b_clear_old_lines)
     _lines = lines;
	else
 	_lines.merge_lines(lines, _lines.count());

  return true;
}

void interpreter_t::clear_lines()
{
  _lines.clear();
  set_cur_src_line(0);
}

// Returns the count of source lines
size_t interpreter_t::get_source_lines_count()
{
	return _lines.count();
}