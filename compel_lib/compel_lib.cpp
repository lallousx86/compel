#include "compel_lib.h"
#include "compel_lib_script.h"
#include "fnc_libusercommand.h"

#include <stdarg.h>
#include <windows.h>
#include <map>
#include <sstream>

#define GET_COMPEL_LIB_SCRIPT      compel_lib_script_t *script = reinterpret_cast<compel_lib_script_t *>(compel_script)
#define GET_COMPEL_TOKENIZER       compel_string_tokenizer_t *tok = reinterpret_cast<compel_string_tokenizer_t *>(compel_tokenizer)
#define GET_COMPEL_VALUE           value_t *value = reinterpret_cast<value_t *>(compel_value)
#define GET_COMPEL_OBJECT          object_t *object = reinterpret_cast<object_t *>(compel_object)
#define TO_COMPEL_VALUE(x)         reinterpret_cast<compel_value_t>(x)

typedef std::map<int, int> int_int_map_t;

static int_int_map_t err_engine_to_lib, err_lib_to_engine;
static std::map<int, std::string> err_lib_str;

void errors_map_init()
{
  err_engine_to_lib.clear();
  err_lib_to_engine.clear();

  err_engine_to_lib[parse_error_more_param] =
    err_engine_to_lib[parse_error_less_param] = compel_error_wrong_param_count;

  err_engine_to_lib[parse_error_line_is_comment] =
    err_engine_to_lib[parse_error_line_is_void] =
    err_engine_to_lib[parse_line_is_empty] = compel_error_no_operation;

  err_engine_to_lib[parse_error_none] =
    err_engine_to_lib[parse_continue_parsing] =
    err_engine_to_lib[parse_error_reparse] = compel_error_success;

  err_engine_to_lib[parse_error_symbol_not_in_table] = compel_error_symbol_undefined;

  err_engine_to_lib[parse_error_function_expected] = compel_error_function_expected;

  err_engine_to_lib[parse_error_symbol_redefinition] = compel_error_symbol_redefined;

  err_engine_to_lib[parse_stop_parsing] = compel_error_script_stopped;

  err_engine_to_lib[parse_error_wrong_syntax] =
  err_engine_to_lib[parse_error_imbalance] =
  err_engine_to_lib[parse_error_wrong_context] = compel_error_syntax_error;

  err_engine_to_lib[parse_error_symbol_type_mismatch] =
  err_engine_to_lib[parse_error_variable_expected] =
  err_engine_to_lib[parse_error_object_expected] = compel_error_symbol_expected;

  err_engine_to_lib[parse_branch_to] = compel_error_branch_handled;

  err_engine_to_lib[parse_error_divide_by_zero_detected] =
  err_engine_to_lib[parse_error_file_not_found] = compel_error_wrong_param_value;

  err_engine_to_lib[parse_error_handler_continue_search] = compel_error_error_handler_continue_search;

  //---
  // inverse - here we have loss of error precision
  // since previously multiple errors mapped to a single error
  // thus we cannot really reverse the mapping
  //---
  err_lib_to_engine[compel_error_wrong_param_count] = parse_error_more_param;
  err_lib_to_engine[compel_error_success] = parse_error_none;
  err_lib_to_engine[compel_error_symbol_undefined] = parse_error_symbol_not_in_table;
  err_lib_to_engine[compel_error_function_expected] = parse_error_function_expected;
  err_lib_to_engine[compel_error_symbol_redefined] = parse_error_symbol_redefinition;
  err_lib_to_engine[compel_error_script_stopped] = parse_stop_parsing;
  err_lib_to_engine[compel_error_syntax_error] = parse_error_wrong_syntax;
  err_lib_to_engine[compel_error_symbol_expected] = parse_error_symbol_type_mismatch;
  err_lib_to_engine[compel_error_branch_handled] = parse_branch_to;
  err_lib_to_engine[compel_error_wrong_param_value] = parse_error_file_not_found;
  err_lib_to_engine[compel_error_error_handler_continue_search] = parse_error_handler_continue_search;
  err_lib_to_engine[compel_error_invalid_extension] = parse_error_wrong_syntax;
  err_lib_to_engine[compel_error_no_operation] = parse_error_line_is_void;

  // Error strings
  err_lib_str[compel_error_success] = "success";
  err_lib_str[compel_error_symbol_undefined] = "undefined symbol or symbol not in table";
  err_lib_str[compel_error_syntax_error] = "syntax error";
  err_lib_str[compel_error_error_handler_continue_search] = "continue error handler search";
  err_lib_str[compel_error_wrong_param_count] = "wrong parameters count";
  err_lib_str[compel_error_function_expected] = "command not found";
  err_lib_str[compel_error_script_stopped] = "script stopped";
  err_lib_str[compel_error_symbol_redefined] = "symbol redefinition";
  err_lib_str[compel_error_unknown_error] = "unknown error occurred";
  err_lib_str[compel_error_branch_handled] = "branching";
  err_lib_str[compel_error_symbol_expected] = "symbol type mismatch";
  err_lib_str[compel_error_wrong_param_value] = "wrong parameter value or type";
  err_lib_str[compel_error_no_operation] = "no operation";
  err_lib_str[compel_error_invalid_extension] = "user extension is invalid";
}

COMPEL_EXPORT int COMPEL_API compel_engine_to_lib_error(int engine_err)
{
  if (err_engine_to_lib.size() == 0)
    errors_map_init();

  int_int_map_t::iterator it = err_engine_to_lib.find(engine_err);
  if (it == err_engine_to_lib.end())
    return compel_error_unknown_error;
  else
    return it->second;
}

COMPEL_EXPORT int COMPEL_API compel_lib_to_engine_error(int lib_err)
{
  if (err_engine_to_lib.size() == 0)
    errors_map_init();

  int_int_map_t::iterator it = err_lib_to_engine.find(lib_err);
  if (it == err_lib_to_engine.end())
    return parse_error_wrong_syntax;
  else
    return it->second;
}

static void _dbg_out(char *fmt, ...)
{
  va_list va;

  va_start(va, fmt);

  char buf[4096];
  vsprintf(buf, fmt, va);
  OutputDebugString(buf);
  va_end(va);
}

// Converts an error code to error string
COMPEL_EXPORT const char *COMPEL_API compel_error_code_to_string(int lib_err)
{
	std::map<int, std::string>::iterator it = err_lib_str.find(lib_err);
	if (it == err_lib_str.end())
		return "";
	else
		return it->second.c_str();
}

// allocates a string
COMPEL_EXPORT char * COMPEL_API compel_string_create(const char *str, size_t sz)
{
  if (str != 0)
  {
    size_t len = strlen(str);
    sz = (sz > len ? sz : len) + 1;
  }
  
  char *s = new char[sz];

  if (str != 0)
    strcpy(s, str);

  return s;
}

// de-allocates a string
COMPEL_EXPORT void COMPEL_API compel_string_destroy(char *str)
{
  delete [] str;
}

// creates a tokenizer object
COMPEL_EXPORT compel_tokenizer_t COMPEL_API compel_tokenize_init(const char *str, const char *delim, const char *quote, const char *escape)
{
  compel_string_tokenizer_t *tok = new compel_string_tokenizer_t;

  tok->parse(str, delim, quote, escape, 0);

  return reinterpret_cast<compel_tokenizer_t>(tok);
}

// tokenizes another string using the tok object
COMPEL_EXPORT int COMPEL_API compel_tokenize_parse(
  compel_tokenizer_t compel_tokenizer,
  const char *str, 
  const char *delim, 
  const char *quote, 
  const char *escape)
{
  GET_COMPEL_TOKENIZER;

  return (int) tok->parse(str, delim, quote, escape);
}

// returns a tokenized string by index
COMPEL_EXPORT const char * COMPEL_API compel_tokenize_get(compel_tokenizer_t compel_tokenizer, const size_t idx)
{
  GET_COMPEL_TOKENIZER;
  return tok->get_string(idx);
}

// sets a tokenized string by index
COMPEL_EXPORT const char * COMPEL_API compel_tokenize_set(compel_tokenizer_t compel_tokenizer, const size_t idx, const char *str)
{
  GET_COMPEL_TOKENIZER;
  return tok->set_string(idx, str);
}

// deletes the tokenizer object
COMPEL_EXPORT void COMPEL_API compel_tokenize_free(compel_tokenizer_t compel_tokenizer)
{
  GET_COMPEL_TOKENIZER;
  delete tok;
}

// returns the number of tokenized strings
COMPEL_EXPORT size_t COMPEL_API compel_tokenize_parsed_count(compel_tokenizer_t compel_tokenizer)
{
  GET_COMPEL_TOKENIZER;
  return tok->parsed_count();
}

// parses a number
COMPEL_EXPORT long COMPEL_API compel_parse_number(const char *value)
{
  return compel_string_tokenizer_t::parse_number(value);
}

// returns the list of extensions avail
COMPEL_EXPORT const char * COMPEL_API compel_script_avail_extensions()
{
  return compel_lib_script_t::get_builtin_avail_extensions();
}

// initialize the scripting engine
COMPEL_EXPORT compel_script_t COMPEL_API compel_script_init(p_compel_init_t init)
{
  compel_lib_script_t *script = new compel_lib_script_t;
  compel_script_t compel_script = script->to_compel_script_t();// reinterpret_cast<compel_script_t>(script);

  // arguments passed?
  if (init->script_args != 0)
    script->m_script_args  = init->script_args;

  if (init->extensions != 0)
    script->m_extensions = init->extensions;

  script->m_b_usefullns = init->b_usefullns;
  script->m_b_dbgout_script = init->b_dbgout_script;
  script->m_script_file = init->script_file ? init->script_file : "";
  script->m_compel_script = compel_script;

  if (!script->init())
  {
    delete script;
    return 0;
  }
  return compel_script;
}

// returns the context set by the user
COMPEL_EXPORT compel_user_context_t COMPEL_API compel_script_context_get(
  compel_script_t compel_script, 
  const char *guid)
{
  GET_COMPEL_LIB_SCRIPT;
  return script->get_context(guid);
}

// stores the user context value in the script object
COMPEL_EXPORT void COMPEL_API compel_script_context_set(
  compel_script_t compel_script, 
  const char *guid,
  compel_user_context_t context)
{
  GET_COMPEL_LIB_SCRIPT;
  script->set_context(guid, context);
}

// de-initializes the scripting engine
COMPEL_EXPORT void COMPEL_API compel_script_deinit(compel_script_t compel_script)
{
  GET_COMPEL_LIB_SCRIPT;
  delete script;
}

/*
// ;! returns the last error mapped to compel_errors
int compel_get_last_error(compel_script_t compel_script)
{
  GET_COMPEL_LIB_SCRIPT;
  return parse_errors_to_compel_error(script->get_last_parse_error());
}
*/

// loads a script file into interpreter
COMPEL_EXPORT int COMPEL_API compel_script_load_file(compel_script_t compel_script, const char *filename)
{
  GET_COMPEL_LIB_SCRIPT;

  script->script_load_from_file(filename, false);

  return script->get_last_engine_error();
}

// run the whole script
COMPEL_EXPORT int COMPEL_API compel_script_run(compel_script_t compel_script)
{
  GET_COMPEL_LIB_SCRIPT;

  interpreter_t *interpreter = script->get_interpreter();

  parse_errors_e err, lasterr;

  err = lasterr = parse_error_none;

  // Not paused initially
  ::ResetEvent(script->evt_paused);
  script->b_pause = false;
  script->n_ms_exectime = ::GetTickCount();
  while (err != parse_stop_parsing)
  {
    if (script->b_pause)
      break;

    size_t curline = interpreter->get_cur_source_lineno();

    if (script->m_b_dbgout_script)
      _dbg_out(">line:%d >%s<\n", curline, interpreter->get_cur_source_line_str());

    err = interpreter->interpret_line();
    switch (err)
    {
    case parse_error_line_is_void:
    case parse_error_none:
    case parse_stop_parsing:
    case parse_line_is_empty:
    case parse_error_line_is_comment:
      continue;
    default:
      lasterr = err;
      err = parse_stop_parsing;
    }
  }

  if (script->b_pause)
    ::SetEvent(script->evt_paused);

  script->n_ms_exectime = ::GetTickCount() - script->n_ms_exectime;

  if (lasterr != err)
    script->set_last_engine_error((int)lasterr);
  else
    script->set_last_engine_error((int)parse_error_none);
  return compel_engine_to_lib_error(script->get_last_engine_error());
}

// steps one line in compel script
COMPEL_EXPORT int COMPEL_API compel_script_step(compel_script_t compel_script)
{
  GET_COMPEL_LIB_SCRIPT;

  interpreter_t *interpreter = script->get_interpreter();

  parse_errors_e err;

  size_t curline = interpreter->get_cur_source_lineno();

  if (script->m_b_dbgout_script)
    _dbg_out(">line:%d >%s<\n", curline, interpreter->get_cur_source_line_str());

  err = interpreter->interpret_line();

  script->set_last_engine_error((int)err);

  return compel_engine_to_lib_error((int)err);
}

// interprets one line at a time (no preparsing occurs)
COMPEL_EXPORT int COMPEL_API compel_script_interpret_line(compel_script_t compel_script, const char *line)
{
  GET_COMPEL_LIB_SCRIPT;
  interpreter_t *interpreter = script->get_interpreter();

  parse_errors_e err = interpreter->interpret_new_line(line);
  size_t curline = interpreter->get_cur_source_lineno();
  if (script->m_b_dbgout_script)
    _dbg_out(">line:%d >%s<\n", curline, line);

  script->set_last_engine_error((int) err);
  return compel_engine_to_lib_error((int)err);
}

// loads a script from a string delimited by "delims"
COMPEL_EXPORT bool COMPEL_API compel_script_load_lines(compel_script_t compel_script, const char *lines, const char *delims)
{
  GET_COMPEL_LIB_SCRIPT;
  return script->script_load_from_string(lines, delims, false);
}

// clears the script lines
COMPEL_EXPORT void COMPEL_API compel_script_clear_lines(compel_script_t compel_script)
{
  GET_COMPEL_LIB_SCRIPT;
  script->script_clear_lines();
}

// gets a value_t's value quickly, if not found, an empty string is returned
COMPEL_EXPORT const char * COMPEL_API compel_value_value_get(
  compel_script_t compel_script, 
  const char *val_name)
{
  compel_value_t val = compel_value_find(compel_script, val_name);
  if (val == 0)
    return "";
  return compel_value_get(compel_script, val);
}


COMPEL_EXPORT bool COMPEL_API compel_value_value_set(
  compel_script_t compel_script, 
  const char *val_name,
  const char *val_val)
{
  compel_value_t val = compel_value_find(compel_script, val_name);
  if (val == 0)
    return false;

  compel_value_set(compel_script, val, val_val);
  return true;
}

// Deletes a value_t from the namespace
COMPEL_EXPORT bool COMPEL_API compel_value_destroy(
  compel_script_t compel_script, 
  const char *varname)
{
  GET_COMPEL_LIB_SCRIPT;

  interpreter_t *_int = script->get_interpreter();
  value_t *value = _int->get_value(varname);
  if (value == 0)
    return false;

  _int->remove_symbol(varname);
  return true;
}

// returns a value reference
COMPEL_EXPORT compel_value_t COMPEL_API compel_value_find(compel_script_t compel_script, const char *varname)
{
  GET_COMPEL_LIB_SCRIPT;

  interpreter_t *_int = script->get_interpreter();
  value_t *value = _int->get_value(varname);

  return TO_COMPEL_VALUE(value);
}

// returns a value reference
COMPEL_EXPORT compel_value_t COMPEL_API compel_value_create(compel_script_t compel_script, const char *varname, const char *initial_value)
{
  // is it there already?
  if (compel_value_find(compel_script, varname) != 0)
    return 0;

  GET_COMPEL_LIB_SCRIPT;
  interpreter_t *_int = script->get_interpreter();
  return TO_COMPEL_VALUE(_int->add_symbol(varname, new value_t(initial_value)));
}

//bool compel_value_delete(compel_script_t, const char *varname);

// modifies a value
COMPEL_EXPORT void COMPEL_API compel_value_set(compel_script_t compel_script, compel_value_t compel_value, const char *valstr)
{
  UNREFERENCED_PARAMETER(compel_script);
  GET_COMPEL_VALUE;
  value->set_str_value(valstr);
}

// returns the value of a value_t
COMPEL_EXPORT const char * COMPEL_API compel_value_get(compel_script_t compel_script, compel_value_t compel_value)
{
  UNREFERENCED_PARAMETER(compel_script);
  GET_COMPEL_VALUE;
  return value->get_str_value();
}

// creates an object
COMPEL_EXPORT compel_object_t COMPEL_API compel_object_create(compel_script_t compel_script, const char *objname)
{
  // already exists?
  if (compel_object_find(compel_script, objname) != 0)
    return 0;

  GET_COMPEL_LIB_SCRIPT;
  interpreter_t *_int = script->get_interpreter();
  return reinterpret_cast<compel_object_t>(_int->add_symbol(objname, new object_t));
}

// returns a reference to an object_t
COMPEL_EXPORT compel_object_t COMPEL_API compel_object_find(
  compel_script_t compel_script, 
  const char *objname)
{
  GET_COMPEL_LIB_SCRIPT;

  interpreter_t *_int = script->get_interpreter();
  object_t *object= _int->get_object(objname);

  return reinterpret_cast<compel_object_t>(object);
}

COMPEL_EXPORT char *COMPEL_API compel_script_evaluate_expression(
  compel_script_t compel_script,
  const char *expr,
  bool bKeepQuotes,
  char chDelim)
{
  GET_COMPEL_LIB_SCRIPT;

  interpreter_t *_int = script->get_interpreter();

  std::ostringstream os;

  compel_string_tokenizer_t tok;

  size_t to = tok.parse(expr);

  for (size_t i=0;i<to;i++)
  {
    const char *evaluated_str = _int->evaluate_at(i, &tok);

    bool bQuoteIt = false;

    if (bKeepQuotes)
      bQuoteIt = strchr(evaluated_str, ' ') != 0;

    if (bQuoteIt && bKeepQuotes)
      os << '"';

    os << evaluated_str;

    if (bQuoteIt && bKeepQuotes)
      os << '"';

    if (chDelim != 0)
    {
      if (i != to-1)
        os << " ";
    }
  }

  std::string s = os.str();

  return compel_string_create(s.c_str(), 0);
}

// Converts an object to its string representation
COMPEL_EXPORT char *COMPEL_API compel_object_to_string(
  compel_script_t compel_script, 
  const char *objname)
{
  GET_COMPEL_LIB_SCRIPT;

  interpreter_t *_int = script->get_interpreter();
  object_t *object= _int->get_object(objname);

  if (object == 0)
    return 0;

  std::string str;

  object->to_string(str);
  return compel_string_create(str.c_str(), str.size());
}

// deletes an object
COMPEL_EXPORT bool COMPEL_API compel_object_destroy(compel_script_t compel_script, const char *objname)
{
  GET_COMPEL_LIB_SCRIPT;

  interpreter_t *_int = script->get_interpreter();
  object_t *object= _int->get_object(objname);
  if (object == 0)
    return false;

  _int->remove_symbol(objname);
  return true;
}

// returns a value_t of a given object
COMPEL_EXPORT compel_value_t COMPEL_API compel_object_find_attr(compel_script_t compel_script, compel_object_t compel_object, const char *attrname)
{
  UNREFERENCED_PARAMETER(compel_script);
  GET_COMPEL_OBJECT;
  value_t *value = object->find_attribute(attrname);
  return TO_COMPEL_VALUE(value);
}

// adds an attribute to a given object
COMPEL_EXPORT compel_value_t COMPEL_API compel_object_add_attr(
  compel_script_t compel_script, 
  compel_object_t compel_object, 
  const char *attrname,
  const char *initial_value)
{
  UNREFERENCED_PARAMETER(compel_script);
  GET_COMPEL_OBJECT;
  value_t val(initial_value);
  
  return TO_COMPEL_VALUE(object->insert_attribute(attrname, val));
}

// removes attributes from a given object
COMPEL_EXPORT bool COMPEL_API compel_object_remove_attr(
  compel_script_t compel_script,  
  compel_object_t compel_object, 
  const char *attrname)
{
  UNREFERENCED_PARAMETER(compel_script);
  GET_COMPEL_OBJECT;
  return object->remove_attribute(attrname);
}

// adjusts the current execution line number
COMPEL_EXPORT void COMPEL_API compel_script_set_lineno(
  compel_script_t compel_script,
  size_t lineno)
{
  GET_COMPEL_LIB_SCRIPT;
  script->get_interpreter()->set_cur_src_line(lineno);
}

// returns the current executing line number
COMPEL_EXPORT size_t COMPEL_API compel_script_get_lineno(compel_script_t compel_script)
{
  GET_COMPEL_LIB_SCRIPT;
  return script->get_interpreter()->get_cur_source_lineno();
}

// Retrieves the source lines count
COMPEL_EXPORT int COMPEL_API compel_script_get_lines_count(
  compel_script_t compel_script)
{
  GET_COMPEL_LIB_SCRIPT;
  return (int) script->get_interpreter()->get_source_lines_count();
}

// Retrieves the source line value
COMPEL_EXPORT const char *COMPEL_API compel_script_get_line(
  compel_script_t compel_script,
  size_t lineno)
{
  GET_COMPEL_LIB_SCRIPT;
  interpreter_t *_int = script->get_interpreter();
  return _int->get_source_line_str(lineno);
}

// Patches the source line value
COMPEL_EXPORT bool COMPEL_API compel_script_set_line(
  compel_script_t compel_script,
  size_t lineno,
  const char *value)
{
  GET_COMPEL_LIB_SCRIPT;
  return script->get_interpreter()->set_src_line_str(lineno, value);
}

// registers a new libuser command
COMPEL_EXPORT int COMPEL_API compel_lu_cmd_register(compel_script_t compel_script, p_lib_usercommand_info_t cmd)
{
  GET_COMPEL_LIB_SCRIPT;

  fnc_libusercommand *fnc = new fnc_libusercommand(script->get_interpreter(), cmd);
  parse_errors_e p_err = fnc->register_function(compel_script);
  if (p_err != parse_error_none)
    delete fnc;
  return compel_engine_to_lib_error((int) p_err);
}

// returns the lib_user_command_info_t associated with the current function
COMPEL_EXPORT p_lib_usercommand_info_t COMPEL_API compel_lu_cmd_get_info(
  compel_script_t compel_script)
{
  GET_COMPEL_LIB_SCRIPT;
  return script->get_lib_usercommand_get_info();
}

// Sets the return value of the current libuser command
COMPEL_EXPORT bool COMPEL_API compel_lu_cmd_set_retval(compel_script_t compel_script, const char *retval)
{
  GET_COMPEL_LIB_SCRIPT;
  return script->set_lib_usercommand_retvalue(retval);
}

// Registers a libuser command given the parameters                      
COMPEL_EXPORT int COMPEL_API compel_lu_cmd_register2(
  compel_script_t script,
  compel_lib_usercommand_cb_t cb,
  const char *name,
  const size_t minargs,
  const size_t maxargs)
{
  lib_usercommand_info_t cmd = {0};
  cmd.cb = cb;
  cmd.minargs = minargs;
  cmd.maxargs = maxargs;
  cmd.name = name;
  return compel_lu_cmd_register(script, &cmd);
}

COMPEL_EXPORT void COMPEL_API compel_script_set_error_handler(
  compel_script_t compel_script, 
  compel_script_error_handler_cb_t handler)
{
  GET_COMPEL_LIB_SCRIPT;
  script->set_script_error_handler(handler);
}

COMPEL_EXPORT int COMPEL_API compel_internal(
  compel_script_t compel_script, 
  int arg1, int arg2, int arg3, int arg4)
{
  GET_COMPEL_LIB_SCRIPT;

  return script->internal(arg1, arg2, arg3, arg4);
}

COMPEL_EXPORT int COMPEL_API compel_extension_load(
  compel_script_t compel_script, 
  const char *extname,
  const char *extpath)
{
  GET_COMPEL_LIB_SCRIPT;
  script->user_extension_load(extname, extpath);
  return script->get_last_engine_error();
}
