#ifndef __PARSER_01242006__
#define __PARSER_01242006__

#include <string.h>
#include <stdlib.h>
#include "compel_string_tokenizer.h"
#include "fwd.h"
#include "symbol.h"
#include "variable.h"
#include "value.h"
#include "object.h"
#include "symbol_table.h"
#include "lines.h"
#include "function.h"

// ==================================================================================
//
//    INTERPRETER  ERROR  HANDLER  INTERFACE
//
// ==================================================================================

// Interpreter error handlers interface
class interpreter_errors_i
{
private:
  int _priority;
public:
  virtual ~interpreter_errors_i() { }
  interpreter_errors_i(int priority = 0) { ie_set_priority(priority); }
  virtual parse_errors_e ie_on_parse_error(size_t lineno, parse_errors_e err) = 0; //{ return parse_stop_parsing; } 
  inline void ie_set_priority(int priority) { _priority = priority; }
  inline const int ie_get_priority() const { return _priority; }

  bool operator<(const interpreter_errors_i &r) const
  {
    return _priority < r._priority;
  }
};

class interpreter_errors_sorting_functor
{
public:
  bool operator()(const interpreter_errors_i *lhs, const interpreter_errors_i *rhs) const;
};


// ==================================================================================
//
//                             INTERPRETER CLASS
//
// ==================================================================================

struct interpreter_bind_struct_t
{
  DWORD tid;
  interpreter_t *int_bind;
};

class interpreter_t
{
  friend class interpreter_helper_t;
private:

  std::string _temp_str;

  // don't want copying this class
  interpreter_t(const interpreter_t &);

  void operator=(const interpreter_t &) { }

protected:
  compel_string_tokenizer_t *_slp;

  symbol_table_t *_symtbl;

  lines_t _lines;

  size_t _cursourceline;

  // the currently executing function
  function_t *_cur_function;

  // warning message and error messages sentby user commands
  std::string _msg_warning, _msg_error;

  typedef std::set<interpreter_errors_i *, interpreter_errors_sorting_functor> interpreter_errors_clients_t;

  interpreter_errors_clients_t _error_clients;

  parse_errors_e _last_err;

public:

  inline const int  get_last_error() const { return (int)_last_err; }

  bool error_client_register(interpreter_errors_i *client);
  bool error_client_unregister(interpreter_errors_i *client);
  bool error_client_get_priorities(int &highest, int &lowest);

  interpreter_t();
  ~interpreter_t();

  static bool    is_variable_name(const char *varname);
  virtual bool   is_deferred_line(compel_string_tokenizer_t * = 0);

  static char    get_variable_prefix();
  static char    get_object_separator();

  virtual void   void_line(size_t lineno);
  virtual bool   is_void_line(compel_string_tokenizer_t * = 0);

  const char    *get_const_at(size_t nAt, compel_string_tokenizer_t * = 0);
  const char     *evaluate_at(size_t nAt, compel_string_tokenizer_t * = 0);

  variable_t    *get_variable(const char *name);
  variable_t    *get_variable_raw(const char *varname);

  variable_t    *get_variable_at(size_t nAt, compel_string_tokenizer_t * = 0);

  object_t      *get_object(const char *objname);
  object_t      *get_object_at(size_t nAt, compel_string_tokenizer_t * = 0);

  variable_t    *get_var_ref(const char *expr, std::string &out);

  value_t       *get_value(const char *name);
  value_t       *get_value_at(size_t nAt, compel_string_tokenizer_t * = 0);

  function_t    *get_function(const char *fncname);

  bool           load_lines_from_file(const char *filename, bool b_clear_old_lines);
  bool           load_from_lines(lines_t &lines, bool b_clear_old_lines);

  void           clear_lines();
	size_t         get_source_lines_count();

  size_t         get_fnc_arg_count() const;
  
  // symbol table functions proxy
  symbol_t      *add_symbol(const char *symname, symbol_t *);
  symbol_t      *find_symbol(const char *symname);
  bool           remove_symbol(const char *symname);
  
  //
  size_t         get_cur_source_lineno() const;
  const char    *get_cur_source_line_str();

  const    char *get_source_line_str(size_t lineno);
  bool           set_src_line_str(size_t lineno, const char *value);

  void           set_cur_src_line(size_t lineno);
  void           set_symbol_table(symbol_table_t *);

  const function_t *get_cur_function() const;

  parse_errors_e interpret_line();
  parse_errors_e interpret_new_line(const char *);
  parse_errors_e interpreter_line_at(size_t lineno);

  parse_errors_e prepare(int &failing_line);

  int             find_deferred_line(size_t line_to_find);
  int             find_deferred_line(compel_string_tokenizer_t * = 0);

  parse_errors_e on_parse_error(size_t lineno, parse_errors_e err);

  void           set_msg_warning(const char *msg);
  void           set_msg_error(const char *msg);
  const char    *get_msg_error();
  const char    *get_msg_warning();

  interpreter_bind_struct_t _int_bind;

};

#endif