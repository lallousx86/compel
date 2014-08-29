#ifndef __COMPEL_SCRIPT_LIB_03302006
#define __COMPEL_SCRIPT_LIB_03302006

#include "fwd.h"
#include "symbol_table.h"
#include "interpreter.h"
#include "interpreter_helper.h"
#include "compel_lib.h"
#include <windows.h>
#include <map>
#include <list>

#include "fnc_libusercommand.h"

#define S_COMPEL_VERSION "0.3.1 (" __DATE__ " " __TIME__ ")"

struct user_extension_struct_t
{
  HMODULE h_ext;
  compel_extension_deinit_cb_t deinit;
  compel_user_context_t userctx;
  std::string extname, extpath;

  void assign(const user_extension_struct_t &rhs)
  {
    h_ext = rhs.h_ext;
    deinit = rhs.deinit;
    userctx = rhs.userctx;
    extname = rhs.extname;
    extpath = rhs.extpath;
  }

  user_extension_struct_t()
  {
    h_ext = 0;
    deinit = 0;
    userctx = 0;
  }

  user_extension_struct_t(const user_extension_struct_t &rhs)
  {
    assign(rhs);
  }

  user_extension_struct_t &operator=(const user_extension_struct_t &rhs)
  {
    assign(rhs);
    return *this;
  }
};

class compel_lib_script_t : public interpreter_errors_i
{
private:
  typedef std::map<std::string, function_t *, StdStringCmpNoCase> functions_map_t;
  typedef std::list<user_extension_struct_t> user_extension_list_t;
  typedef std::map<std::string, compel_user_context_t, StdStringCmpNoCase> str_uctx_map_t;

  symbol_table_t *_sym_tbl;
  interpreter_t *_interpreter;
  user_extension_list_t _ext_list;
  str_uctx_map_t _context_map;

  // prevent object copying
  compel_lib_script_t(const compel_lib_script_t &) { }
  compel_lib_script_t &operator=(const compel_lib_script_t &) { }

  bool _b_delete_on_dtor;

  functions_map_t _functions;

  int _last_engine_err;
  int _failing_line;

  std::string _str_extensions;

  interpreter_helper_t inthelper;

  void register_functions(bool bUseNS);
  bool register_commands();
  bool register_reserved_variables();
  inline void set_manual_delete(bool b_manual) { _b_delete_on_dtor = b_manual; }
  void clear_functions();

  parse_errors_e ie_on_parse_error(size_t lineno, parse_errors_e err);
  compel_script_error_handler_cb_t _script_error_handler;

  const fnc_libusercommand *get_current_libusercommand();

  void user_extension_unload_all();

  bool is_built_in_extension(const char *extname);
  
public:
  compel_script_t to_compel_script_t();

  // public - script - options
  bool m_b_usefullns;
  bool m_b_dbgout_script;
  std::string m_script_args;
  std::string m_script_file;
  std::string m_extensions;
  compel_script_t m_compel_script;

  void set_context(const char *guid, void *context);
  void *get_context(const char *guid);

  compel_lib_script_t();
  ~compel_lib_script_t();

  bool init();

  // interpreter
  inline interpreter_t *get_interpreter() { return _interpreter; }
  inline void set_interpreter(interpreter_t *interpreter) { _interpreter = interpreter; }

  inline void set_last_engine_error(int err) { _last_engine_err = err; }
  inline int  get_last_engine_error() const { return _last_engine_err; }

  //! Returns built-in extensions
  static const char *get_builtin_avail_extensions();

  bool script_load_from_file(const char *filename = 0, bool b_clear_old_lines = true);
  bool script_load_from_string(const char *script_string, const char *delim = "\n\r", bool b_clear_old_lines = true);
  void script_clear_lines();

  bool set_script_file(const char *filename);

  bool set_lib_usercommand_retvalue(const char *val);
  p_lib_usercommand_info_t get_lib_usercommand_get_info();

  void set_script_error_handler(compel_script_error_handler_cb_t script_error_handler);

  bool register_arguments(const char *new_args = 0);
  bool register_environment_variables();

  int internal(int arg1, int arg2, int arg3, int arg4);

  /*!
  Loads user extension
  \returns
  - compel_error_symbol_undefined: extension not found
  - compel_error_invalid_extension: missing callbacks
  - compel_error_success: the extension was loaded
  - compel_error_symbol_redefined: if extension name clashes with built-in extension names
  - other values: what the init callback returns
  */
  bool user_extension_load(const char *extname, const char *extpath);

  //! Checks whether a user extension is already loaded
  bool user_extension_is_loaded(const char *extname);

  bool b_pause;
  DWORD n_ms_exectime;
  HANDLE evt_paused;
};

#endif