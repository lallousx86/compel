#ifndef __COMPEL_SCRIPT_02242006
#define __COMPEL_SCRIPT_02242006

#include "fwd.h"
#include "symbol_table.h"
#include "interpreter.h"
#include "interpreter_helper.h"

#define S_COMPEL_VERSION       "0.2.1 (" __DATE__ " " __TIME__ ")"
#define S_SWITCH_RAWFN         "-rawfile"
#define S_SWITCH_LEAKFN        "-memleaks"
#define S_SWITCH_ARGS          "-args"
#define S_SWITCH_DUMPSYMTBL    "-dumpsymtbl"
#define S_SWITCH_INTERACTIVE   "-i"
#define S_SWITCH_FULLNS        "-fullns"
#define S_SWITCH_CWD           "-cwd"
#define S_SWITCH_DBGOUTSCRIPT  "-dbgoutscript"
#define S_SWITCH_ENABLESHELL   "-shell"

struct compel_cmdline_options_t
{
  std::string 
    fn_symtbldump, 
    fn_memleaks, 
    fn_raw_parse,
    working_dir, old_working_dir;

  bool b_gen_raw_parse;
  bool b_show_mem_leaks;
  bool b_dump_symtbl;
  bool b_interactive;
  bool b_usefullns;
  bool b_dbgout_script;
  bool b_enable_shell;

  std::string args;
  std::string script_file;
  std::string interactive_prompt_str;

  compel_cmdline_options_t();
};

class compel_script_t
{
private:
  compel_cmdline_options_t *_args;
  symbol_table_t           *_sym_tbl;
  interpreter_t            *_interpreter;

  compel_script_t(const compel_script_t &) { }
  compel_script_t &operator=(const compel_script_t &) { }

  bool _b_delete_on_dtor;

public:
  compel_script_t();
  ~compel_script_t();

  bool init();
  interpreter_helper_t     inthelper;

  // interpreter
  inline interpreter_t *get_interpreter() { return _interpreter; }
  inline void set_interpreter(interpreter_t *interpreter) { _interpreter = interpreter; }

  // args manipulation
  inline compel_cmdline_options_t *get_args() { return _args; }
  inline void set_args(compel_cmdline_options_t *args) { _args = args; }

  bool register_commands();
  bool register_arguments();
  bool register_reserved_variables();
  bool register_environment_variables();
  inline void set_manual_delete(bool b_manual) { _b_delete_on_dtor = b_manual; }

};

#endif