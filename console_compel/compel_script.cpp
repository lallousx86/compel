#include "compel_script.h"

// General includes
#include "symbol.h"
#include "variable.h"
#include "interpreter.h"
#include "interpreter_helper.h"
#include "lines.h"
#include "value.h"
#include "object.h"
#include "function.h"
#include "symbol_table.h"

// Objects
#include "obj/memory_object.h"
#include "obj/file_object.h"

// Functions
#include "fnc/fnc_crt.h"
#include "fnc/fnc_branching.h"
#include "fnc/fnc_arith.h"
#include "fnc/fnc_fileio.h"
#include "fnc/fnc_memory.h"
#include "fnc/fnc_vars.h"
#include "fnc/fnc_include.h"
#include "fnc/fnc_scopes.h"
#include "fnc/fnc_usercommand.h"
#include "fnc/fnc_comments.h"
#include "fnc/fnc_shell.h"
#include "fnc/fnc_dirs.h"
#include "fnc/fnc_thread.h"
#include "fnc/fnc_string.h"
#include "fnc/fnc_time.h"
#include "fnc/fnc_com.h"
#include "fnc/fnc_ini.h"
#include "fnc/fnc_mm.h"
#include "fnc/fnc_win.h"

// OS specific libraries
#include <windows.h>

compel_script_t::compel_script_t()
{
  _args = new compel_cmdline_options_t;
  _sym_tbl = new symbol_table_t;
  _interpreter = new interpreter_t;

  _b_delete_on_dtor = true;
}

compel_script_t::~compel_script_t()
{
  if (_b_delete_on_dtor)
  {
    delete _args;
    delete _sym_tbl;
    delete _interpreter;
  }
}

compel_cmdline_options_t::compel_cmdline_options_t()
{
  b_interactive = 
  b_gen_raw_parse = 
  b_show_mem_leaks = 
  b_usefullns =
  b_dbgout_script =
  b_enable_shell = 
  b_dump_symtbl = false;

  fn_raw_parse  = "_raw_parse.compel";
  fn_memleaks   = "_raw_leaks.txt";
  fn_symtbldump = "_raw.symtbl.txt";
  interactive_prompt_str = "#";
}

bool compel_script_t::register_commands()
{
  bool bUseNS = _args->b_usefullns;

  typedef std::list<function_t *> functions_list_t;

  functions_list_t functions;

  functions.push_back(new fnc_binary_arith(bUseNS));
  functions.push_back(new fnc_thread);
  functions.push_back(new fnc_com);
  functions.push_back(new fnc_binary_comparison());
  functions.push_back(new fnc_fileio(bUseNS));
  functions.push_back(new fnc_vars());
  functions.push_back(new fnc_unconditional_branching());
  functions.push_back(new fnc_memory());
  functions.push_back(new fnc_include());
  functions.push_back(new fnc_scopes());
  functions.push_back(new fnc_comments());
  functions.push_back(new fnc_crt(bUseNS));
  functions.push_back(new fnc_shell(_args->b_enable_shell));
  functions.push_back(new fnc_dirs(bUseNS));
  functions.push_back(new fnc_time(bUseNS));
	functions.push_back(new fnc_string(bUseNS));
  functions.push_back(new fnc_ini(bUseNS));
  functions.push_back(new fnc_mm(bUseNS));
  functions.push_back(new fnc_win(bUseNS));

  for (functions_list_t::iterator it=functions.begin();
       it != functions.end();
       ++it)
  {
    function_t *f = *it;
    f->register_function(_interpreter);
    delete f;
  }

  return true;
}

// registers command line arguments
bool compel_script_t::register_arguments()
{
  compel_string_tokenizer_t slp;

  int cnt = slp.parse(_args->args.c_str());

  object_t *arg_obj = new object_t;

  arg_obj->insert_attribute("0")->set_str_value(_args->script_file.c_str());

  for (int i=0;i<cnt;i++)
  {
    char attr[20];
    sprintf(attr, "%d", i+1);
    arg_obj->insert_attribute(attr)->set_str_value(slp.get_string(i));
  }

  arg_obj->insert_attribute("count")->set_int_value(cnt+1);

  _interpreter->add_symbol("$ARGS", arg_obj);

  return true;
}

bool compel_script_t::register_environment_variables()
{
  LPSTR lpEnv = ::GetEnvironmentStrings();

  LPSTR p = lpEnv;
  
  object_t *env = new object_t;

  size_t len = 0;
  do 
  {
    std::string s = p;
    size_t pos = s.find("=");
    if (pos != 0 && pos != std::string::npos)
    {
      env->insert_attribute(s.substr(0, pos).c_str())->set_str_value(s.substr(pos+1).c_str());
    }
  	len = strlen(p);
    p += len + 1;
  } while(len != 0);

  _sym_tbl->add_symbol("$ENV", env);

  ::FreeEnvironmentStrings(lpEnv);

  return true;
}

// register internal reserved variables
bool compel_script_t::register_reserved_variables()
{
  object_t *obj = new object_t;
  obj->insert_attribute("version")->set_str_value(S_COMPEL_VERSION);
  _sym_tbl->add_symbol("$_COMPEL", obj);

  return true;
}

bool compel_script_t::init()
{
  _interpreter->set_symbol_table(_sym_tbl);
  inthelper.set_interpreter(get_interpreter());
  
  if (!register_commands() || 
      !register_arguments() || 
      !register_reserved_variables() ||
      !register_environment_variables())
  {
    return false;
  }

  return true;
}