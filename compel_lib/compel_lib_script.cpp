#include <windows.h>
#include <sstream>
#include <algorithm>
#pragma warning (disable: 4312)

// Engine
#include "symbol.h"
#include "variable.h"
#include "interpreter.h"
#include "interpreter_helper.h"
#include "lines.h"
#include "value.h"
#include "object.h"
#include "function.h"
#include "symbol_table.h"
#include "compel_string_tokenizer.h"
#include "parse_util.h"

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
#include "fnc/fnc_time.h"
#include "fnc_compellib.h"
#include "fnc/fnc_string.h"
#include "fnc/fnc_com.h"
#include "fnc/fnc_ini.h"
#include "fnc/fnc_mm.h"
#include "fnc/fnc_win.h"

// COMPEL lib
#include "compel_lib_script.h"

static const char *s_str_env        = "$ENV";
static const char *s_str_compel     = "$_COMPEL";
static const char *s_str_args       = "$ARGS";
static const char *s_str_extension_separator = ";";

static bool save_raw_lines(interpreter_helper_t &inthlp, char *fn)
{
  lines_t &lines = inthlp.get_interpreter_lines();

  std::ofstream of(fn);
  if (!of)
    return false;

  size_t ncount = lines.count();
  for (size_t i=0;i<ncount;i++)
  {
    of << lines.line(i) << std::endl;
  }
  of.close();
  return true;
}

// ctor does initializations
compel_lib_script_t::compel_lib_script_t()
{
  m_b_usefullns =
  m_b_dbgout_script = false;
  m_compel_script = 0;

  _sym_tbl      = new symbol_table_t;
  _interpreter  = new interpreter_t;
  _script_error_handler = 0;
  _b_delete_on_dtor = true;

  evt_paused = ::CreateEvent(NULL, FALSE, FALSE, NULL);
}

// dtor does cleanups
compel_lib_script_t::~compel_lib_script_t()
{
  if (_b_delete_on_dtor)
  {
    // Order of deletion is very important
    // . User extensions
    // . Internal commands list
    // . Interpreter
    // . Symbol table

    // Unload external extensions
    user_extension_unload_all();

    // clear functions list
    clear_functions();

    delete _interpreter;

    delete _sym_tbl;

    ::CloseHandle(evt_paused);
  }
}

// clears all registered functions table 
// does not remove already registered commands from the symbol table
void compel_lib_script_t::clear_functions()
{
  for (functions_map_t::iterator it = _functions.begin(); it != _functions.end();++it)
  {
    delete it->second;
  }
  _functions.clear();
}

/*!
*/
void compel_lib_script_t::register_functions(bool bUseNS)
{
  clear_functions();

  _functions[fnc_binary_arith::fnc_name]            = new fnc_binary_arith(bUseNS);
  _functions[fnc_binary_comparison::fnc_name]       = new fnc_binary_comparison();
  _functions[fnc_fileio::fnc_name]                  = new fnc_fileio(bUseNS);
  _functions[fnc_vars::fnc_name]                    = new fnc_vars();
  _functions[fnc_unconditional_branching::fnc_name] = new fnc_unconditional_branching();
  _functions[fnc_memory::fnc_name]                  = new fnc_memory();
  _functions[fnc_include::fnc_name]                 = new fnc_include();
  _functions[fnc_scopes::fnc_name]                  = new fnc_scopes();
  _functions[fnc_comments::fnc_name]                = new fnc_comments();
  _functions[fnc_crt::fnc_name]                     = new fnc_crt(bUseNS);
  _functions[fnc_thread::fnc_name]                  = new fnc_thread();
  _functions[fnc_dirs::fnc_name]                    = new fnc_dirs(bUseNS);
  _functions[fnc_time::fnc_name]                    = new fnc_time(bUseNS);
  _functions[fnc_com::fnc_name]                     = new fnc_com;
	_functions[fnc_string::fnc_name]                  = new fnc_string(bUseNS);
  _functions[fnc_ini::fnc_name]                     = new fnc_ini(bUseNS);
  _functions[fnc_mm::fnc_name]                      = new fnc_mm(bUseNS);
  _functions[fnc_win::fnc_name]                     = new fnc_win(bUseNS);
  _functions[fnc_shell::fnc_name]                   = new fnc_shell(false);
  _functions[fnc_compellib::fnc_name]               = new fnc_compellib(this);
}

// registers all the commands found in compel_lib_script_t::_functions
bool compel_lib_script_t::register_commands()
{
  bool bUseNS = m_b_usefullns;

  register_functions(bUseNS);

  compel_string_tokenizer_t fnc_tok;

  size_t pcount = fnc_tok.parse(m_extensions.c_str(), s_str_extension_separator, 0);

  functions_map_t::iterator it;
  for (size_t i=0;i<pcount;i++)
  {
    const char *fnc_name = fnc_tok.get_string(i);

    if ( (it = _functions.find(fnc_name)) == _functions.end())
      continue;

    it->second->register_function(_interpreter);
  }

  return true;
}

// registers command-line arguments
bool compel_lib_script_t::register_arguments(const char *new_args)
{
  _interpreter->remove_symbol(s_str_args);

  compel_string_tokenizer_t slp;

  if (new_args != 0)
    m_script_args = new_args;

  size_t cnt = slp.parse(m_script_args.c_str());

  object_t *arg_obj = new object_t;

  arg_obj->insert_attribute("0")->set_str_value(m_script_file.c_str());

  for (size_t i=0;i<cnt;i++)
  {
    char attr[20];
    sprintf(attr, "%d", i+1);
    arg_obj->insert_attribute(attr)->set_str_value(slp.get_string(i));
  }

  arg_obj->insert_attribute("count")->set_int_value((long)cnt+1);

  _interpreter->add_symbol(s_str_args, arg_obj);

  return true;
}

// ;! remove as fast as you implement functions
bool compel_lib_script_t::register_environment_variables()
{
  LPSTR lpEnv = ::GetEnvironmentStringsA();

  LPSTR p = lpEnv;

  _interpreter->remove_symbol(s_str_env);

  object_t *env = new object_t;

  size_t len = 0;
  do 
  {
    std::string s = p;
    size_t pos = s.find("=");
    if (pos != 0 && pos != std::string::npos)
    {
      env->insert_attribute(s.substr(0, pos).c_str())->
        set_str_value(s.substr(pos+1).c_str());
    }

    len = strlen(p);
    p += len + 1;
  } while(len != 0);

  _interpreter->add_symbol(s_str_env, env);

  ::FreeEnvironmentStringsA(lpEnv);

  return true;
}

// register internal reserved variables
bool compel_lib_script_t::register_reserved_variables()
{
  _interpreter->remove_symbol(s_str_compel);

  object_t *obj = new object_t;

  obj->insert_attribute("version")->set_str_value(S_COMPEL_VERSION);
  _interpreter->add_symbol(s_str_compel, obj);

  return true;
}

// initialize the script engine
bool compel_lib_script_t::init()
{
  _interpreter->set_symbol_table(_sym_tbl);

  inthelper.set_interpreter(get_interpreter());

  // initialize the scripting engine
  if 
   (
    !register_commands() || 
    !register_arguments(0) || 
    !register_reserved_variables() ||
    !register_environment_variables()
   )
  {
    return false;
  }

  return true;
}

// forms and returns available built-in extensions
const char *compel_lib_script_t::get_builtin_avail_extensions()
{
  static std::string str_extensions;

  if (!str_extensions.empty())
    return str_extensions.c_str();

  std::ostringstream ostr;
  
  compel_lib_script_t *script = new compel_lib_script_t;
  
  script->register_functions(false);

  for (functions_map_t::iterator it = script->_functions.begin(); 
       it != script->_functions.end();++it)
  {
    ostr << it->first << s_str_extension_separator;
  }

  delete script;

  str_extensions = ostr.str();

  // remove the trailing separator
  if (!str_extensions.empty())
    str_extensions.erase(str_extensions.size()-1);

  return str_extensions.c_str();
}

// sets the script filename
// also adjusts the global $ARGS.0 value
bool compel_lib_script_t::set_script_file(const char *filename)
{
  interpreter_t *_int = get_interpreter();
  object_t *arg_obj = _int->get_object(s_str_args);
  if (arg_obj == 0)
    return false;
  arg_obj->find_attribute("0")->set_str_value(filename);
  return true;
}

// load's set of comands from a filename
bool compel_lib_script_t::script_load_from_file(
	const char *filename, 
	bool b_clear_old_lines)
{
  filename = filename ? filename : m_script_file.c_str();
  if (filename == 0)
    return false;

  interpreter_t *_int = get_interpreter();

  // preparse
  int start_or_failing_line;

  if (b_clear_old_lines)
    start_or_failing_line = 0;
  else
  {
    // start preparing from the ending line
    start_or_failing_line = (int) _int->get_source_lines_count();
  }

  // load lines
  if (!_int->load_lines_from_file(filename, b_clear_old_lines))
    return false;

  // adjust script name variable
  set_script_file(filename);

  parse_errors_e err = _int->prepare(start_or_failing_line);
  int ierr = compel_engine_to_lib_error((int) err);

  if (err != parse_error_none)
  {
    _failing_line = start_or_failing_line;
    set_last_engine_error(ierr);
    return false;
  }
  set_last_engine_error(ierr);
  return true;
}

// clears the script's lines
void compel_lib_script_t::script_clear_lines()
{
  get_interpreter()->clear_lines();
}

// loads a script from a string
// each command is separated by "delim"
bool compel_lib_script_t::script_load_from_string(
	const char *script_string, 
	const char *delim,
	bool b_clear_old_lines)
{
  compel_string_tokenizer_t tok;
  size_t pcount = tok.parse(script_string, delim, 0, 0, 0);
  lines_t lines;

  interpreter_t *_int = get_interpreter();

  for (size_t i=0;i<pcount;i++)
  {
    const char *p = tok.get_string(i);
    if (*p == 0)
      continue;
    lines.add(p);
  }

  int start_or_failing_line;

	if (b_clear_old_lines)
		start_or_failing_line = 0;
	else
		start_or_failing_line = (int) _int->get_source_lines_count();

  _int->load_from_lines(lines, b_clear_old_lines);

  // adjust script name variable
  set_script_file("<string>");

  // preparse
  parse_errors_e err = _int->prepare(start_or_failing_line);

  if (err != parse_error_none)
  {
    set_last_engine_error((int) err);
    return false;
  }

  return true;
}

// sets the current executing command's return value
// the command must be of type libusercommand only
bool compel_lib_script_t::set_lib_usercommand_retvalue(const char *val)
{
  fnc_libusercommand *lucmd;
  
  lucmd = const_cast<fnc_libusercommand *>(get_current_libusercommand());

  if (lucmd == 0)
    return false;

  value_t *ret_val = _interpreter->get_value(lucmd->get_ret_name().c_str());
  if (ret_val == 0)
    return false;

  ret_val->set_str_value(val);

  return true;
}

// On parse error handler
parse_errors_e compel_lib_script_t::ie_on_parse_error(size_t lineno, parse_errors_e err)
{
  // no error handler, then allow other handlers to take care of it
  if (_script_error_handler == 0)
    return parse_error_handler_continue_search;

  // convert engine error to lib error
  int l_err = compel_engine_to_lib_error((int) err);

  // pass to user
  l_err = _script_error_handler(m_compel_script, lineno, l_err);

  // pass to engine
  return (parse_errors_e) compel_lib_to_engine_error(l_err);
}

// Sets error handler or removes it
void compel_lib_script_t::set_script_error_handler(compel_script_error_handler_cb_t script_error_handler)
{
  // no handler specified?
  if (script_error_handler == 0)
  {
    // last had a handler and now no handler? let us remove the handler
    if (_script_error_handler != 0)
      _interpreter->error_client_unregister(this);
    return;
  }

  // first time registration?
  if (_script_error_handler == 0)
  {
    int high, low;
    _interpreter->error_client_get_priorities(high, low);

    // try to register with priority 1000 or high+1
    ie_set_priority(high < 1000 ? 1000 : high + 1);

    _interpreter->error_client_register(this);
  }

  // save set handler
  _script_error_handler = script_error_handler;
}

int compel_lib_script_t::internal(int arg1, int arg2, int arg3, int arg4)
{
  interpreter_helper_t hlp(_interpreter);

  switch (arg1)
  {
  case compel_internal_writeraw:
    return save_raw_lines(hlp, arg2 != 0 ? (char *)arg2 : "raw.compel") ? 1 : 0;
  case compel_internal_showlines:
    hlp.show_lines();
    break;
  case compel_internal_setdbgout:
    m_b_dbgout_script = arg2 == 0 ? false : true;
    break;
  case compel_internal_showsymtbl:
    hlp.show_symbols();
    break;
  case compel_internal_pause:
    b_pause = true;
    ::WaitForSingleObject(evt_paused, INFINITE);
    return (int) _interpreter->get_cur_source_lineno();
  case compel_internal_exectime:
    return n_ms_exectime;
  case compel_internal_preparse_failing_line:
    return _failing_line;
  }
  return 0;
}

p_lib_usercommand_info_t compel_lib_script_t::get_lib_usercommand_get_info()
{
  fnc_libusercommand *lucmd;

  lucmd = const_cast<fnc_libusercommand *>(get_current_libusercommand());

  if (lucmd == 0)
    return 0;
  return lucmd->get_info();
}

const fnc_libusercommand *compel_lib_script_t::get_current_libusercommand()
{
  return dynamic_cast<const fnc_libusercommand *>(_interpreter->get_cur_function());
}

compel_script_t compel_lib_script_t::to_compel_script_t()
{
  return reinterpret_cast<compel_script_t>(this);
}

bool compel_lib_script_t::user_extension_is_loaded(const char *extname)
{
  std::string name(extname);

  parse_util::to_lower(name);

  for (user_extension_list_t::iterator it=_ext_list.begin();it != _ext_list.end();++it)
  {
    user_extension_struct_t &ext = *it;
    if (ext.extname == name)
      return true;
  }

  return false;
}

bool compel_lib_script_t::user_extension_load(const char *extname, const char *extpath)
{
  // Skip loading the extension if it was already loaded
  if (user_extension_is_loaded(extname))
    return true;

  // Check uniquity of extension name
  if (is_built_in_extension(extname))
  {
    set_last_engine_error((int)compel_error_symbol_redefined);
    return false;
  }

  
  user_extension_struct_t ext;

  // Can load the extension?
  if ( (ext.h_ext = ::LoadLibraryA(extpath)) == 0)
  {
    set_last_engine_error((int)compel_error_symbol_undefined);
    return false;
  }

  compel_extension_init_cb_t init_cb;
  
  // Get the init callback
  if ((init_cb = (compel_extension_init_cb_t)::GetProcAddress(ext.h_ext, "compel_ext_init")) == NULL)
  {
    set_last_engine_error(compel_error_invalid_extension);
    return false;
  }

  int err = init_cb(to_compel_script_t(), &ext.userctx);
  if (err != compel_error_success)
  {
    // Unload extension
    ::FreeLibrary(ext.h_ext);

    // Set last error
    set_last_engine_error(err);

    // Success
    return true;
  }

  // Optional deinit callback
  ext.deinit = (compel_extension_deinit_cb_t) ::GetProcAddress(ext.h_ext, "compel_ext_deinit");

  // Save other stuff
  ext.extname = extname;
  ext.extpath = extpath;

  // Store extension name as lower case
  parse_util::to_lower(ext.extname);

  // Store extension in internal list
  _ext_list.push_back(ext);

#ifdef COMPEL_DEBUG_2
  printf("ext: deinit:%p h_ext:%08X userctx:%p %s %s\n", ext.deinit, ext.h_ext, ext.userctx, 
    ext.extname.c_str(), ext.extpath.c_str());
#endif

  set_last_engine_error((int)compel_error_success);

  return true;
}

void compel_lib_script_t::user_extension_unload_all()
{
  for (user_extension_list_t::iterator it=_ext_list.begin();it != _ext_list.end();++it)
  {
    user_extension_struct_t &ext = *it;

#ifdef COMPEL_DEBUG_2
    printf("ext: deinit:%p h_ext:%08X userctx:%p %s %s\n", ext.deinit, ext.h_ext, ext.userctx, 
      ext.extname.c_str(), ext.extpath.c_str());
#endif COMPEL_DEBUG_2
    // Optionally call deinitialization routine
    if (ext.deinit)
      ext.deinit(to_compel_script_t(), ext.userctx);

    // Unload extension
    ::FreeLibrary(ext.h_ext);
  }
  _ext_list.clear();
}

// Checks the name to see if it is a built-in extension
bool compel_lib_script_t::is_built_in_extension(const char *extname)
{
  functions_map_t::iterator it = _functions.find(extname);
  if (it == _functions.end())
    return false;
  return true;
}

void compel_lib_script_t::set_context(const char *guid, compel_user_context_t context)
{
  _context_map[guid] = context;
}

compel_user_context_t compel_lib_script_t::get_context(const char *guid)
{
  str_uctx_map_t::iterator it = _context_map.find(guid);
  if (it == _context_map.end())
    return 0;
  return it->second;
}
