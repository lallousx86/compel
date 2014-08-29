#pragma warning (disable: 4996)
#define WIN32_LEAN_AND_MEAN

#ifdef _MSC_VER
  #include <crtdbg.h>
#endif

#include <windows.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <map>
#include <string>
#include <algorithm>
#include <list>
#include <iterator>
#include <fstream>
#include <direct.h>
#include <iostream>

#include "compel_string_tokenizer.h"

#include "fwd.h"
#include "interpreter.h"
#include "interpreter_helper.h"
#include "compel_script.h"

#include "fnc/fnc_usercommand.h"

void _dbg_out(char *fmt, ...)
{
  va_list va;

  va_start(va, fmt);

  char buf[2048];
  vsprintf(buf, fmt, va);
  OutputDebugString(buf);
  va_end(va);
}

void show_usage()
{
#define IDENT "  "
  const char *hlp = 
    "compel " S_COMPEL_VERSION " - console\n"
    "\n"
    "basic usage:\n"
    "------------\n"
    IDENT "ccompel script.compel\n"
    "\n"
    "general usage:\n"
    "--------------\n"
    IDENT "ccompel [script.compel] [switch [switch param]]\n"
    "\n"
    "switches:\n"
    "---------\n"
    IDENT S_SWITCH_RAWFN        " [fn] : saves the script file after being processed internally\n"
#ifdef _MSC_VER
    IDENT S_SWITCH_LEAKFN       " [fn] : generates memory leaks in the interpreter\n"
#endif
    IDENT S_SWITCH_ARGS         " \"arg1 arg2 ... argN\" : arguments to be passed to the script\n"
    IDENT S_SWITCH_DUMPSYMTBL   " [fn] : dumps the symbol table upon script termination\n"
    IDENT S_SWITCH_INTERACTIVE  " [prompt] : run script file, if passed, then start prompting for commands\n"
    IDENT S_SWITCH_FULLNS       " : register commands with their full namespace naming\n"
    IDENT S_SWITCH_CWD          " : changes the current directory to the one specified\n"
    IDENT S_SWITCH_DBGOUTSCRIPT " : debug outs the script lines and internal processing\n"
    IDENT S_SWITCH_ENABLESHELL  " : enable shell commands support\n"
    "\n"
    ;

  printf(hlp);
#undef IDENT
}

void parse_arguments(int argc, char *argv[], compel_cmdline_options_t &args)
{
  for (int i=1;i<argc;i++)
  {
    // Passing arguments?
    if (stricmp(argv[i], S_SWITCH_ARGS) == 0&& (i+1 < argc))
    {
      i++;
      args.args = argv[i];
    }
    // Want to show raw filename?
    else if (stricmp(argv[i], S_SWITCH_RAWFN) == 0)
    {
      args.b_gen_raw_parse = true;
      if (i+1 < argc && argv[i+1][0] != '-')
      {
        i++;
        args.fn_raw_parse = argv[i];
      }
    }
    // Want to change working directory
    else if (stricmp(argv[i], S_SWITCH_CWD) == 0)
    {
      if (i+1 < argc && argv[i+1][0] != '-')
      {
        args.working_dir = argv[++i];
      }
    }
#ifdef _MSC_VER
    // Want to show leak information?
    else if (stricmp(argv[i], S_SWITCH_LEAKFN) == 0)
    {
      args.b_show_mem_leaks = true;
      if (i+1 < argc && argv[i+1][0] != '-')
      {
        i++;
        args.fn_memleaks = argv[i];
      }
    }
#endif
    // Interactive switch?
    else if (stricmp(argv[i], S_SWITCH_INTERACTIVE) == 0)
    {
      args.b_interactive = true;
      if (i+1 < argc && argv[i+1][0] != '-')
      {
        i++;
        args.interactive_prompt_str = argv[i];
      }
    }
    // Want to dump symbol table?
    else if (stricmp(argv[i], S_SWITCH_DUMPSYMTBL) == 0)
    {
      args.b_dump_symtbl = true;
      if (i+1 < argc && argv[i+1][0] != '-')
      {
        i++;
        args.fn_symtbldump = argv[i];
      }
    }
    // Want to use fullname space with commands?
    else if (stricmp(argv[i], S_SWITCH_FULLNS) == 0)
    {
      args.b_usefullns = true;
    }
    // Want to use fullname space with commands?
    else if (stricmp(argv[i], S_SWITCH_ENABLESHELL) == 0)
    {
      args.b_enable_shell = true;
    }
    else if (stricmp(argv[i], S_SWITCH_DBGOUTSCRIPT) == 0)
    {
      args.b_dbgout_script = true;
    }
    // Passing a script name?
    else if (args.script_file.empty())
      args.script_file = argv[i];
  }
}

void save_raw_lines(interpreter_helper_t &inthlp, char *fn)
{
  lines_t &lines = inthlp.get_interpreter_lines();

  std::ofstream of(fn);

  size_t ncount = lines.count();
  for (size_t i=0;i<ncount;i++)
  {
    of << lines.line(i) << std::endl;
  }
  of.close();
}

static size_t line_to_lines(const char *line, lines_t &lines)
{
  compel_string_tokenizer_t slp;
  
  size_t pcount = slp.parse(line, "`", 0, 0);
  lines.clear();

  for (size_t i=0;i<pcount;i++)
    lines.add(slp.get_string(i));

  return pcount;
}

class run_script_error_handler : public interpreter_errors_i
{
private:
  interpreter_t *_int;
public:
  run_script_error_handler(interpreter_t *interpreter)
  {
    int hi, lo;
    _int = interpreter;
    _int->error_client_get_priorities(hi, lo);
    ie_set_priority(lo-1);
    _int->error_client_register(this);
  }

  ~run_script_error_handler()
  {
    _int->error_client_unregister(this);
  }

  parse_errors_e ie_on_parse_error(size_t lineno, parse_errors_e err)
  {
    printf("Script parsing error %d at line %d. Stopped.\n", err, lineno+1);
    return parse_stop_parsing;
  }
};

class run_interactive_script_error_handler : public interpreter_errors_i
{
private:
  interpreter_t *_int;
public:
  run_interactive_script_error_handler(interpreter_t *interpreter)
  {
    int hi, lo;
    _int = interpreter;
    _int->error_client_get_priorities(hi, lo);
    ie_set_priority(lo-1);
    _int->error_client_register(this);
  }

  ~run_interactive_script_error_handler()
  {
    _int->error_client_unregister(this);
  }

  parse_errors_e ie_on_parse_error(size_t lineno, parse_errors_e err)
  {
    printf("Command '%s' caused error %d.\n", _int->get_cur_source_line_str(), err);

    // skip faulty line
    if (err == parse_error_function_expected)
      _int->set_cur_src_line(lineno+1);
    return parse_error_none;
  }
};

/*!
 \brief Interactively (command by command) runs the script
*/
static void interpret_interactive(compel_script_t &script)
{
  interpreter_t *interpreter = script.get_interpreter();
  parse_errors_e err = parse_error_none;

  // register this special error handler
  run_interactive_script_error_handler err_handler(interpreter);

  std::string line;
  lines_t lines;

  const char *prompt = script.get_args()->interactive_prompt_str.c_str();

  while (true)
  {
    printf(prompt);

    std::getline(std::cin, line);

    const char *cline = line.c_str();
    if (stricmp(cline, prompt) == 0)
      break;

    size_t n_commands = line_to_lines(cline, lines);
    interpreter->load_from_lines(lines, false);
    size_t curline = interpreter->get_cur_source_lineno();
    size_t n_exec = 0;

    parse_errors_e err = parse_error_none;

    while (err != parse_stop_parsing)
    {
      size_t curline = interpreter->get_cur_source_lineno();

      if (script.get_args()->b_dbgout_script)
        _dbg_out(">line:%d >%s<\n", curline, interpreter->get_cur_source_line_str());

      err = interpreter->interpret_line();
      switch (err)
      {
      case parse_error_line_is_void:
      case parse_error_none:
      case parse_stop_parsing:
      case parse_line_is_empty:
      case parse_error_line_is_comment:
        n_exec++;
        continue;
      default:
        printf(">error: %d\n", err);
        err = parse_stop_parsing;
      }

      if (script.get_args()->b_dbgout_script)
        _dbg_out(">line:%d >%s<\n", curline, line);
    } // script run
  } // while
}

/*!
 Interpret a complete file
*/
void interpret_file(compel_script_t &script)
{
  interpreter_t *interpreter = script.get_interpreter();

  run_script_error_handler err_handler(interpreter);

  char *progfn = (char *) script.get_args()->script_file.c_str();

  parse_errors_e err = parse_error_none;

  bool bLoadOk = interpreter->load_lines_from_file(progfn, true);

  int prepare_failing_line = 0;

  err = interpreter->prepare(prepare_failing_line);

  if (err != parse_error_none)
  {
    printf(">preparse error: %d @ line %d\n", err, prepare_failing_line+1);
    bLoadOk = false;
  }

  while (bLoadOk && err != parse_stop_parsing)
  {
    size_t curline = interpreter->get_cur_source_lineno();

    if (script.get_args()->b_dbgout_script)
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
      printf(">error: %d @ line %d\n", err, curline+1);
      err = parse_stop_parsing;
    }
  }
}

int start(int argc, char *argv[])
{
  compel_script_t script;

  compel_cmdline_options_t &args = *script.get_args();

  parse_arguments(argc, argv, args);

  if (args.script_file.empty() && !args.b_interactive)
  {
    show_usage();
    return -1;
  }

  if (args.b_show_mem_leaks)
  {
    freopen(args.fn_memleaks.c_str(), "w", stderr);

    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_WARN, _CRTDBG_FILE_STDERR);
    _CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_ERROR, _CRTDBG_FILE_STDERR);
    _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR); 
  }

  if (!script.init())
  {
    printf("script engine initialization error!");
    return -1;
  }

  // save old directory
  args.old_working_dir.resize(MAX_PATH, 0);
  getcwd(&args.old_working_dir[0], MAX_PATH);

  if (args.working_dir.empty())
    args.working_dir = args.old_working_dir;

  // go to new working directory
  chdir(args.working_dir.c_str());

  if (!args.script_file.empty())
    interpret_file(script);

  if (args.b_interactive)
    interpret_interactive(script);

  if (args.b_gen_raw_parse)
    save_raw_lines(script.inthelper, (char *) args.fn_raw_parse.c_str());

  if (args.b_dump_symtbl)
    script.inthelper.show_symbols();

  // restore old directory
  chdir(args.old_working_dir.c_str());

  //t1.show_lines();
  //t1.show_symbols();

  return 0;
}

int main(int argc, char *argv[])
{
  _CrtSetDbgFlag(
    _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG)
    | _CRTDBG_LEAK_CHECK_DF
    | _CRTDBG_ALLOC_MEM_DF
    | _CRTDBG_CHECK_ALWAYS_DF);

  int r = start(argc, argv);

  return r;
}