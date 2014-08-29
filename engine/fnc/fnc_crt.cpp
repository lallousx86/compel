#include <windows.h>
#include <stdio.h>
#include <conio.h>
#include "fnc_crt.h"
#include "../function_helper.h"
#include "../lib/Win32InputBox.h"

// Prototype from crt\src\internal.h
//extern void __cdecl __initconin(void);         /* initcon.c */
//extern void __cdecl __initconout(void);        /* initcon.c */
extern "C" intptr_t _coninpfh;

const static char 
  *s_crt_ns_name    = "crt.",
  *s_cmd_echo       = "echo",
  *s_cmd_echoln     = "echoln",
  *s_cmd_msgbox     = "msgbox",
  *s_cmd_yesnobox   = "yesnobox",
  *s_cmd_inputbox   = "inputbox",
  *s_cmd_clrscr     = "clrscr",
	*s_cmd_inputline  = "inputline",
  *s_cmd_dbgout     = "dbgout",
  *s_cmd_getch      = "getch",
  *s_cmd_delay      = "delay",
  *s_cmd_textattr   = "textattr",
  *s_cmd_echoxy     = "echoxy",
  *s_cmd_gotoxy     = "gotoxy",
  *s_cmd_getxy      = "getxy";

const char *fnc_crt::fnc_name = "crt";

static bool initialize_console()
{
  // Allocate a console
  // - Fails if console already created
  // - Create a new one if non exist
  if (!::AllocConsole())
    return true;

  // We created the console
  ::SetConsoleTitle("COMPEL - CONSOLE");

  /*
  // method #1 - for STDOUT
  int hCrt = _open_osfhandle((intptr_t)GetStdHandle(STD_OUTPUT_HANDLE), _O_TEXT);
  FILE * hf = _fdopen(hCrt, "w");
  *stdout = *hf;
  setvbuf( stdout, NULL, _IONBF, 0 );   
  */

  // Adjust stdin/stdout
  freopen("CONOUT$", "w", stdout); 
  freopen("CONIN$", "r", stdin);

	return true;
}

static void console_cls(HANDLE hConsole)
{
  if (!initialize_console())
    return;

  COORD coordScreen = { 0, 0 };    // home for the cursor 
  DWORD cCharsWritten;
  CONSOLE_SCREEN_BUFFER_INFO csbi; 
  DWORD dwConSize;

  // Get the number of character cells in the current buffer. 

  if (!GetConsoleScreenBufferInfo( hConsole, &csbi))
    return;

  dwConSize = csbi.dwSize.X * csbi.dwSize.Y;

  // Fill the entire screen with blanks.
  if (!FillConsoleOutputCharacter( hConsole, (TCHAR)' ',
    dwConSize, coordScreen, &cCharsWritten ))
  {
    return;
  }

  // Get the current text attribute.
  if (!GetConsoleScreenBufferInfo( hConsole, &csbi))
    return;

  // Set the buffer's attributes accordingly.
  if( !FillConsoleOutputAttribute( hConsole, csbi.wAttributes,
    dwConSize, coordScreen, &cCharsWritten ))
    return;

  // Put the cursor at its home coordinates.
  SetConsoleCursorPosition( hConsole, coordScreen );
}

// from getch.c
// int __cdecl _getch_nolock()
static int console_getch(void)
{
	if (!initialize_console())
		return 0;

  if ( _coninpfh != -1 && _coninpfh != -2 ) 
  {
    CloseHandle( (HANDLE)_coninpfh );
    _coninpfh = -2;
  }
  return ::getch();
//	return my_getch();
}

static bool getxy(SHORT &x, SHORT &y)
{
  if (!initialize_console())
    return false;

  HANDLE hOuput = GetStdHandle(STD_OUTPUT_HANDLE);
  if (hOuput == 0)
    return false;

  CONSOLE_SCREEN_BUFFER_INFO info;
  ::GetConsoleScreenBufferInfo(hOuput, &info);

  x = info.dwCursorPosition.X + 1;
  y = info.dwCursorPosition.Y + 1;

  return true;
}

static bool gotoxy(SHORT x, SHORT y)
{
  COORD scrn;    

  if (!initialize_console())
    return false;

  HANDLE hOuput = GetStdHandle(STD_OUTPUT_HANDLE);

  if (hOuput == 0)
    return false;

  scrn.X = x;
  scrn.Y = y;
  SetConsoleCursorPosition(hOuput,scrn);

  return true;
}

fnc_crt::fnc_crt(interpreter_t *interpreter, crt_operation op)
{
  _op = op;

  switch (_op)
  {
  case co_clrscr:
    set_namedesc(s_cmd_clrscr, " <- clears the screen");
    set_minmaxargs(0, 0);
    break;
  case co_echo:
    set_namedesc(s_cmd_echo, "(text|var)+ <- prints a series of text to the screen and returns to a new line");
    set_minmaxargs(1, 0);
    break;
  case co_echoln:
    set_namedesc(s_cmd_echoln, "(t1ext|var)+ <- prints a series of text to the screen");
    set_minmaxargs(0, 0);
    break;
  case co_msgbox:
    set_namedesc(s_cmd_msgbox, "(text|var)+ <- prints a series of text as a msg box");
    set_minmaxargs(1, 0);
    break;
  case co_inputbox:
    set_namedesc(s_cmd_inputbox, "$result_var [prompt] [title] <- toggles an input box with a given prompt and title");
    set_minmaxargs(1, 0);
    break;
  case co_yesnobox:
    set_namedesc(s_cmd_yesnobox, "$result_var prompt [title] <- toggles an input box with a yes/no buttons");
    set_minmaxargs(2, 0);
    break;
  case co_delay:
    set_namedesc(s_cmd_delay, "(msecs) <- delays the execution for a given number of milliseconds");
    set_minmaxargs(1, 1);
    break;
  case co_getch:
    set_namedesc(s_cmd_getch, "[$result_var] <- reads one character from the console");
    set_minmaxargs(0, 0);
    break;
  case co_inputline:
    set_namedesc(s_cmd_inputline, "$result_var <- reads a string from the console");
    set_minmaxargs(1, 0);
    break;
  case co_dbgout:
    set_namedesc(s_cmd_dbgout, "(text|var)+ <- prints a series of text as a msg box");
    set_minmaxargs(1, 0);
    break;
  case co_textattr:
    set_namedesc(s_cmd_textattr, "COLOR <- set the console screen text color");
    set_minmaxargs(1, 1);
    break;
  case co_echoxy:
    set_namedesc(s_cmd_echoxy, "echo X Y message <- moves the cursor to a given location and echos a message");
    set_minmaxargs(3, 0);
    break;
  case co_gotoxy:
    set_namedesc(s_cmd_gotoxy, "gotoxy X Y <- moves the cursor to a given location");
    set_minmaxargs(2, 2);
    break;
  case co_getxy:
    set_namedesc(s_cmd_getxy, "$X $Y <- returns the cursor's position");
    set_minmaxargs(2, 2);
    break;
  }

  set_interpreter(interpreter);
}

fnc_crt::fnc_crt(bool b_use_namespace)
{
  _b_use_namespace = b_use_namespace;
}

parse_errors_e fnc_crt::register_function(interpreter_t *interpreter)
{
  fnc_crt *functions[] =
  {
    new fnc_crt(interpreter, co_echo),
    new fnc_crt(interpreter, co_echoxy),
    new fnc_crt(interpreter, co_msgbox),
    new fnc_crt(interpreter, co_inputbox),
    new fnc_crt(interpreter, co_dbgout),
    new fnc_crt(interpreter, co_inputline),
    new fnc_crt(interpreter, co_getch),
    new fnc_crt(interpreter, co_gotoxy),
    new fnc_crt(interpreter, co_textattr),
    new fnc_crt(interpreter, co_clrscr),
    new fnc_crt(interpreter, co_echoln),
    new fnc_crt(interpreter, co_yesnobox),
    new fnc_crt(interpreter, co_getxy),
    new fnc_crt(interpreter, co_delay)
//    new fnc_crt(interpreter, et_logfile)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
  {
    std::string fnc_name = functions[i]->_name;
    if (_b_use_namespace)
      fnc_name = s_crt_ns_name + fnc_name;
    interpreter->add_symbol(fnc_name.c_str(), functions[i]);
  }
  return parse_error_none;
}

parse_errors_e fnc_crt::execute_inputline()
{
  static const int PAR_RESULT = 1;

  // get the 'result' variable
  interpreter_t *_int = get_interpreter();
  value_t *var_result = _int->get_value_at(PAR_RESULT);
  if (var_result == 0)
    return parse_error_symbol_not_in_table;

  char szResult[1024] = {0};
  gets(szResult);
  var_result->set_str_value(szResult);

  return parse_error_none;
}

parse_errors_e fnc_crt::execute_gotoxy()
{
  interpreter_t *_int = get_interpreter();

  static const int PAR_X = 1;
  static const int PAR_Y = 2;

  ::gotoxy( 
    (SHORT) compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_X)) - 1,
    (SHORT) compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_Y)) - 1
  );

  return parse_error_none;
}

parse_errors_e fnc_crt::execute_echoxy()
{
  interpreter_t *_int = get_interpreter();

  static const int PAR_X = 1;
  static const int PAR_Y = 2;
  static const int PAR_MSG = 3;

  SHORT x, y;

  ::getxy(x, y);

  ::gotoxy( 
    (SHORT) compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_X)) - 1,
    (SHORT) compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_Y)) - 1
    );

  std::string out;
  function_helper::eval_from_to(_int, PAR_MSG, _int->get_fnc_arg_count(), out, false, 0);

  const char *msg = out.c_str();

  printf(msg);

  ::gotoxy(x, y);

  return parse_error_none;
}

parse_errors_e fnc_crt::execute_getxy()
{
  static const int PAR_X = 1;
  static const int PAR_Y = 2;

  interpreter_t *_int = get_interpreter();

  value_t *val;
  SHORT x, y;

  ::getxy(x, y);

  if ( (val = _int->get_value_at(PAR_X)) == 0)
    return parse_error_variable_expected;
  val->set_int_value(x);

  if ( (val = _int->get_value_at(PAR_Y)) == 0)
    return parse_error_variable_expected;

  val->set_int_value(y);

  return parse_error_none;
}

parse_errors_e fnc_crt::execute_inputbox()
{
  static const int PAR_RESULT = 1;
  static const int PAR_PROMPT = 2;
  static const int PAR_TITLE  = 3;

  // get the 'result' variable
  interpreter_t *_int = get_interpreter();
  value_t *var_result = _int->get_value_at(PAR_RESULT);
  if (var_result == 0)
    return parse_error_symbol_not_in_table;

  char szResult[1024] = {0};
  DWORD nResult = sizeof(szResult)/sizeof(szResult[0]);

  const char *szTitle, *szPrompt;

  // pass default value
  strncpy(szResult, var_result->get_str_value(), nResult);
  
  // set title and prompt
  szTitle = _int->get_fnc_arg_count() >= 4 ? _int->get_const_at(PAR_TITLE) : "COMPEL";
  szPrompt = _int->get_fnc_arg_count() >= 3 ? _int->get_const_at(PAR_PROMPT) : "Enter value";

  INT_PTR r = CWin32InputBox::InputBox(szTitle, szPrompt, szResult, nResult);

  if (r == IDCANCEL)
    strcpy(szResult, "");

  var_result->set_str_value(szResult);

  return parse_error_none;
}

parse_errors_e fnc_crt::execute_yesnobox()
{
  static const int PAR_RESULT = 1;
  static const int PAR_PROMPT = 2;
  static const int PAR_TITLE  = 3;

  // get the 'result' variable
  interpreter_t *_int = get_interpreter();
  value_t *var_result = _int->get_value_at(PAR_RESULT);
  if (var_result == 0)
    return parse_error_symbol_not_in_table;

  const char *szTitle, *szPrompt;

  // Parse title and prompt
  szPrompt = _int->get_const_at(PAR_PROMPT);
  szTitle = _int->get_fnc_arg_count() >= 4 ? _int->get_const_at(PAR_TITLE) : "COMPEL";

  INT_PTR r = ::MessageBoxA(0, szPrompt, szTitle, MB_YESNO | MB_ICONQUESTION);

  var_result->set_int_value(r == IDYES);

  return parse_error_none;
}

parse_errors_e fnc_crt::execute_outputs()
{
  interpreter_t *_int = get_interpreter();
  std::string out;

	if ((_op == co_echoln) && _int->get_fnc_arg_count() < 2)
		out = "";
	else
    function_helper::eval_from_to(_int, 1, _int->get_fnc_arg_count(), out, false, 0);

  const char *msg = out.c_str();

  switch (_op)
  {
  case co_echo:
    initialize_console();
    printf(msg);
    break;
  case co_echoln:
    initialize_console();
    printf("%s\n", msg);
    break;
  case co_msgbox:
    MessageBoxA(0, msg, "COMPEL", MB_OK | MB_ICONINFORMATION);
    break;
  case co_dbgout:
    OutputDebugStringA(msg);
    break;
  }
  return parse_error_none;
}

parse_errors_e fnc_crt::execute_delay()
{
  static const int PAR_MS = 1;

  ::Sleep(compel_string_tokenizer_t::parse_number(get_interpreter()->evaluate_at(PAR_MS)));

  return parse_error_none;
}

parse_errors_e fnc_crt::execute_getch()
{
  static const int PAR_RESULT = 1;

  char str[2] = {0};
  str[0] = console_getch();

  interpreter_t *_int = get_interpreter();
  if (_int->get_fnc_arg_count() <= PAR_RESULT)
    return parse_error_none;

  // get the 'result' variable
  value_t *var_result = _int->get_value_at(PAR_RESULT);
  if (var_result == 0)
    return parse_error_symbol_not_in_table;

  var_result->set_str_value(str);

  return parse_error_none;
}

parse_errors_e fnc_crt::execute()
{
  switch (_op)
  {
  case co_inputbox:
    return execute_inputbox();
  case co_inputline:
    return execute_inputline();
  case co_getch:
    return execute_getch();
  case co_delay:
    return execute_delay();
  case co_echoxy:
    return execute_echoxy();
  case co_gotoxy:
    return execute_gotoxy();
  case co_textattr:
    return execute_textattr();
  case co_clrscr:
    return execute_clrscr();
  case co_getxy:
    return execute_getxy();
  case co_yesnobox:
    return execute_yesnobox();
  case co_echo:
  case co_echoln:
  case co_msgbox:
  case co_dbgout:
    return execute_outputs();
  }
  return parse_error_function_expected;
}

parse_errors_e fnc_crt::execute_clrscr()
{  
  console_cls(GetStdHandle(STD_OUTPUT_HANDLE));

  return parse_error_none;
}

parse_errors_e fnc_crt::execute_textattr()
{
  SetConsoleTextAttribute(
    GetStdHandle(STD_OUTPUT_HANDLE), 
    (WORD) compel_string_tokenizer_t::parse_number(get_interpreter()->evaluate_at(1))
  );
  return parse_error_none;
}