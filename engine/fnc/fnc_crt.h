#ifndef __FNC_CRT__03132006__INC__
#define __FNC_CRT__03132006__INC__

#include "../fwd.h"
#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_crt : public function_t
{
public:
  enum crt_operation
  {
    co_none,
    co_echo,
    co_msgbox,
    co_dbgout,
    co_inputbox,
    co_delay,
    co_getch,
    co_gotoxy,
    co_textattr,
    co_echoxy,
    co_clrscr,
    co_inputline,
    co_echoln,
    co_yesnobox,
    co_getxy
//    et_logfile
  };
private:
  crt_operation _op;
  bool _b_use_namespace;

  parse_errors_e execute_inputbox();
  parse_errors_e execute_yesnobox();
  parse_errors_e execute_inputline();
  parse_errors_e execute_getch();
  parse_errors_e execute_outputs();
  parse_errors_e execute_delay();
  parse_errors_e execute_echoxy();
  parse_errors_e execute_gotoxy();
  parse_errors_e execute_getxy();
  parse_errors_e execute_textattr();
  parse_errors_e execute_clrscr();

public:
  fnc_crt(interpreter_t *, crt_operation op);
  
  fnc_crt(bool b_use_namespace);

  parse_errors_e register_function(interpreter_t *interpreter);

  parse_errors_e execute();

  static const char *fnc_name;
};

#endif