#ifndef __FNC_WIN_10092006__
#define __FNC_WIN_10092006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_win : public function_t
{
public:
  enum op_e
  {
    op_unknown = 0,
    op_getmousexy,
    op_ismousemoved
  };
private:
  op_e _op;
  bool _bUseNameSpace;

  parse_errors_e execute_getmousexy();
  parse_errors_e execute_ismousemoved();

public:
  static const char *fnc_name;
  fnc_win(interpreter_t *, op_e);
  fnc_win(bool bUseNameSpace);
  parse_errors_e execute();
  parse_errors_e register_function(interpreter_t *);
};

#endif