#ifndef __FNC_MM_10092006__
#define __FNC_MM_10092006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_mm : public function_t
{
public:
  enum op_e
  {
    op_unknown,
    op_playwave
  };
private:
  op_e _op;
  bool _bUseNameSpace;
  bool _bPlaying;

  parse_errors_e execute_playwave();

public:
  static const char *fnc_name;
  fnc_mm(interpreter_t *, op_e);
  fnc_mm(bool bUseNameSpace);
  parse_errors_e execute();
  parse_errors_e register_function(interpreter_t *);
  virtual ~fnc_mm();
};

#endif