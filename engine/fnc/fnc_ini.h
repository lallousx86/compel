#ifndef __FNC_INI_10092006__
#define __FNC_INI_10092006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_ini : public function_t
{
public:
  enum op_e
  {
    op_unknown,
    op_readini,
    op_writeini
/*    ,
    op_readq,
    op_writeq,
    op_selectsection
*/
  };
private:
  op_e _op;
  bool _bUseNameSpace;

  parse_errors_e do_rw(bool bRead);
  parse_errors_e do_qrw(bool bRead);

public:
  static const char *fnc_name;
  fnc_ini(interpreter_t *, op_e);
  fnc_ini(bool bUseFullNS);
  parse_errors_e execute();
  parse_errors_e register_function(interpreter_t *);
};

#endif