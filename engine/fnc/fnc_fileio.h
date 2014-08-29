#ifndef  _FNC_FILEIO_01242006__
#define  _FNC_FILEIO_01242006__

#include "../fwd.h"
#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"
#include "../obj/file_object.h"
#include "../obj/memory_object.h"

class fnc_fileio : public function_t
{
public:
  enum fileio_op
  {
    fio_fopen,
    fio_fclose,
    fio_fread,
    fio_fseek,
    fio_fwrite
  };
private:
  bool _bUseNameSpace;
  fileio_op _op;
  parse_errors_e execute_fopen();
  parse_errors_e execute_freadwrite(bool bRead = true);
  parse_errors_e execute_fclose();
  parse_errors_e execute_fseek();

  parse_errors_e parse_mobj(int idx, memory_object_t **mobj);
  parse_errors_e parse_fobj(int idx, file_object_t **fobj);
public:
  fnc_fileio(interpreter_t *, fileio_op op);
  parse_errors_e execute();
  fnc_fileio(bool bUseNameSpace = false);
  parse_errors_e register_function(interpreter_t *);

  static const char *fnc_name;

};


#endif