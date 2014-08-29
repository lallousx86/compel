#ifndef __FNCDIRS__05082006__
#define __FNCDIRS__05082006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"
#include "../obj/dirs_object.h"

class fnc_dirs : public function_t
{
public:
  enum op_e
  {
    op_unknown,
    op_enumfiles,
    op_filenstats,
    op_drivestats,
    op_filestats,
    op_chdir,
    op_hasfattr,
    op_mkdir,
    op_getdir,
    op_enumdrives,
    op_rmdir,
    op_direxists,
    op_fileexists,
    op_findfiles
  };
private:
  bool _bUseNameSpace;
  op_e _op;
  void basic_init();

  parse_errors_e execute_chdir();
  parse_errors_e execute_fileexists();
  parse_errors_e execute_direxists();
  parse_errors_e execute_getdir();
  parse_errors_e execute_enumfiles();
  parse_errors_e execute_enumdrives();

  parse_errors_e parse_dobj(int idx, dirs_object_t **dobj);
public:
  static const char *fnc_name;

  fnc_dirs(interpreter_t *, op_e);
  fnc_dirs(bool bUseNameSpace = false);
  parse_errors_e execute();
  parse_errors_e register_function(interpreter_t *);
};

#endif