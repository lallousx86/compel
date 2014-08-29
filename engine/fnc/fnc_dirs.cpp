#include "fnc_dirs.h"
#include "../parse_util.h"

static const char *s_ns_name = "dirs.";
const char *fnc_dirs::fnc_name = "dirs";
static const char *s_mkdir = "mkdir";
static const char *s_enumfiles = "enumfiles";
static const char *s_chdir = "chdir";
static const char *s_getdir = "getdir";
static const char *s_direxists = "direxists";
static const char *s_fileexists = "fileexists";
static const char *s_hasfattr = "hasfattr";
static const char *s_enumdrives = "enumdrives";

fnc_dirs::fnc_dirs(interpreter_t *interpreter, op_e op)
{
  _op = op;
  set_interpreter(interpreter);

  switch (_op)
  {
  case op_enumfiles:
    set_namedesc(s_enumfiles, "$dobj \"mask\" <- enumerates files and folders with the given mask");
    set_minmaxargs(2, 2);
    break;
  case op_getdir:
    set_namedesc(s_getdir, "$result_var <- returns the current directory");
    set_minmaxargs(1, 1);
    break;
  case op_chdir:
    set_namedesc(s_chdir, "$result_var \"directory_var\" <- changes the current directory");
    set_minmaxargs(2, 2);
    break;
  case op_hasfattr:
    set_namedesc(s_hasfattr, "$attr_var \"result_var\" <- checks if a given attribute has the required file attributes");
    set_minmaxargs(2, 2);
    break;
  case op_direxists:
    set_namedesc(s_direxists, "$result_var \"directory_name\" <- checks if a directory exists or not");
    set_minmaxargs(2, 2);
    break;
  case op_fileexists:
    set_namedesc(s_fileexists, "$result_var \"directory_name\" <- checks if a file exists");
    set_minmaxargs(2, 2);
    break;
  case op_enumdrives:
    set_namedesc(s_enumdrives, "$dobj <- enumerates all logical drives");
    set_minmaxargs(1, 1);
    break;
  }
}

fnc_dirs::fnc_dirs(bool bUseNameSpace)
{
  _bUseNameSpace = bUseNameSpace;
  _op = op_unknown;
}

// This will return the directory object at the given index
parse_errors_e fnc_dirs::parse_dobj(int idx, dirs_object_t **fobj)
{
  interpreter_t *_int = get_interpreter();
  const char *objname = _int->get_const_at(idx);
  if (!_int->is_variable_name(objname))
    return parse_error_variable_expected;

  std::string out;
  variable_t *var = _int->get_var_ref(objname, out);
  object_t *obj;

  if (var == 0)
    obj = _int->get_object(out.c_str());
  else
    obj = static_cast<object_t *>(var);

  if (obj == 0)
    return parse_error_symbol_not_in_table;
  else if (obj->get_obj_kind() != object_dirs)
    return parse_error_object_expected;

  *fobj = static_cast<dirs_object_t *>(obj);

  return parse_error_none;
}

parse_errors_e fnc_dirs::execute_getdir()
{
  const int PAR_RESULT = 1;

  interpreter_t *_int = get_interpreter();

  value_t *val = _int->get_value_at(PAR_RESULT);
  if (val == 0)
    return parse_error_variable_expected;

  // Determine buffer length
  DWORD len = ::GetCurrentDirectoryA(0, NULL);

  // Allocate buffer
  char *buf = new char[len+2];

  // Get current directory
  ::GetCurrentDirectoryA(len, buf);

  std::string s;

  parse_util::escape_std_string(buf, s);

  // Release buffer
  delete [] buf;

  // Pass the directory
  val->set_str_value(s.c_str());

  return parse_error_none;
}

// change directory
parse_errors_e fnc_dirs::execute_chdir()
{
  const int PAR_RESULT = 1;
  const int PAR_DIR = 2;

  interpreter_t *_int = get_interpreter();

  value_t *val = _int->get_value_at(PAR_RESULT);
  if (val == 0)
    return parse_error_variable_expected;

  const char *dir = _int->evaluate_at(PAR_DIR);
  if (dir == 0)
    return parse_error_symbol_type_mismatch;

  val->set_int_value(::SetCurrentDirectoryA(dir) == FALSE ? 0 : 1);

  return parse_error_none;
}

// direxists
parse_errors_e fnc_dirs::execute_direxists()
{
  const int PAR_RESULT = 1;
  const int PAR_NAME = 2;

  interpreter_t *_int = get_interpreter();

  value_t *val = _int->get_value_at(PAR_RESULT);
  if (val == 0)
    return parse_error_variable_expected;

  const char *name = _int->evaluate_at(PAR_NAME);
  if (name == 0)
    return parse_error_symbol_type_mismatch;

  DWORD attr = ::GetFileAttributesA(name);

  val->set_int_value(
    (attr != INVALID_FILE_ATTRIBUTES)
    && 
    ((attr & FILE_ATTRIBUTE_DIRECTORY) == FILE_ATTRIBUTE_DIRECTORY)    
    );

  return parse_error_none;
}

// fileexist checks
parse_errors_e fnc_dirs::execute_fileexists()
{
  const int PAR_RESULT = 1;
  const int PAR_NAME = 2;

  interpreter_t *_int = get_interpreter();

  value_t *val = _int->get_value_at(PAR_RESULT);
  if (val == 0)
    return parse_error_variable_expected;

  const char *name = _int->evaluate_at(PAR_NAME);
  if (name == 0)
    return parse_error_symbol_type_mismatch;

  DWORD attr = ::GetFileAttributesA(name);

  val->set_int_value(
    (attr != INVALID_FILE_ATTRIBUTES)
    && 
    ((attr & FILE_ATTRIBUTE_DIRECTORY) != FILE_ATTRIBUTE_DIRECTORY)    
  );

  return parse_error_none;
}

// enumfiles()
parse_errors_e fnc_dirs::execute_enumfiles()
{
  const int PAR_OBJ = 1;
  const int PAR_MASK = 2;

  interpreter_t *_int = get_interpreter();

  // get the variable name to create
  const char *objname = _int->get_const_at(PAR_OBJ);

  if (!_int->is_variable_name(objname))
    return parse_error_variable_expected;

  const char *mask = _int->evaluate_at(PAR_MASK);

  // create an object
  dirs_object_t *dobj = new dirs_object_t;

  symbol_t *symref = _int->add_symbol(objname, dobj);
  if (symref == 0)
  {
    delete dobj;
    return parse_error_symbol_redefinition;
  }

  dobj->enumfiles(mask);

  return parse_error_none;
}

// enumdrives()
parse_errors_e fnc_dirs::execute_enumdrives()
{
  const int PAR_OBJ = 1;

  interpreter_t *_int = get_interpreter();

  // get the variable name to create
  const char *objname = _int->get_const_at(PAR_OBJ);

  if (!_int->is_variable_name(objname))
    return parse_error_variable_expected;

  // create an object
  dirs_object_t *dobj = new dirs_object_t;

  symbol_t *symref = _int->add_symbol(objname, dobj);
  if (symref == 0)
  {
    delete dobj;
    return parse_error_symbol_redefinition;
  }

  dobj->enumdrives();

  return parse_error_none;
}

parse_errors_e fnc_dirs::execute()
{
  switch (_op)
  {
  case op_enumfiles:
    return execute_enumfiles();
  case op_chdir:
    return execute_chdir();
  case op_getdir:
    return execute_getdir();
  case op_fileexists:
    return execute_fileexists();
  case op_direxists:
    return execute_direxists();
  case op_enumdrives:
    return execute_enumdrives();
  }
  return parse_error_function_expected;
}

parse_errors_e fnc_dirs::register_function(interpreter_t *interpreter)
{
  fnc_dirs *functions[] =
  {
    new fnc_dirs(interpreter, op_enumfiles),
    new fnc_dirs(interpreter, op_enumdrives),
    new fnc_dirs(interpreter, op_direxists),
    new fnc_dirs(interpreter, op_fileexists),
    new fnc_dirs(interpreter, op_chdir),
    new fnc_dirs(interpreter, op_getdir),

    new fnc_dirs(interpreter, op_hasfattr)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
  {
    std::string fnc_name = functions[i]->_name;
    if (_bUseNameSpace)
      fnc_name = s_ns_name + fnc_name;
    interpreter->add_symbol(fnc_name.c_str(), functions[i]);
  }
  return parse_error_none;
}
