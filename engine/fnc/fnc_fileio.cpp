#include "fnc_fileio.h"
#include <algorithm>

const static char *s_ns_name = "fio.";
const static char *s_whence_here = "here";
const static char *s_whence_begin = "begin";
const static char *s_whence_end = "end";

const char *fnc_fileio::fnc_name = "fileio";

// ==================================================================================
//
//                             file i/o
//
// ==================================================================================
parse_errors_e fnc_fileio::register_function(interpreter_t *interpreter)
{
  fnc_fileio *functions[] =
  {
    new fnc_fileio(interpreter, fio_fopen),
    new fnc_fileio(interpreter, fio_fclose),
    new fnc_fileio(interpreter, fio_fread),
    new fnc_fileio(interpreter, fio_fseek),
    new fnc_fileio(interpreter, fio_fwrite)
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

fnc_fileio::fnc_fileio(bool bUseNameSpace)
{
  _bUseNameSpace = bUseNameSpace;
}

fnc_fileio::fnc_fileio(interpreter_t *interpreter, fileio_op op)
{
  _op = op;

  switch (op)
  {
  case fio_fopen:
    set_namedesc("fopen", "$fobj \"file_name\" \"flags\" <- opens a file");
    set_minmaxargs(2, 3);
    break;
  case fio_fread:
    set_namedesc("fread", "read from a file");
    set_minmaxargs(2, 3);
    break;
  case fio_fwrite:
    set_namedesc("fwrite", "write to a file");
    set_minmaxargs(2, 3);
    break;
  case fio_fclose:
    set_namedesc("fclose", "closes a file object");
    set_minmaxargs(1, 1);
    break;
  case fio_fseek:
    set_namedesc("fseek", "seeks in a file");
    set_minmaxargs(2, 3);
    break;
  }

  set_interpreter(interpreter);
}

parse_errors_e fnc_fileio::execute()
{
  switch (_op)
  {
  case fio_fopen:
    return execute_fopen();
  case fio_fclose:
    return execute_fclose();
  case fio_fread:
    return execute_freadwrite(true);
  case fio_fwrite:
    return execute_freadwrite(false);
  case fio_fseek:
    return execute_fseek();
  default:
    return parse_error_wrong_syntax;
  }
}

// This will return the file object at the given index
parse_errors_e fnc_fileio::parse_fobj(int idx, file_object_t **fobj)
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
  else if (obj->get_obj_kind() != object_file)
    return parse_error_object_expected;

  *fobj = static_cast<file_object_t *>(obj);

  return parse_error_none;
}

// This will return the file object at the given index
parse_errors_e fnc_fileio::parse_mobj(int idx, memory_object_t **mobj)
{
  interpreter_t *_int = get_interpreter();

  const char *objname = _int->get_const_at(idx);
  if (!_int->is_variable_name(objname))
    return parse_error_variable_expected;

  object_t *obj = _int->get_object_at(idx);
  if (obj == 0)
    return parse_error_symbol_not_in_table;
  else if (obj->get_obj_kind() != object_memory)
    return parse_error_object_expected;

  *mobj = static_cast<memory_object_t *>(obj);

  return parse_error_none;
}

// fseek()
parse_errors_e fnc_fileio::execute_fseek()
{
  const int PAR_FOBJ = 1;
  const int PAR_OFFSET = 2;
  const int PAR_WHENCE = 3;
  interpreter_t *_int = get_interpreter();

  // get the variable name, to create
  const char *fobjname = _int->get_const_at(PAR_FOBJ);

  file_object_t *fobj;
  parse_errors_e err;

  if ((err = parse_fobj(PAR_FOBJ, &fobj)) != parse_error_none)
    return err;

  long offset = compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_OFFSET));

  // get file open mode, or use default if not passed
  const char *whence = _int->get_fnc_arg_count() > PAR_WHENCE ? _int->get_const_at(PAR_WHENCE) : s_whence_here;

  int nWhence;
  if (stricmp(whence, s_whence_begin) == 0)
    nWhence = SEEK_SET;
  else if (stricmp(whence, s_whence_end) == 0)
    nWhence = SEEK_END;
  else 
    nWhence = SEEK_CUR;

  fobj->seek(offset, nWhence);

  return parse_error_none;
}

// fopen()
parse_errors_e fnc_fileio::execute_fopen()
{
  const int PAR_FOBJ = 1;
  const int PAR_FNAME = 2;
  const int PAR_OFLAG = 3;
  interpreter_t *_int = get_interpreter();

  // get the variable name, to create
  const char *fobjname = _int->get_const_at(PAR_FOBJ);

  if (!_int->is_variable_name(fobjname))
    return parse_error_variable_expected;

  // get file open mode, or use default if not passed
  const char *openflags = _int->get_fnc_arg_count() > PAR_OFLAG ? _int->get_const_at(PAR_OFLAG) : "rb";

  const char *filename = _int->evaluate_at(PAR_FNAME);

  // create an object
  file_object_t *fobj = new file_object_t(filename, openflags);

  // try to open the file
  if (!fobj->open())
  {
    delete fobj;
    return parse_error_none;
  }

  symbol_t *symref = _int->add_symbol(fobjname, fobj);
  if (symref == 0)
  {
    delete fobj;
    return parse_error_symbol_redefinition;
  }

  return parse_error_none;
}

// fclose()
parse_errors_e fnc_fileio::execute_fclose()
{
  const int PAR_FOBJ = 1;

  interpreter_t *_int = get_interpreter();

  const char *fobjname = _int->get_const_at(PAR_FOBJ);
  if (!_int->is_variable_name(fobjname))
    return parse_error_variable_expected;

  object_t *obj = _int->get_object_at(PAR_FOBJ);
  if (obj == 0)
    return parse_error_symbol_not_in_table;
  else if (obj->get_obj_kind() != object_file)
    return parse_error_object_expected;

  file_object_t *fobj = dynamic_cast<file_object_t *>(obj);

  fobj->close();

  _int->remove_symbol(fobjname);

  return parse_error_none;
}


parse_errors_e fnc_fileio::execute_freadwrite(bool bRead)
{
  const int PAR_FOBJ = 1;
  const int PAR_DATA = 2;
  const int PAR_LEN  = 3;

  interpreter_t *_int = get_interpreter();

  using namespace std;

  parse_errors_e err;

  file_object_t *fobj;
  if ((err = parse_fobj(PAR_FOBJ, &fobj)) != parse_error_none)
    return err;

  size_t sz;

  // if no size parameter, then we assume ZERO
  // then we compute later
  if (_int->get_fnc_arg_count() > PAR_LEN)
    sz = compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_LEN));
  else
    sz = 0;

  // read data: [ ] string [x] mobj
  // write data: [x] string [x] mobj

  memory_object_t *mobj(0);
  if (
    // no memory object?
    ((err = parse_mobj(PAR_DATA, &mobj)) != parse_error_none)
    // reading?
    && bRead
    )
  {
    return parse_error_object_expected;
  }

  // i/o to/from a memory object
  if (mobj)
  {
    if (sz == 0)
      sz = mobj->get_size();
    else
      sz = _MIN(mobj->get_size(), sz);

    if (bRead)
      fobj->read(mobj->get_ptr(), sz);
    else
      fobj->write(mobj->get_ptr(), sz);
  }
  // write from a string
  else
  {
    std::string str;
    compel_string_tokenizer_t::unescape_c_string(_int->get_const_at(PAR_DATA), str);

    if (sz == 0)
      sz = str.size();

    fobj->write((void *)str.c_str(), sz);
  }

  return err;
}