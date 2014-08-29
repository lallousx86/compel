#include "fnc_memory.h"
#include "../obj/memory_object.h"

static const char 
  *s_str_malloc = "malloc",
  *s_str_mfree = "mfree";

// ==================================================================================
//
//                             fopen
//
// ==================================================================================

const char *fnc_memory::fnc_name = "memory";

fnc_memory::fnc_memory()
{
}

parse_errors_e fnc_memory::register_function(interpreter_t *interpreter)
{
  fnc_memory *functions[] =
  {
    new fnc_memory(interpreter, mo_malloc),
    new fnc_memory(interpreter, mo_mfree)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
    interpreter->add_symbol(functions[i]->_name.c_str(), functions[i]);

  return parse_error_none;
}

fnc_memory::fnc_memory(interpreter_t *interpreter, memory_op op)
{
  _op = op;

  switch (op)
  {
  case mo_malloc:
    set_namedesc(s_str_malloc, "$mobj size <- opens a file");
    set_minmaxargs(2, 3);
    break;
  case mo_mfree:
    set_namedesc(s_str_mfree, "frees memory");
    set_minmaxargs(1, 1);
    break;
  }

  set_interpreter(interpreter);
}

parse_errors_e fnc_memory::execute()
{
  switch (_op)
  {
  case mo_malloc:
    return execute_malloc();
  case mo_mfree:
    return execute_mfree();
  default:
    return parse_error_wrong_syntax;
  }
}

parse_errors_e fnc_memory::execute_malloc()
{
  const int PAR_NAME = 1;
  const int PAR_SIZE = 2;
  const int PAR_FILLER = 3;

  interpreter_t *_int = get_interpreter();

  // get the required size
  long size = compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_SIZE));
  if (size <= 0)
    return parse_stop_parsing;

  // get the variable name, to create
  const char *objname = _int->get_const_at(PAR_NAME);

  if (!_int->is_variable_name(objname))
    return parse_error_variable_expected;

  char filler = _int->get_fnc_arg_count() > PAR_FILLER ? _int->get_const_at(PAR_FILLER)[0] : 0;

  // create an object
  memory_object_t *mobj = new memory_object_t(size);

  // try to alloc memory
  if (!mobj->alloc(filler))
  {
    delete mobj;
    return parse_error_none;
  }

  symbol_t *symref = _int->add_symbol(objname, mobj);
  if (symref == 0)
  {
    delete mobj;
    return parse_error_symbol_redefinition;
  }

  return parse_error_none;
}

parse_errors_e fnc_memory::execute_mfree()
{
  interpreter_t *_int = get_interpreter();

  const char *objname = _int->get_const_at(1);
  if (!_int->is_variable_name(objname))
    return parse_error_variable_expected;

  object_t *obj = _int->get_object_at(1);
  if (obj == 0)
    return parse_error_symbol_not_in_table;
  else if (obj->get_obj_kind() != object_memory)
    return parse_error_object_expected;

  memory_object_t *mobj = static_cast<memory_object_t *>(obj);

  mobj->free();

  _int->remove_symbol(objname);

  return parse_error_none;
}
