#include "interpreter_helper.h"
#include "object.h"

void interpreter_helper_t::set_tokenizer(compel_string_tokenizer_t *tokenizer)
{
  _int->_slp = tokenizer;
}

interpreter_bind_struct_t *interpreter_helper_t::get_bind_struct()
{
  return &_int->_int_bind;
}

compel_string_tokenizer_t *interpreter_helper_t::get_tokenizer()
{
  return _int->_slp;
}

lines_t &interpreter_helper_t::get_interpreter_lines()
{
  return _int->_lines;
}

void interpreter_helper_t::set_interpreter(interpreter_t *interpreter)
{
  _int = interpreter;
}

interpreter_helper_t::interpreter_helper_t(interpreter_t *interpreter)
{
  set_interpreter(interpreter);
}

void interpreter_helper_t::show_lines()
{
  lines_container_type_t &lines = _int->_lines.get_string_list();

  size_t i=0;
  for (lines_container_type_t::iterator it=lines.begin();
    it != lines.end();
    ++it)
  {
    printf("%0.8d: %s\n", i++, it->c_str());
  }
}

symbol_table_t *interpreter_helper_t::get_interpreter_symtbl()
{
  return _int->_symtbl;
}

void interpreter_helper_t::show_symbols()
{
  strsymbol_map_t &symtbl = _int->_symtbl->get_symtbl_raw();
  for (strsymbol_map_t::iterator it = symtbl.begin(); it != symtbl.end();++it)
  {
    printf("%0.8p: %s\n", it->second, it->first.c_str());
  }
}