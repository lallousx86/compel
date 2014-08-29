#include "symbol_table.h"

#define SYNCHRONIZED_BLOCK mutex_scope_protect_class mu(_mutex)

symbol_table_t::symbol_table_t()
{
  _mutex.create();
}

void symbol_table_t::reference_sym(symbol_t *sym)
{
#ifdef COMPEL_DBG_1
  printf("reference_sym: %p\n", sym);
#endif
  sym_count_t::iterator it = _symcount.find(sym);
  if (it == _symcount.end())
  {
    sym_count_t::_Pairib ib = _symcount.insert(sym_count_t::value_type(sym, 1));
    return;
  }
  it->second++;
}

void symbol_table_t::dereference_sym(symbol_t *sym)
{
  sym_count_t::iterator it = _symcount.find(sym);
  if (it == _symcount.end())
    return;

#ifdef COMPEL_DBG_1
  printf("dereference_sym: %p\n", sym);
#endif
  if (--(it->second) == 0)
  {
    _symcount.erase(it);
    delete sym;
  }
}
strsymbol_map_t &symbol_table_t::get_symtbl_raw()
{
  return _symbols;
}

symbol_table_t::~symbol_table_t()
{
  clear_table();
  _mutex.destroy();
}

symbol_t *symbol_table_t::find_symbol(const char *symname)
{
  strsymbol_map_t::iterator it = _symbols.find(symname);
  if (it == _symbols.end())
    return 0;
  else
    return it->second;
}

//
// adds a symbol to the table, if symbol already exists it fails
//
symbol_t *symbol_table_t::add_symbol(const char *symname, symbol_t *sym)
{
  SYNCHRONIZED_BLOCK;
  if (find_symbol(symname) != 0)
  {
    _err = parse_error_symbol_redefinition;
    return 0;
  }

  _err = parse_error_none;

  strsymbol_map_t::_Pairib ib = _symbols.insert(
    strsymbol_map_t::value_type(symname, sym));

#ifdef COMPEL_DBG_1
  printf("addsym: %s %p\n", symname, sym);
#endif
  // increment reference
  reference_sym(sym);

  return ib.first->second;
}

bool symbol_table_t::remove_symbol(const char *symname)
{
  SYNCHRONIZED_BLOCK;
  strsymbol_map_t::iterator it = _symbols.find(symname);
  if (it == _symbols.end())
  {
    _err = parse_error_symbol_not_in_table;
    return false;
  }
  _err = parse_error_none;

  dereference_sym(it->second);

  _symbols.erase(it);
  return true;
}

void symbol_table_t::clear_table()
{
  SYNCHRONIZED_BLOCK;
  for (strsymbol_map_t::iterator it = _symbols.begin(); it != _symbols.end(); ++it)
  {
    dereference_sym(it->second);
  }
  _symbols.clear();
  _symcount.clear();
}