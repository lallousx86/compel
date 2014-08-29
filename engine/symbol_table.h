#ifndef __SYMBOLTABLE__01232006__
#define __SYMBOLTABLE__01232006__

#include "symbol.h"
#include "variable.h"
#include "function.h"

#include "lib\synch_class.h"

class symbol_table_t
{
private:
  typedef std::map<symbol_t *, size_t> sym_count_t;

  strsymbol_map_t _symbols;
  parse_errors_e  _err;
  sym_count_t     _symcount;

  mutex_class   _mutex;

  void          reference_sym(symbol_t *sym);
  void          dereference_sym(symbol_t *sym);
public:

  virtual       ~symbol_table_t();
  symbol_table_t();

  symbol_t      *find_symbol(const char *symname);

  symbol_t      *add_symbol(const char *symname, symbol_t *sym);

  bool           remove_symbol(const char *symname);

  void           clear_table();

  strsymbol_map_t &get_symtbl_raw();
};

#endif