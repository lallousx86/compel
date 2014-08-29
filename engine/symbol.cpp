#include "symbol.h"
/*
History
----------
02/09/2006 - added a TAG to a given symbol

*/
void symbol_t::set_sym_kind(symbol_kind_e k)
{
  _sym_kind = k;
}

symbol_kind_e symbol_t::get_sym_kind()
{
  return _sym_kind;
}

void symbol_t::assign(const symbol_t &rhs)
{
  _tag = rhs._tag;
  _sym_kind = rhs._sym_kind;
}

symbol_t::symbol_t(const symbol_t &rhs)
{
  assign(rhs);
}

symbol_t::symbol_t()
{
  _tag = 0;
  _sym_kind = symbol_base;
}

symbol_t::~symbol_t() 
{

}