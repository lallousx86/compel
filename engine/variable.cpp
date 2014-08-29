#include "variable.h"

void variable_t::set_var_kind(variable_kind_e k)
{
  _var_kind = k;
}

variable_kind_e variable_t::get_var_kind()
{
  return _var_kind;
}

variable_t::variable_t(const variable_t &rhs) : symbol_t(rhs)
{
  _var_kind = rhs._var_kind;
}

variable_t::variable_t()
{
  set_sym_kind(symbol_variable);
}
