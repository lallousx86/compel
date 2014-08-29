#ifndef __VARIABLE__01242006__
#define __VARIABLE__01242006__

#include "fwd.h"
#include "symbol.h"

class variable_t : public symbol_t
{
private:
  variable_kind_e _var_kind;

public:

  void set_var_kind(variable_kind_e k);

  variable_kind_e get_var_kind();

  variable_t(const variable_t &rhs);

  variable_t();
};

#endif