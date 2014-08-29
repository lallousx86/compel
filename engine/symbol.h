#ifndef __SYMBOL__01242006__
#define __SYMBOL__01242006__

#include "fwd.h"

class symbol_t
{
private:
  void assign(const symbol_t &r);

  void operator=(const symbol_t &r);
protected:
  symbol_kind_e _sym_kind;
  int _tag;
public:
  symbol_t(const symbol_t &);
  void set_sym_kind(symbol_kind_e k);
  symbol_kind_e get_sym_kind();
  symbol_t();
  virtual ~symbol_t();
};

#endif