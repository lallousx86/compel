#ifndef __VALUE_01222006__
#define __VALUE_01222006__

#include "compel_string_tokenizer.h"
#include "variable.h"

class value_t : public variable_t
{
private:
  std::string _value;
  char _str_temp[30];

  void init();
  void assign(const value_t &rhs);
public:

  //
  // --- constructors
  //

  value_t(const value_t &rhs);
  value_t &operator=(const value_t &rhs);
  value_t(long val);
  value_t(const char *p = 0);

  //
  // --- integer handling
  //

  long get_int_value() const;

  void set_int_value(long val);

  value_t &operator=(long val);

  //
  // --- string handling
  //

  void set_str_value(const char *p);

  const char *get_str_value(int base = 0);

  value_t &operator=(char *p);
};

#endif