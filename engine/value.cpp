#include "value.h"

void value_t::init()
{
  set_var_kind(variable_value);
  _str_temp[0] = 0;
}


void value_t::assign(const value_t &rhs)
{
  _value = rhs._value;
  init();
}

// assignment operator
value_t &value_t::operator=(const value_t &rhs)
{
  assign(rhs);
  return *this;
}

// copy constructor
value_t::value_t(const value_t &rhs)
{
  assign(rhs);
}

// integer value constructor
value_t::value_t(long val)
{
  init();
  set_int_value(val);
}

// string value constructor
value_t::value_t(const char *p)
{
  init();
  if (p == 0)
    set_str_value("");
  else
    set_str_value(p);
}

//
// --- integer handling
//

// returns integer value
long value_t::get_int_value() const
{
  return compel_string_tokenizer_t::parse_number(_value.c_str());
}

// sets integer value
void value_t::set_int_value(long val)
{
  sprintf(_str_temp, "%ld", val);
  set_str_value(_str_temp);
}

// integer assignment operator
value_t &value_t::operator=(long val)
{
  set_int_value(val);
  return *this;
}

//
// --- string handling
//

// string value assignment
void value_t::set_str_value(const char *p)
{
  _value = std::string(p);
}

// returns string value
// returned in the designated base
const char *value_t::get_str_value(int base)
{
  if (base == 0)
    return _value.c_str();
  else if (base == 10)
    sprintf(_str_temp, "%ld", get_int_value());
  else if (base == 8)
    sprintf(_str_temp, "%lo", get_int_value());
  else if (base == 16)
    sprintf(_str_temp, "0x%lx", get_int_value());

  return _str_temp;
}

// string assignment operator
value_t &value_t::operator=(char *p)
{
  set_str_value(p);
  return *this;
}

