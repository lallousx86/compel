#ifndef __OBJECT_01222006__
#define __OBJECT_01222006__

#include "fwd.h"
#include "variable.h"
#include "value.h"

class object_t : public variable_t
{
private:
  strvalue_map_t _attributes;
  object_kind_e _obj_kind;
  value_t _null_value;

  void init();

protected:

  object_t &assign(const object_t &);

public:

  void set_obj_kind(object_kind_e obj_kind);

  object_kind_e get_obj_kind();

  value_t *insert_attribute(const char *attribute);

  value_t *insert_attribute(const char *attribute, value_t &val);

  bool remove_attribute(const char *attribute);

  value_t *find_attribute(const char *attribute);

  value_t *value(const char *attribute);

  object_t();

  object_t(const object_t &);
  object_t &operator=(const object_t &);

  void clear_attributes();
  const size_t attributes_count() const;

  virtual void to_string(std::string &);
  virtual void from_string(std::string &);
  virtual value_t &operator[](const char *attribute);
};

#endif