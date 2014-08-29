#include "object.h"
#include <sstream>

object_t &object_t::operator=(const object_t &rhs)
{
  return assign(rhs);
}

void object_t::clear_attributes()
{
  _attributes.clear();
}

const size_t object_t::attributes_count() const
{
  return _attributes.size();
}

object_t &object_t::assign(const object_t &rhs)
{
  init();
  _obj_kind = rhs._obj_kind;

  // clear old attributes
  _attributes.clear();

  /*
  // manual copy method
  for (strvalue_map_t::const_iterator it=rhs._attributes.begin();
    it != rhs._attributes.end();
    ++it)
  {
    insert_attribute(it->first.c_str(), value_t(it->second));
  }
  */

  // copy _attributes using STL
  _attributes.insert(rhs._attributes.begin(), rhs._attributes.end());
  return *this;
}

void object_t::init()
{
  _obj_kind = object_base;
  set_var_kind(variable_object);
}

void object_t::set_obj_kind(object_kind_e obj_kind)
{
  _obj_kind = obj_kind;
}

object_kind_e object_t::get_obj_kind()
{
  return _obj_kind;
}

// inserts an attribute and returns reference to the newly inserted value
value_t *object_t::insert_attribute(const char *attribute)
{
  return insert_attribute(attribute, _null_value);
}

// inserts an attribute and returns reference to the newly inserted value
// if item already exists, then it is updated
value_t *object_t::insert_attribute(const char *attribute, value_t &val)
{
  strvalue_map_t::_Pairib ib;
  
  ib.first = _attributes.find(attribute);
  if (ib.first == _attributes.end())
  {
    // insert new value
    ib = _attributes.insert(
      strvalue_map_t::value_type(std::string(attribute), val));
  }
  else
  {
    // update the value
    ib.first->second = val;
  }
  return &ib.first->second;
}

// an object, when converted to string would return the value of all its attributes
// and its in-memory address
void object_t::to_string(std::string &out)
{
  std::ostringstream ostr;

  size_t cnt = _attributes.size();
  
  ostr << "{";

  for (strvalue_map_t::iterator it=_attributes.begin(); it != _attributes.end(); ++it)
  {
    ostr << it->first << ":" << "\"" << it->second.get_str_value() << "\"";
    if (--cnt)
      ostr << ", ";
  }
  ostr << "}";
  out = ostr.str();
}

// removes a given attribute
// it will return 'false' if attribute was not found
bool object_t::remove_attribute(const char *attribute)
{
  strvalue_map_t::iterator it = _attributes.find(attribute);
  if (it == _attributes.end())
    return false;

  _attributes.erase(it);
  return true;
}

// returns a pointer to the given attribute's value
value_t *object_t::find_attribute(const char *attribute)
{
  strvalue_map_t::iterator it = _attributes.find(attribute);
  if (it == _attributes.end())
    return 0;
  else
    return &it->second;
}

// returns a value from the name
// if name not found an empty value is returned
// do not call this function unless you are sure that attribute exists
value_t *object_t::value(const char *attribute)
{
  strvalue_map_t::iterator it = _attributes.find(attribute);
  if (it == _attributes.end())
    return &_null_value;
  return &it->second;
}

// default constructor
object_t::object_t()
{
  init();
}

// copy constructor
object_t::object_t(const object_t &rhs)
{
  assign(rhs);
}


value_t &object_t::operator[](const char *attribute)
{
  return *value(attribute);
}

void object_t::from_string(std::string &from)
{
  // ;!
  // should parse the string
  // and construct attributes and values....
}