#include "memory_object.h"

#pragma warning(disable:4311)
void memory_object_t::basic_init()
{
  set_obj_kind(object_memory);
}

void memory_object_t::init()
{
  _val_size    = insert_attribute("size");
  _val_ptr     = insert_attribute("ptr");
  _val_length  = insert_attribute("length"); // length of the string that was passed from from_string

  basic_init();
}

void memory_object_t::dispose()
{
  if (--*_refcount == 0)
  {
    free();
    delete _refcount;
  }
}

memory_object_t::memory_object_t(size_t size)
{
  init();

  _size = size;
  _ptr = 0;

  _val_size->set_int_value((long)_size);
  _val_length->set_int_value((long)_size);
  _val_ptr->set_int_value(0);

  _refcount = new size_t(1);
}

bool memory_object_t::alloc(char filler)
{
  try
  {
    _ptr = new char [_size];

    if (_ptr == 0)
      return false;

    memset(_ptr, filler, _size);

    _val_ptr->set_int_value(reinterpret_cast<long>(_ptr));

    return true;
  }
  catch (...)
  {
    return false;
  }
}

char *memory_object_t::get_ptr() const
{
  return _ptr;
}

const size_t memory_object_t::get_size() const
{
  return _size;
}

memory_object_t::memory_object_t(const memory_object_t &rhs) : object_t(rhs)
{
  basic_init();

  _size = rhs._size;
  _ptr = rhs._ptr;

  // increment reference
  _refcount = rhs._refcount;
  ++*_refcount;
}

void memory_object_t::free()
{
  if (_ptr == 0)
    return;
  delete [] _ptr;
  _ptr = 0;
}

memory_object_t::~memory_object_t()
{
  dispose();
}

void memory_object_t::to_string(std::string &out)
{
  if (_ptr == 0)
  {
    out = "(null)";
    return;
  }
  else
  {
    out.assign(_ptr, _size);
  }
}

void memory_object_t::from_string(std::string &from)
{
  if (_ptr == 0)
    return;

  size_t sz = from.size();

  _val_length->set_int_value((long)sz);

  if (sz == 0)
  {
    memset(_ptr, 0, _size);
    return;
  }
  else if (_size < sz)
    sz = _size;
  memcpy(_ptr, from.c_str(), sz+1);
}