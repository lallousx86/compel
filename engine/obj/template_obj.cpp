#include "dirs_object.h"

void dirs_object_t::basic_init()
{
  set_obj_kind(object_dirs);
}

void dirs_object_t::init()
{
  basic_init();
}

dirs_object_t::dirs_object_t(size_t size)
{
  init();
}

dirs_object_t::dirs_object_t(const dirs_object_t &rhs) : object_t(rhs)
{
  basic_init();
}

dirs_object_t::~dirs_object_t()
{
}

void dirs_object_t::to_string(std::string &out)
{
}

void dirs_object_t::from_string(std::string &from)
{
}