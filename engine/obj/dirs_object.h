#ifndef __DIRS_05082006__
#define __DIRS_05082006__

#include "../object.h"
#include <windows.h>

class dirs_object_t : public object_t
{
private:
  value_t *_attr_ok;
  typedef std::vector<WIN32_FIND_DATAA> findata_vector_t;

  enum op_e
  {
    op_unknown,
    op_enumfiles,
    op_enumdrives,
    op_filenstats,
    op_dirnstats
  };

  op_e _op;

  findata_vector_t _dirs_vect, _files_vect;
  WIN32_FIND_DATAA *_last_fd;

  dirs_object_t(const dirs_object_t &rhs) { }
  dirs_object_t operator=(const dirs_object_t &rhs) { }

  void reset();

public:
  dirs_object_t();

  void to_string(std::string &);
  void from_string(std::string &);

  void enumfiles(const char *mask);
  void enumdrives();
  WIN32_FIND_DATAA *filenstats(size_t idx);
  WIN32_FIND_DATAA *dirnstats(size_t idx);
  ~dirs_object_t();
};

#endif