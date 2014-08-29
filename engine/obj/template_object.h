#ifndef __DIRS_05082006__
#define __DIRS_05082006__

#include "../object.h"

class dirs_object_t : public object_t
{
private:
  void basic_init();
  void init();
  void dispose();

public:
  explicit dirs_object_t(size_t size);
  dirs_object_t(const dirs_object_t &rhs);

  void to_string(std::string &);
  void from_string(std::string &);

  ~dirs_object_t();
};

#endif