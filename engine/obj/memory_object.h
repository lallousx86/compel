#ifndef __MEMORY_01242006__
#define __MEMORY_01242006__

#include "../object.h"

class memory_object_t : public object_t
{
private:
  char *_ptr;
  size_t _size;

  size_t *_refcount;

  value_t *_val_size;
  value_t *_val_ptr;
  value_t *_val_length;

  void basic_init();
  void init();
  void dispose();

public:
  explicit memory_object_t(size_t size);

  bool alloc(char filler = 0);

  char *get_ptr() const;
  const size_t get_size() const;

  memory_object_t(const memory_object_t &rhs);

  void free();

  void to_string(std::string &);
  void from_string(std::string &);

  ~memory_object_t();
};

#endif