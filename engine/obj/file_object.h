#ifndef __FILEOBJECT__01242006__
#define __FILEOBJECT__01242006__

#include <stdio.h>
#include "../object.h"

class file_object_t : public object_t
{
private:
  value_t *_val_size; // file's size
  value_t *_val_pos; // file's current position
  value_t *_val_ok; // last operation ok?
  value_t *_val_read; // last read count
  value_t *_val_write; // last write count
  value_t *_val_fname; // file name
  value_t *_val_eof; // designates if it we reached end of 
  size_t  *_refcount; // reference count

  std::string _filename;
  std::string _openflags;
  FILE *_fp;

  void basic_init();
  void init();

  void update_val_eof();
  void update_val_ok(bool bOk);
  void update_val_readcount(size_t count);
  void update_val_writecount(size_t count);
  void update_val_filepos();
  void update_val_filesize();

  void dispose();

public:
  explicit file_object_t(const char *fn, const char *openflags);
  file_object_t(const file_object_t &rhs);

  bool open();
  void close();

  bool write(void *buffer, size_t size);

  bool seek(long offset, int origin);

  bool read(void *buffer, size_t size);
  bool reada(void *buffer, long position, size_t size);

  ~file_object_t();
};

#endif