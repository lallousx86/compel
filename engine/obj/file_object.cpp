#include "file_object.h"


// called from ctor and cctor
void file_object_t::basic_init()
{
  set_obj_kind(object_file);
}

// called only from ctor
void file_object_t::init()
{
  // set the object's attributes
  _val_size  = insert_attribute("size");
  _val_pos   = insert_attribute("pos");
  _val_read  = insert_attribute("read");
  _val_write = insert_attribute("write");
  _val_ok    = insert_attribute("ok");
  _val_fname = insert_attribute("name");
  _val_eof   = insert_attribute("eof");

  basic_init();
}

// checks if we are at end of file
void file_object_t::update_val_eof()
{
  _val_eof->set_int_value(feof(_fp) ? 1 : 0);
}

// updates the ok flag
void file_object_t::update_val_ok(bool bOk)
{
  _val_ok->set_int_value(bOk ? 1 : 0);
}

// updates the last read count
void file_object_t::update_val_readcount(size_t count)
{
  _val_read->set_int_value((long) count);
}

// updates the last write count
void file_object_t::update_val_writecount(size_t count)
{
  _val_write->set_int_value((long) count);
}

// updates the file's position
void file_object_t::update_val_filepos()
{
  _val_pos->set_int_value(ftell(_fp));
}

void file_object_t::update_val_filesize()
{
  // save current position
  long lastpos = ftell(_fp);

  // go to end of file
  fseek(_fp, 0, SEEK_END);

  // get file's position == file's size
  _val_size->set_int_value(ftell(_fp));

  // restore old position
  fseek(_fp, lastpos, SEEK_SET);
}

// Decrements reference and closes file on last reference
void file_object_t::dispose()
{
  if (--*_refcount == 0)
  {
    close();
    delete _refcount;
  }
}

file_object_t::file_object_t(const char *fn, const char *openflags)
{
  init();

  _filename = fn;
  _openflags = openflags;

  _val_fname->set_str_value(fn);
  _refcount = new size_t(1);
  _fp = 0;
}

// copy constructor
file_object_t::file_object_t(const file_object_t &rhs) : object_t(rhs)
{
  basic_init();

  _refcount = rhs._refcount;
  ++*_refcount; // increment reference

  _fp = rhs._fp;
  _filename = rhs._filename;
  _openflags = rhs._openflags;
}

bool file_object_t::open()
{
  _fp = fopen(_filename.c_str(), _openflags.c_str());
  if (_fp == NULL)
  {
    update_val_ok(false);
    return false;
  }

  update_val_ok(true);
  update_val_eof();
  update_val_filepos();
  update_val_filesize();

  return true;
}

bool file_object_t::write(void *buffer, size_t size)
{
  if (_fp == NULL)
  {
    _val_ok->set_int_value(0);
    return false;
  }

  size_t nwrite = fwrite(buffer, 1, size, _fp);
  update_val_ok(nwrite > 0);
  update_val_writecount(nwrite);
  update_val_filepos();
  update_val_eof();
  return true;
}

bool file_object_t::seek(long offset, int origin)
{
  if (_fp == NULL || fseek(_fp, offset, origin) == 0)
  {
    update_val_ok(false);
    return false;
  }

  update_val_ok(true);
  update_val_filepos();
  update_val_eof();

  return true;
}

bool file_object_t::read(void *buffer, size_t size)
{
  if (_fp == NULL)
  {
    _val_ok->set_int_value(0);
    return false;
  }

  size_t nread = fread(buffer, 1, size, _fp);

  update_val_ok(nread > 0);
  update_val_readcount(nread);
  update_val_filepos();
  update_val_eof();

  return true;
}

bool file_object_t::reada(void *buffer, long position, size_t size)
{
  if (_fp == NULL)
  {
    update_val_ok(false);
    return false;
  }

  // Try to seek to desired location
  long oldpos = ftell(_fp);
  if (fseek(_fp, position, SEEK_SET) == 0)
  {
    update_val_ok(false);
    fseek(_fp, oldpos, SEEK_SET);
    return false;
  }

  size_t nread = fread(buffer, 1, size, _fp);

  update_val_ok(nread > 0);
  update_val_readcount(nread);
  update_val_filepos();
  update_val_eof();

  return true;
}

void file_object_t::close()
{
  if (_fp)
  {
    fclose(_fp);
    _fp = 0;
  }
}

file_object_t::~file_object_t()
{
  dispose();
}
