#include "dirs_object.h"

static const char *s_count = "count";

dirs_object_t::dirs_object_t() : object_t()
{
  set_obj_kind(object_dirs);
  _op = op_unknown;
}

dirs_object_t::~dirs_object_t()
{
}

void dirs_object_t::to_string(std::string &out)
{
  if (_op == op_filenstats || _op == op_dirnstats && _last_fd != 0)
  {
    //;!
  }
  else if (_op == op_enumdrives)
  {
    value_t *attr = find_attribute(s_count);
    if (attr == 0)
      return;

    out.clear();

    long cnt = attr->get_int_value();
    for (long i=1;i<=cnt;i++)
    {
      char temp[20];
      sprintf(temp, "%d", i);
      attr = find_attribute(temp);
      if (attr == 0)
        continue;
      out += attr->get_str_value();
      out.push_back(',');
    }
    if (!out.empty())
      out.erase(out.length()-1);
  }
}

void dirs_object_t::reset()
{
  clear_attributes();
  _dirs_vect.clear();
  _files_vect.clear();
  _last_fd = 0;
}

void dirs_object_t::from_string(std::string &from)
{

}

void dirs_object_t::enumfiles(const char *mask)
{
  WIN32_FIND_DATAA fd;

  HANDLE hfind = ::FindFirstFileA(mask, &fd);

  BOOL b = hfind != INVALID_HANDLE_VALUE;
  
  int count = 0;
  int dir_count = 0;
  int file_count = 0;
  
  _op = op_enumfiles;

  reset();

  char temp[20];

  while (b)
  {
    if ((fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == FILE_ATTRIBUTE_DIRECTORY)
    {
      if (
        (
        fd.cFileName[0] == '.' && 
        fd.cFileName[1] == '\0') 
        ||
        (
        fd.cFileName[0] == '.' && 
        fd.cFileName[1] == '.' && 
        fd.cFileName[2] == '\0')
        )
      {
        goto l_continue;
      }
      _dirs_vect.push_back(fd);
      dir_count++;
      sprintf(temp, "dir%d", dir_count);
    }
    else
    {
      file_count++;
      sprintf(temp, "file%d", file_count);
      _files_vect.push_back(fd);
    }
    insert_attribute(temp, value_t(fd.cFileName));
    ++count;
l_continue:
    b = ::FindNextFileA(hfind, &fd);
  }

  if (count > 0)
    ::FindClose(hfind);

  insert_attribute("dircount", value_t(dir_count));
  insert_attribute("filecount", value_t(file_count));
  insert_attribute(s_count, value_t(count));
}

void dirs_object_t::enumdrives()
{
  DWORD dwDrivesMask = ::GetLogicalDrives();

  int count = 0;

  char temp1[20], temp2[20];

  reset();

  _op = op_enumdrives;

  for (DWORD i=0;i<26;i++)
  {
    if ((dwDrivesMask & (1 << i)) == 0)
      continue;

    count++;

    sprintf(temp1, "%c:", i+'A');
    sprintf(temp2, "%d", count);

    insert_attribute(temp2, value_t(temp1));
  }

  insert_attribute(s_count, value_t(count));
}

WIN32_FIND_DATAA *dirs_object_t::filenstats(size_t idx)
{
  _op = op_filenstats;
  _last_fd = 0;
  if (idx > _files_vect.size())
    return 0;
  return &_files_vect.at(idx);
}

WIN32_FIND_DATAA *dirs_object_t::dirnstats(size_t idx)
{
  _op = op_dirnstats;
  _last_fd = 0;
  if (idx > _dirs_vect.size())
    return 0;
  return &_dirs_vect.at(idx);
}