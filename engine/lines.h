#ifndef __LINES_01222006__
#define __LINES_01222006__

#include "fwd.h"

class lines_t
{
public:
	enum
	{
		maxlinewidth = 1024*10
	};
private:
  lines_container_type_t _lines;
  std::string   _curline;
  size_t _curlineno;

  void assign(const lines_t &rhs);
public:
  lines_t();
  lines_t(const lines_t &rhs);
  lines_t &operator=(const lines_t &rhs);

  void clear();

  void add(const char *line);
  void add(const std::string &line);
  void add(const char *line, size_t at);
  void add(const std::string &line, size_t at);

  bool delete_line(size_t lineno);
  const char *getline(size_t lineno);
  size_t count() const;
  std::string &line(size_t lineno);
  bool read_from_file(const char *fn, bool bClearLines = false);
  size_t get_cur_line() const;
  lines_container_type_t &get_string_list();
  size_t merge_lines(lines_t &lines2, size_t at);
};

#endif