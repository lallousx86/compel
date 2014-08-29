#include "lines.h"


void lines_t::assign(const lines_t &rhs)
{
  if (&rhs == this)
    return;

  _lines = rhs._lines;
  _curline = rhs._curline;
  _curlineno = rhs._curlineno;
}

lines_t::lines_t(const lines_t &rhs)
{
  assign(rhs);
}

lines_t &lines_t::operator=(const lines_t &rhs)
{
  assign(rhs);
  return *this;
}

lines_t::lines_t()
{
  _curlineno = 0;
}

void lines_t::clear()
{
  _lines.clear();
}

void lines_t::add(const char *line)
{
  _curline = line;
  _lines.push_back(_curline);
  _curlineno++;
}

void lines_t::add(const std::string &line)
{
  _curline = line;
  _lines.push_back(line);
  _curlineno++;
}

size_t lines_t::get_cur_line() const
{
  return _curlineno;
}

const char *lines_t::getline(size_t lineno)
{
  lines_container_type_t::iterator it = _lines.begin();
  std::advance(it, lineno);
  return it->c_str();
//	return _lines[lineno].c_str();
}

size_t lines_t::count() const
{
  return _lines.size();
}

std::string &lines_t::line(size_t lineno)
{
	// for std::list
  lines_container_type_t::iterator it = _lines.begin();
  std::advance(it, lineno);
  return *it;
}

lines_container_type_t &lines_t::get_string_list()
{
  return _lines;
}

bool lines_t::read_from_file(const char *fn, bool bClearLines)
{
  std::ifstream inf(fn);
  if (!inf)
    return false;

  if (bClearLines)
    clear();

	std::string str;

  while (!inf.eof())
  {
		std::getline(inf, str);
    add(str);
  }

  inf.close();
  return true;
}

void lines_t::add(const char *line, size_t at)
{
  std::string s = line;
  add(s, at);
}

void lines_t::add(const std::string &line, size_t at)
{
  lines_container_type_t::iterator it = _lines.begin();
  std::advance(it, at);
  _lines.insert(it, line);
}

size_t lines_t::merge_lines(lines_t &lines2, size_t at)
{
  lines_container_type_t::iterator it = _lines.begin();
  std::advance(it, at);

  _lines.insert(it, lines2._lines.begin(), lines2._lines.end());

  // return the new line after the merge
  return lines2.count() + at;
}

bool lines_t::delete_line(size_t lineno)
{
  lines_container_type_t::iterator it = _lines.begin();
  std::advance(it, lineno);
  _lines.erase(it);
  return true;
}