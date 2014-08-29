#include "RuntimeStdStringCmp.h"

bool RuntimeStdStringCmp::nocase_compare(char c1, char c2)
{
  return toupper(c1) < toupper(c2);
}

RuntimeStdStringCmp::RuntimeStdStringCmp(cmp_mode m) : mode(m)
{
}

bool RuntimeStdStringCmp::operator()(const std::string &s1, const std::string &s2) const
{
  if (mode == normal)
  {
    return s1 < s2;
  }
  else
  {
    return std::lexicographical_compare(
      s1.begin(), s1.end(), 
      s2.begin(), s2.end(), 
      nocase_compare);
  }
}
