#ifndef __01222005__RT_STDSTR__
#define __01222005__RT_STDSTR__

#include <string>

class RuntimeStdStringCmp
{
public:
  enum cmp_mode { normal, nocase };
private:
  const cmp_mode mode;

  static bool nocase_compare(char c1, char c2);
public:
  RuntimeStdStringCmp(cmp_mode m = nocase);

  bool operator()(const std::string &s1, const std::string &s2) const;
};

class StdStringCmpCase : public RuntimeStdStringCmp
{
public:
  StdStringCmpCase() : RuntimeStdStringCmp(normal)
  {
  }
};

class StdStringCmpNoCase : public RuntimeStdStringCmp
{
public:
  StdStringCmpNoCase() : RuntimeStdStringCmp(nocase)
  {
  }
};

#endif