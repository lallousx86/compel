#pragma once

#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <string>

class parse_util
{
public:
  static long parse_number(const char *p);
  static void unescape_c_string(const char *src, char *dst, size_t *newlen = 0);
  static void unescape_c_string(const char *src, std::string &out);
  static void escape_std_string(const char *src, std::string &out);
  static int  hexchartoint(char x);
  static void *str_to_ptr(const char *str);
  static void ptr_to_str(void *ptr, char *str);
  static void to_lower(std::string &str);
};
