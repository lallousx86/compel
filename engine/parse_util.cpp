#include "parse_util.h"
#include <algorithm>

// ==================================================================================
//
//                             PARSE UTIL CLASS
//
// ==================================================================================
long parse_util::parse_number(const char *p)
{
  int base;
  char *dum;

  if ( (strnicmp(p+strlen(p)-1, "h", 1)==0) || (strncmp(p, "0x", 2)==0))
    base = 16;
  else if (p[0] == '0')
    base = 8;
  else
    base = 10;
  return strtol(p, &dum, base);
}

//////////////////////////////////////////////////////////////////////////
// Converts a hex character into a number
// That is '0'-'9' are 0-9 and
// 'A'-'F' are 10-15
//
int parse_util::hexchartoint(char x)
{
  if (x >= '0' && x <= '9')
    return x - '0';

  x = toupper(x);
  if (x >= 'A' && x <= 'F')
    return x - 'A' + 10;
  return 0;
}

void parse_util::escape_std_string(const char *src, std::string &out)
{
  size_t len = strlen(src);
  out.clear();
  for (size_t i=0;i<len;i++)
  {
    out.push_back(src[i]);
    if (src[i] == '\\')
      out.push_back('\\');
  }
}

//////////////////////////////////////////////////////////////////////////
// Given a string that has C escaped characters, it unescapes them
// Example an input string having:
// hello\x12 will yield: hello{alt+12} <-- actual ascii code
// etc...
void parse_util::unescape_c_string(const char *src, char *dst, size_t *newlen)
{
  if (src != dst)
    strcpy(dst, src);

  struct chars_t
  {
    char *str;
    char ch;
  };

  chars_t chars[] = 
  {
    {"\\t", '\t'},
    {"\\n", '\n'},
    {"\\r", '\r'},
    {"\\a", '\a'},
    {"\\b", '\b'},
    {"\\|", '\\'},
  };

  size_t len = strlen(dst);

  char *p;

  while (p = strstr(dst, "\\\\"))
  {
    *p = '\\';
    p++;
    *p = '|';
  }

  char *p2 = dst;
  while (p = strstr(p2, "\\x"))
  {
    p += 2;
    int c = 0;
    if (isxdigit(*p))
    {
      unsigned char ch = hexchartoint(*p);
      c++;
      if (isxdigit(*(p+1)))
      {
        ch = (ch << 4) | hexchartoint(*(p+1));
        c++;
      }
      p = p - 2;
      *p = ch;
      memcpy(p+1, p + c + 2, len - (p-p2));
      len -= 1 + c;
      p2 = p+1;
    }
  }

  for (size_t i=0;i<sizeof(chars)/sizeof(chars[0]);i++)
  {
    while (p = strstr(dst, chars[i].str))
    {
      *p = chars[i].ch;
      memcpy(p+1, p + 2, len - (p - dst));
      len -= 1;
    }
  }

  if (newlen != 0)
    *newlen = len;
}

void parse_util::unescape_c_string(const char *src, std::string &out)
{
  char *dest = new char[strlen(src) + 20];
  size_t newlen = 0;
  parse_util::unescape_c_string(src, dest, &newlen);
  out.assign(dest, newlen);
  delete [] dest;
}

void *parse_util::str_to_ptr(const char *str)
{
  void *p;
  sscanf(str, "%p", &p);
  return p;
}

void parse_util::ptr_to_str(void *ptr, char *str)
{
  sprintf(str, "%p", ptr);
}

void parse_util::to_lower(std::string &str)
{
  std::transform(str.begin(), str.end(), str.begin(), ::towlower);
}