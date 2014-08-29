#define _COMPEL_DEBUG_SCOPE

#pragma warning (disable: 4996)

#include <map>
#include <string>
#include <string.h>
#include <iostream>
#include <stdio.h>
#include <stack>
#include <list>
#include "compel_string_tokenizer.h"
#include "lines.h"

char *identstr(int level)
{
  static char str[1024];

  str[0] = 0;
  for (int i=0;i<level;i++)
  {
    strcat(str, "   ");
  }
  return str;
}

void die(char *str, ...)
{
  printf(str);
  exit(0);
}

int test_slp()
{
  compel_string_tokenizer_t slp;

  size_t count = slp.parse("1 2");

  for (size_t i=0;i<count;i++)
  {
    printf("@%d:>%s<\n", i, slp.get_string(i));
  }
  return 0;
}
 
#include "fnc/fnc_scopes.h"
#include "fnc/fnc_scopes.cpp"

void test1()
{
  lines_t lines;
  scope_preparse_t prep;
  lines.read_from_file("_scope2.txt", true);
  preparse_scopes(lines, prep);
}

int main()
{
  test1();
  return 0;
}