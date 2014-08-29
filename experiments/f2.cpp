#include <map>
#include <string>
#include <string.h>
#include <stdio.h>
#include <list>

//#define _USE_ABSTRACT_STRING_TOKENIZER_
#include "lib\rdp_string_tokenizer.h"

//#include "compel_string_tokenizer.h"

int main()
{
#ifdef _USE_ABSTRACT_STRING_TOKENIZER_
  abstract_string_tokenizer_t *a = new rdp_string_tokenizer(0, " \t", "\"");
  delete a;
#endif

  rdp_string_tokenizer t(0, " \t", "\"");

  t.parsed_count();

  //t.parse("Hello");
  //a->parse("hello world");

  return 0;
}