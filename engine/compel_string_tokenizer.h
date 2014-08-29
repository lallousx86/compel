#ifndef __COMPEL_STOKENIZER_03072006__
#define __COMPEL_STOKENIZER_03072006__

#include <string>
#include "lib/abstract_string_tokenizer.h"
#include "lib/rdp_string_tokenizer.h"

class compel_string_tokenizer_t
{
private:
  rdp_string_tokenizer _tokenizer;
  std::string _joined_str;
public:
  compel_string_tokenizer_t();

  void set_param(
    const char *input, 
    const char *delim,
    const char *quote,
    const char *escape,
    const char *keepdelim);

  size_t parse(
    const char *input = 0, 
    const char *delim = 0, 
    const char *quote = 0, 
    const char *escape = 0,
    const char *keepdelim = 0);

  const char *get_string(size_t idx, bool b_get_empty_quote = false);
  const char *set_string(size_t idx, const char *val);
  const size_t parsed_count() const;
  const char *join(const char *delim);
  const char *join_from(const size_t idx, const char *delim = " ");
  const char *join_from_to(const size_t from, size_t to, const char *delim = " ");

  static void unescape_c_string(const char *src, std::string &out);
  static long parse_number(const char *p);
};

#endif