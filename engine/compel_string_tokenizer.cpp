#include "compel_string_tokenizer.h"
#include "parse_util.h"

compel_string_tokenizer_t::compel_string_tokenizer_t() :
_tokenizer(0, " \t", "\"")
{

}

void compel_string_tokenizer_t::set_param(
  const char *input, 
  const char *delim,
  const char *quote,
  const char *escape,
  const char *keepdelim)
{
  _tokenizer.set_param(input, delim, quote, escape, keepdelim);
}

size_t compel_string_tokenizer_t::parse(
  const char *input, 
  const char *delim, 
  const char *quote, 
  const char *escape,
  const char *keepdelim)
{
  return _tokenizer.parse(input, delim, quote, escape, keepdelim);
}

const char *compel_string_tokenizer_t::get_string(size_t idx, bool b_get_empty_quote)
{
  const char *result = _tokenizer.get_string(idx);
  if (b_get_empty_quote && *result == 0)
    result = "\"\"";
  return result;
}

const char *compel_string_tokenizer_t::set_string(size_t idx, const char *val)
{
  _tokenizer.set_string(idx, val);
  return _tokenizer.get_string(idx);
}

const size_t compel_string_tokenizer_t::parsed_count() const
{
  return _tokenizer.parsed_count();
}

const char *compel_string_tokenizer_t::join(const char *delim)
{
  return _tokenizer.join(delim);
}

const char *compel_string_tokenizer_t::join_from(const size_t idx, const char *delim)
{
  return join_from_to(idx, this->parsed_count(), delim);
}

const char *compel_string_tokenizer_t::join_from_to(const size_t from, size_t to, const char *delim)
{
  _joined_str.clear();
  for (size_t i=from;i<to;i++)
  {
    const char *str_i = this->get_string(i);
    bool bQuoteIt = strchr(str_i, ' ') != 0;

    if (bQuoteIt)
      _joined_str.push_back('"');

    _joined_str += str_i;

    if (bQuoteIt)
      _joined_str.push_back('"');

    if (i != to-1)
      _joined_str += delim;
  }
  return _joined_str.c_str();
}

void compel_string_tokenizer_t::unescape_c_string(const char *src, std::string &out)
{
  std::string in = src;
  std::string::size_type pos;
  while ( (pos = in.find("\\$")) != std::string::npos)
  {
    in.erase(pos, 1);
  }

  parse_util::unescape_c_string(in.c_str(), out);
}

long compel_string_tokenizer_t::parse_number(const char *p)
{
  return parse_util::parse_number(p);
}