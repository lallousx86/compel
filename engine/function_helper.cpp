#include "function_helper.h"
#include <sstream>

parse_errors_e function_helper::eval_from_to(
  interpreter_t *_int, 
  size_t from, size_t to, 
  std::string &out,
  bool bKeepQuotes,
  char chDelim)
{
  std::ostringstream os;

  for (size_t i=from;i<to;i++)
  {
    const char *evaluated_str = _int->evaluate_at(i);

    bool bQuoteIt = false;
    
    if (bKeepQuotes)
      bQuoteIt = strchr(evaluated_str, ' ') != 0;

    if (bQuoteIt && bKeepQuotes)
      os << '"';

    os << evaluated_str;

    if (bQuoteIt && bKeepQuotes)
      os << '"';

    if (chDelim != 0)
    {
      if (i != to-1)
        os << " ";
    }
  }
  compel_string_tokenizer_t::unescape_c_string(os.str().c_str(), out);
  return parse_error_none;
}