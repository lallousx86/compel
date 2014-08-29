#include "rdp_string_tokenizer.h"
#include <string.h>

/*
The grammar that we parse using recursive-descent technique is close to:

S  -> ((DELIM* S1 | S2) | epsilon)*
S1 -> ALPHA
S2 -> QUOTE ALPHA QUOTE

You may enable the following preprocessor defines:
_USE_ABSTRACT_STRING_TOKENIZER_ : to allow this class to inherit from the abstract_string_tokenizer
_RDP_USE_INLINE_ : to make some functions inline where possible

History
---------
03/04/2006 -
Initial version

03/07/2006 -
bugfix: forgot to reset some variables upon each parse() call

03/14/2006 -
bugfix: can now parse "" as empty string

Todo
-----
. make _result[n] as a struct of: struct { char *str; int token_pos; }, so to preserve
the token position in the initial string

*/

#ifdef _RDP_USE_INLINE_
  #ifdef _MSC_VER
    #define _AS_INLINE __forceinline
  #else
    #define _AS_INLINE inline
  #endif
#else
  #define _AS_INLINE
#endif

rdp_string_tokenizer::rdp_string_tokenizer(
  const char *input, 
  const char *delim,
  const char *quote,
  const char *escape,
  const char *keepdelim)
{
  _cur_string = _input = _delim = _quote = _escape = _keepdelim = _joined_str = 0;

  _result = 0;
  _parsed_count = 0;

  set_param(input, delim, quote, escape, keepdelim);
}

rdp_string_tokenizer::~rdp_string_tokenizer()
{
  assign_str(&_input, 0);
  assign_str(&_cur_string, 0);
  assign_str(&_delim, 0);
  assign_str(&_quote, 0);
  assign_str(&_escape, 0);
  assign_str(&_keepdelim, 0);
  assign_str(&_joined_str, 0);

  build_result(true);
}

void rdp_string_tokenizer::assign_str(char **out, const char *src)
{
  delete [] *out;
  *out = 0;

  if (src == 0)
    return;

  *out = new char[strlen(src)+1];
  strcpy(*out, src);
}

void rdp_string_tokenizer::set_param(
  const char *input, 
  const char *delim,
  const char *quote,
  const char *escape,
  const char *keepdelim)
{
  if (input != 0)
  {
    assign_str(&_input, input);
    assign_str(&_cur_string, input);

    _cur_quote = 0;
    _cur_pos = 0;
    _max_pos = strlen(_input);
    _eos = false;
  }

  if (delim != 0)
    assign_str(&_delim, delim);

  if (quote != 0)
    assign_str(&_quote, quote);

  if (escape != 0)
    assign_str(&_escape, escape);

  if (keepdelim != 0)
    assign_str(&_keepdelim, keepdelim);
}

// checks current token if in QUOTEs list
_AS_INLINE bool rdp_string_tokenizer::is_quote()
{
  return (_quote != 0) && (strchr(_quote, _cur_token) != 0);
}

// checks current token if in ESCAPE list
_AS_INLINE bool rdp_string_tokenizer::is_escape()
{
  return (_escape != 0) && strrchr(_escape, _cur_token) != 0;
}

// checks current token if in DELIM list
_AS_INLINE bool rdp_string_tokenizer::is_delim()
{
  return (_delim != 0) && strrchr(_delim, _cur_token) != 0;
}

_AS_INLINE bool rdp_string_tokenizer::get_token()
{
  if (_cur_pos >= _max_pos)
  {
    _eos = true;
    return false;
  }
  _cur_token = _input[_cur_pos++];
  return true;
}

// all the parse_xxx() functions expects the curtoken to be loaded with their value
// they will only call get_token() if they need another token than the one initially passed to them

// 
// 1. check cur_token if is_delim
// 2. if TRUE -> get_token(), loop 1
// 3. if FALSE -> return TRUE
// 4. if get_token() = eos return FALSE
_AS_INLINE bool rdp_string_tokenizer::parse_delim()
{
  bool b;

  while (is_delim())
  {
    b = get_token();
    if (b == false)
      return false;
  }
  return true;
}

// called when we successfully match the "alpha" non-terminal
_AS_INLINE void rdp_string_tokenizer::on_match_alpha()
{
  // we maintain all matches are multi-sz string
  *_cur_string_p = 0; 
  _cur_string_p++;
  _parsed_count++;
}

// 1. if cur_token is ESCAPE and get_token=FALSE , thus end of string!
// 2. if cur_token is ESCAPE and get_token=TRUE, thus we have skipped the escape character by calling get_token
// 3. save cur_token in our list
// 4. are we in a quoted string and cur_token matches the quote? then stop parsing alpha
// 5. if we are in a quoted string, even if we encounter a delimiter, we continue parsing alpha
// 6. if we are not in a quoted string, we stop if we encounter a delimiter
_AS_INLINE void rdp_string_tokenizer::parse_alpha()
{
  do
  {
    if (is_escape() && !get_token())
      return;

    // save string
    *_cur_string_p = _cur_token;
    _cur_string_p++;

    // can't get next char? exit
    if (!get_token())
      return;

    if (_cur_quote == _cur_token)
      return;

  } while (_cur_quote != 0 || !is_delim());
  return;
}

_AS_INLINE void rdp_string_tokenizer::parse_s1()
{
  parse_alpha();
  on_match_alpha();
}

_AS_INLINE bool rdp_string_tokenizer::parse_s2()
{
  _cur_quote = _cur_token;

  if (!get_token())
    return false;

  if (_cur_token != _cur_quote) 
    parse_alpha();

  on_match_alpha();

  // actually, here we expect another QUOTE, be we won't enforce it
  if (!get_token())
    return false;

  _cur_quote = 0;
  return true;
}

_AS_INLINE void rdp_string_tokenizer::parse_s()
{
  while (get_token())
  {
    if (!parse_delim())
      return;

    if (is_quote())
    {
      if (!parse_s2())
        return;
    }
    else
    {
      parse_s1();
    }
  }
  return;
}

size_t rdp_string_tokenizer::parse(
  const char *input, 
  const char *delim, 
  const char *quote, 
  const char *escape,
  const char *keepdelim)
{
  _parsed_count = 0;
  if (input == 0 && _input == 0)
    return _parsed_count;

  set_param(input, delim, quote, escape, keepdelim);

  // position to begining of string
  _cur_string_p = _cur_string;

  parse_s();
   
  build_result();

  return _parsed_count;
}

void rdp_string_tokenizer::build_result(bool bJustFree)
{
  size_t i;

  if (_result)
  {
    for (i=0;_result[i] != 0;i++)
      assign_str(&_result[i], 0);
    delete [] _result;
    _result = 0;
  }

  if (bJustFree)
    return;

  if (_parsed_count == 0)
    return;

  _result = new char *[_parsed_count+1];
  _cur_string_p = _cur_string;
  for (i=0;i<_parsed_count;i++)
  {
    _result[i] = 0;
    assign_str(&_result[i], _cur_string_p);
    _cur_string_p += strlen(_cur_string_p) + 1;
  }
  _result[i] = 0;
}

const char *rdp_string_tokenizer::get_string(size_t idx)
{
  if (idx > _parsed_count)
    return 0;
  return _result[idx];
}

void rdp_string_tokenizer::set_string(size_t idx, const char *val)
{
  if (val == 0 || idx > _parsed_count)
    return;
  assign_str(&_result[idx], val);
}

const size_t rdp_string_tokenizer::parsed_count() const
{
  return _parsed_count;
}

const char *rdp_string_tokenizer::join(const char *delim)
{
  // free previous allocation
  assign_str(&_joined_str, 0);

  if (_parsed_count == 0)
    return 0;

  // approximate length
  size_t len = (strlen(delim) * _parsed_count) + strlen(_input);

  // compute some consts for optimization
  size_t last_idx = _parsed_count-1;


  char *str, *p;

  str = new char[len];
  p   = str;

  for (size_t i=0;i<_parsed_count;i++)
  {
    // <strcpy> strcpy(p, get_string(i))
    char *s1 = (char *) get_string(i), ch;
    while (ch = *s1)
    {
      s1++;
      *p = ch;
      p++;
    }
    *p = 0;
    // </strcpy>

    if (i != last_idx)
    {
      // <strcpy> strcpy(p, delim);
      s1 = (char *) delim;
      while (ch = *s1)
      {
        s1++;
        *p = ch;
        p++;
      }
      *p = 0;
      // </strcpy>
    }
  }
  assign_str(&_joined_str, str);
  delete [] str;
  return _joined_str;
}
