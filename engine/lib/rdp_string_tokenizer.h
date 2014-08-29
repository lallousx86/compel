#ifndef __RDP_STRING_TOKENIZER_03052006__
#define __RDP_STRING_TOKENIZER_03052006__

#ifdef _USE_ABSTRACT_STRING_TOKENIZER_
  #include "abstract_string_tokenizer.h"
#endif

/************************************************************************
The zlib/libpng License

This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from
the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented;
	you must not claim that you wrote the original software.
	If you use this software in a product, an acknowledgment
	in the product documentation would be appreciated but is
	not required.

2. Altered source versions must be plainly marked as such,
	and must not be misrepresented as being the original software.

3. This notice may not be removed or altered from any source distribution.

***********************************************************************/

/*

Recursive Descent Parser based string tokenizer.

(c) Elias Bachaalany aka lallous <lallousx86@yahoo.com>

*/

class rdp_string_tokenizer 
#ifdef _USE_ABSTRACT_STRING_TOKENIZER_
  : public abstract_string_tokenizer_t
#endif
{
private:
  char _cur_token, _cur_quote;
  size_t _cur_pos, _max_pos;

  char *_cur_string, *_cur_string_p;

  char *_input, *_delim, *_keepdelim, *_quote, *_escape;

  bool _eos;

  char **_result;
  char *_joined_str;

  size_t _parsed_count;

  void assign_str(char **out, const char *src);

  bool is_quote();
  bool is_escape();
  bool is_delim();
  bool get_token();
  bool parse_delim();
  void on_match_alpha();
  void parse_alpha();
  void parse_s1();
  bool parse_s2();
  void parse_s();

  void build_result(bool bJustFree = false);

public:
  ~rdp_string_tokenizer();


  rdp_string_tokenizer(
    const char *input = 0, 
    const char *delim = 0, 
    const char *quote = 0, 
    const char *escape = 0,
    const char *keepdelim = 0);

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

  const char *get_string(size_t idx);
  void set_string(size_t idx, const char *val);
  const size_t parsed_count() const;
  const char *join(const char *delim);
};

#endif
