#ifndef __ABSTRACT_STRINGTOKENIZER_03042006
#define __ABSTRACT_STRINGTOKENIZER_03042006

class abstract_string_tokenizer_t
{
public:
  abstract_string_tokenizer_t(
    const char *input = 0, 
    const char *delim = 0, 
    const char *quote = 0,
    const char *escape = 0,
    const char *keepdelim = 0) { }

  virtual void set_param(
    const char *input, 
    const char *delim, 
    const char *quote,
    const char *escape,
    const char *keepdelim) = 0;

  virtual ~abstract_string_tokenizer_t() { }

  virtual const size_t parsed_count() const = 0;

  virtual void set_string(size_t idx, const char *val) = 0;
  virtual const char *get_string(size_t idx) = 0;

  virtual const char *join(const char *delim) = 0;

  virtual size_t parse(
    const char *input = 0, 
    const char *delim = 0, 
    const char *quote = 0,
    const char *escape = 0,
    const char *keepdelim = 0) = 0;
};

#endif