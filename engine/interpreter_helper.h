#ifndef __INTERPRETER_HELPER_02092006__
#define __INTERPRETER_HELPER_02092006__

#include "fwd.h"
#include "interpreter.h"
#include "lines.h"

class interpreter_helper_t
{
private:
  interpreter_t  *_int;

public:
  interpreter_bind_struct_t *get_bind_struct();
  void set_interpreter(interpreter_t *);
  void show_lines();
  const char *get_script_file() const;
  void show_symbols();

  lines_t &get_interpreter_lines();
  symbol_table_t *get_interpreter_symtbl();
  interpreter_helper_t(interpreter_t * = 0);

  void set_tokenizer(compel_string_tokenizer_t *tokenizer);
  compel_string_tokenizer_t *get_tokenizer();
};

#endif