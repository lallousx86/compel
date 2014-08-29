#pragma once

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_comments : public function_t
{
public:
  enum op_e
  {
    op_slc_hash, // single line comment - hash character
    op_slc_fwd2, // single line comment - two forward slashes
    op_mlc1_beg, // multiple lines comment 1 (begin)
    op_mlc1_end, // ^ end
  };

private:
  void basic_init();
  op_e _op;
public:
  fnc_comments(interpreter_t *, op_e);
  fnc_comments();
  parse_errors_e execute();
  parse_errors_e prepare(size_t passno, int &start_or_failing_line);
  parse_errors_e register_function(interpreter_t *);

  static const char *fnc_name;
};