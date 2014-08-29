#ifndef __FNC_INCLUDE_02142006__
#define __FNC_INCLUDE_02142006__

#include "../fwd.h"
#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_include : public function_t
{
private:
  static str_sizet_map_t _files_ref;  

  size_t &get_file_ref_count(const char *fn);
public:
  fnc_include(interpreter_t *);

  fnc_include();

  parse_errors_e execute();
  parse_errors_e prepare(size_t passno, int &start_or_failing_line);
  parse_errors_e register_function(interpreter_t *);

  static const char *fnc_name;
};

#endif