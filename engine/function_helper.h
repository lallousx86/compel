#ifndef __FUNCTION_HELPER_02022006__
#define __FUNCTION_HELPER_02022006__

#include <string>
#include "fwd.h"
#include "symbol.h"
#include "function.h"
#include "interpreter.h"
#include "symbol_table.h"

class function_helper: public function_t
{
public:
  static parse_errors_e eval_from_to(
    interpreter_t *, 
    size_t from, size_t to, 
    std::string &,
    bool bKeepQuotes = false,
    char chDelim = ' ');
};

#endif