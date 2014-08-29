#ifndef __COMPEL_FWD_01222006__
#define __COMPEL_FWD_01222006__

#include <map>
#include <string>
#include <list>
#include <set>
#include <fstream>
#include <vector>

#include "lib\RuntimeStdStringCmp.h"

#define COMPEL_BAD_INSTRUCTION_LINE ((size_t)-1)

enum symbol_kind_e
{
  symbol_base,
  symbol_variable,
  symbol_function
};

enum variable_kind_e
{
  variable_value,
  variable_object
};

enum object_kind_e
{
  object_base,
  object_file,
  object_processfile,
  object_memory,
  object_dirs,
  object_com
};

enum function_flags_e
{
  // default flags (one pass function)
  function_flag_none          = 0,

  // function should be prepared at pass 2
  // pass2 functions are those who modify the line numbering/count
  // (such as INCLUDE)
  function_flag_pass2         = 0x1,

  // function should be prepared at pass 3
  // pass3 functions are those who rely on the exact line number
  // and do not change line number at all
  function_flag_pass3         = 0x2,

  // function is a forumla
  function_flag_is_forumla    = 0x4
};

enum compel_consts_e
{
  // maximum function passes
  cc_nbpasses = 3
};

enum parse_errors_e
{
  // #0
  // no errors detected
  parse_error_none = 0, 

  // #1
  // too many parameters passed
  parse_error_more_param, 

  // #2
  // few parameters
  parse_error_less_param, 

  // #3
  // attempted to access an invalid symbol
  parse_error_symbol_not_in_table, 

  // #4
  // we expect a variable of a given type but found another type
  // can happen when passing a value_t variable instead of an object_t
  // or we passing a process_object to a file_object function
  parse_error_symbol_type_mismatch, 

  // #5
  // you attempted to redefine an existing symbol in the symtbl
  // That symbol might be a label, function or any other variable
  parse_error_symbol_redefinition, 

  // #6
  // this line is a comment and not processed
  parse_error_line_is_comment, 

  // #7
  // a function is expected
  parse_error_function_expected, 

  // #8
  // on_parse_error() requested to live with the error and continue parsing
  parse_continue_parsing,

  // #9
  // on_parse_error() requested to stop
  // or a method requested to stop (such as END)
  // or just tell interpreter to stop parsing
  parse_stop_parsing, 

  // #10
  // returned when a function was expected but on_error handler decided to live with the error
  parse_error_wrong_syntax, 

  // #11
  // we parsed an empty line
  parse_line_is_empty, 

  // #12
  // we expected a variable at the given argument
  parse_error_variable_expected,

  // #13
  // special code to instruct interpreter not to advance the source line number
  // which means another mechanism must have adjusted the pointer
  parse_branch_to,

  // #14
  // FREE SLOT
  parse_xxxxxx_xxxx1,

  // #15
  // FREE SLOT
  parse_xxxxx_xxxxx2,

  // #16
  // detected a divide by zero, operation skipped
  parse_error_divide_by_zero_detected,

  // #17
  // we expected an appropriate object of a special type
  parse_error_object_expected,

  // #18
  // line is void
  parse_error_line_is_void,

  // #19
  // one function_t instance requested a complete file reparse, thus re-invoking all
  // the passes
  parse_error_reparse,

  // #20
  // the file you tried to include or run does not exist
  parse_error_file_not_found,

  // #21
  // there was an imbalance in the syntax
  // (a scope closing without opening one)
  parse_error_imbalance,

  // #22
  // Syntax was introduced in a wrong context
  // (an else without an IF)
  parse_error_wrong_context,

  // #23
  // on_parse_error() requested to look and use the next handler
  parse_error_handler_continue_search 

};

class symbol_t;
class variable_t;
class value_t;
class object_t;
class function_t;
class function_helper_t;
class symbol_table_t;
class interpreter_t;
class interpreter_helper_t;

typedef std::map<std::string, value_t, StdStringCmpNoCase> strvalue_map_t;
typedef std::map<std::string, symbol_t *, StdStringCmpNoCase> strsymbol_map_t;
typedef std::list<std::string> string_list_t;
typedef std::map<std::string, symbol_t *, StdStringCmpNoCase> strsymbol_map_t;

// case insensitive string TO size_t map (used for string reference counting)
typedef std::map<std::string, size_t, StdStringCmpNoCase> str_sizet_map_t;

// used lines_t 
typedef std::vector<std::string> lines_container_type_t;

#endif
