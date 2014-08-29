#ifndef __LIBUSERCOMMAND_04032006__
#define __LIBUSERCOMMAND_04032006__

#include "interpreter.h"
#include "function.h"
#include "function_helper.h"
#include "compel_lib.h"

class fnc_libusercommand : public function_t
{
private:
  lib_usercommand_info_t _info;
  std::string _ret_name;
  compel_script_t _compel_script;
public:
  fnc_libusercommand(interpreter_t *, p_lib_usercommand_info_t);

  p_lib_usercommand_info_t get_info();
  std::string &get_ret_name();
  parse_errors_e execute();
  parse_errors_e register_function(compel_script_t);
};

#endif