#include "stdafx.h"

#include "../examples/example_initialize.cpp"
#include "../examples/example_run_script.cpp"
#include "../examples/example_tokenizer.cpp"
#include "../examples/example_value.cpp"
#include "../examples/example_object.cpp"
#include "../examples/example_user_command.cpp"
#include "../examples/example_error_handler.cpp"
#include "../examples/example_user_ext.cpp"
#include "../examples/example_user_ext_clib.cpp"
#include "../../engine/lib/lib_getch.h"

//example_run_script(script);
//example_value(script);
//example_object(script);
//example_user_ext(script);
//example_user_command(script);
//example_error_handler(script);
//compel_script_interpret_line(script, "ms1gox 123");

int canvas_test(compel_script_t &script)
{
  int err;

  compel_extension_load(script, "canvas", "..\\..\\canvas_ext\\compel_canvas_ext.dll");

  err = compel_script_load_file(script, "ext_test.compel");

  err = compel_script_run(script);

  return err;
}

extern "C" intptr_t _coninpfh;

int main(int argc, _TCHAR* argv[])
{  
  char ch;

  compel_script_t script;

  example_initialize(script);

  //canvas_test(script);
  compel_script_load_lines(script, "var $c;com.create $c;echoln ch= $c", ";");
  compel_script_run(script);

/*
  ch = my_getch();
  printf("c,ch=%c\n", ch);
*/
  compel_script_deinit(script);

	return 0;
}