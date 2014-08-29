#include "stdafx.h"

// tests error maping (internal)
#include "../examples/example_initialize.cpp"
#include "../examples/example_run_script.cpp"
#include "../examples/example_tokenizer.cpp"
#include "../examples/example_value.cpp"
#include "../examples/example_object.cpp"
#include "../examples/example_user_command.cpp"
#include "../examples/example_error_handler.cpp"
#include "../examples/example_pause_script.cpp"
#include "../examples/example_internal.cpp"


int main(int argc, char *argv[])
{
  compel_script_t script;

  example_initialize(script);

//  example_user_command(script);

  compel_script_deinit(script);

	return 0;
}