// tests error maping (internal)
void test_error_map()
{
  /*
  compel_engine_to_lib_error(5);
  compel_engine_to_lib_error(3);
  compel_engine_to_lib_error(511);
  compel_engine_to_lib_error(compel_error_success);
  */
}

// the error handler
int COMPEL_API script_error_handler(compel_script_t script, size_t lineno, int liberr)
{
  const char *faulty_code = compel_script_get_line(script, lineno);

  printf("error handler caught error %d @ %d:\n>%s<\n", liberr, lineno, faulty_code);

  return compel_error_success;
}

// shows how to install an error handler to handle script errors
void example_error_handler(compel_script_t script)
{
  compel_script_set_error_handler(script, script_error_handler);
  compel_script_load_lines(script, 
    "echoln okay;"
    "some_bad_command;"
    "echoln after_bad_command",
    ";");
  compel_script_run(script);
}
