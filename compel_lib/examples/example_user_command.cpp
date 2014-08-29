/*!
\brief lowercase user command
This command takes the first parameter by value and returns the lowercase version
*/
int COMPEL_API my_lowercase(compel_script_t compel_script, int argc, char *argv[])
{
  size_t len = strlen(argv[0]);
  char *s = new char[len+1];
  strcpy(s, argv[0]);
  strlwr(s);

  compel_lu_cmd_set_retval(compel_script, s);

  delete [] s;

  return 0;
}

/*!
\brief uppercase user command by reference
Takes two arguments, the first one is the variable's name that will hold the uppercase
The 2nd argument is the string that needs to be uppercased
*/
int COMPEL_API my_uppercase(compel_script_t script, int argc, char *argv[])
{
  compel_value_t v = compel_value_find(script, argv[0]);
  if (v == 0)
    return compel_error_symbol_expected;

  size_t len = strlen(argv[1]);
  char *s = new char[len+1];
  strcpy(s, argv[1]);
  strupr(s);

  compel_value_set(script, v, s);

  delete [] s;

  return compel_error_success;
}

void example_user_command(compel_script_t &script)
{
  int err;

  // register a command, the long way
  lib_usercommand_info_t cmd = {0};
  cmd.cb = my_lowercase;
  cmd.minargs = 1;
  cmd.maxargs = 1;
  cmd.name = "my_lowercase";
  err = compel_lu_cmd_register(script, &cmd);

  assert(err == compel_error_success);

  // register another command the quick way
  err = compel_lu_cmd_register2(script, my_uppercase, "my_uppercase", 2, 2);

  assert(err == compel_error_success);

  char *lines = 
    "var $s1 \"tHiS iS a StRiNg\";"
    "var $s2 $s1;"
    "echoln \"before user command calls: \" $s1;"
    "my_lowercase $s1;"
    "echoln \"lowercase: \" $my_lowercase;"
    "my_uppercase &$s2 $s1;"
    "echoln \"uppercase: \" $s2;"
    ;

  // load the test script
  compel_script_load_lines(script, lines, ";");

  // run the script
  compel_script_run(script);
}
