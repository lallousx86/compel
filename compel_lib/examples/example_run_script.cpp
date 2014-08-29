int example_run_script(compel_script_t &script)
{
  // script lines
  char *script_lines =
    "echoln \"Hello world\";"
    "echoln \"Welcome to COMPEL\";"
    "echoln internal:> $_COMPEL <\n;";

  compel_script_load_lines(script, script_lines, ";");
  int err = compel_script_run(script);

  return err;
}
