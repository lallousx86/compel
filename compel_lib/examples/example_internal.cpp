void example_internal_script_load_and_show(compel_script_t &script, char *fn)
{
  compel_script_load_file(script, fn);
  compel_internal(script, compel_internal_showlines, 0, 0, 0);
}

void example_internal_multiple_scripts(compel_script_t &script)
{
  const int max_scripts = 4;
  char *scripts[max_scripts] = 
  {
    "lib_test1.compel", 
    "lib_test2.compel", 
    "lib_test3.compel",
    "lib_test4.compel"
  };

  compel_script_clear_lines(script);

  for (int i=0;i<max_scripts;i++)
    example_internal_script_load_and_show(script, scripts[i]);
}

void example_internal_1(compel_script_t &script)
{
  compel_script_load_file(script, "lib_test1.compel");

  compel_internal(script, compel_internal_setdbgout, 1, 0, 0);

  compel_script_set_lineno(script, 0);
  compel_script_run(script);

  compel_internal(script, compel_internal_writeraw, 0, 0, 0);

  printf("execution ended @ line: %d in (%d msecs)\n", 
    compel_script_get_lineno(script),
    compel_internal(script, compel_internal_exectime, 0, 0, 0)
    );
}