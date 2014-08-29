void example_initialize(compel_script_t &script)
{
  compel_init_t init = {0};
  init.b_usefullns = false;

  init.b_dbgout_script = true;

  // get all the avail extensions
  init.extensions = compel_script_avail_extensions();

  // something like this:
  // "binary_arith;binary_comparison;comments;crt;fileio;include;memory;scopes;shell;unconditional_branching;vars"
  // is returned.

  // initialze the script engine
  script = compel_script_init(&init);
}
