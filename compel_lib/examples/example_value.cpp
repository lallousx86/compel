void example_value(compel_script_t &script)
{
  compel_script_interpret_line(script, "var $s value!");

  // find the value "$"
  compel_value_t val = compel_value_find(script, "$s");

  // get the variable's value
  std::string s = compel_value_get(script, val);

  // patch string - enclose in angle brackets
  s = "<" + s + ">";

  // update the variable's value
  compel_value_set(script, val, s.c_str());

  // display the "$s" value
  compel_script_interpret_line(script, "echoln s= $s");

  // create a new variable
  compel_value_create(script, "$el", "elias");

  // show the value of the newly created variable
  compel_script_interpret_line(script, "echoln el = $el");
}
