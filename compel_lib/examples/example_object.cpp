void example_object(compel_script_t &script)
{
  // create OBJ1 using SCRIPT
  compel_script_load_lines(script, "var $obj1.|var $obj1.name john|var $obj1.lastname doe", "|");
  compel_script_run(script);

  // create another object "obj2"
  compel_object_t obj = compel_object_create(script, "$obj2");

  // create attribute in obj2
  for (int i=0;i<10;i++)
  {
    char attr[20];
    sprintf(attr, "attr%d", i);
    compel_object_add_attr(script, obj, attr, attr);
  }

  compel_script_interpret_line(script, "echoln obj2: $obj2");

  // delete the object "obj2"
  compel_object_destroy(script, "$obj2");

  // adjust an attribute
  obj = compel_object_find(script, "$obj1");
  compel_value_t val = compel_object_find_attr(
    script, 
    obj,
    "name");

  // adjust the attribute's value
  compel_value_set(script, val, "elias");

  // show object info
  compel_script_interpret_line(script, "echoln before_lastname_remove: $obj1");

  // remove an attribute
  compel_object_remove_attr(script, obj, "lastname");

  // show object info - after removing an attribute
  compel_script_interpret_line(script, "echoln after_lastname_remove: $obj1");

  // find another object
  obj = compel_object_find(script, "$_COMPEL");
}

void example_object_2(compel_script_t &script)
{
  compel_script_load_lines(script, "var $o.;var $o.a a;var $o.b b", ";");
  compel_script_run(script);

  char *s;
  s = compel_object_to_string(script, "$o");
  compel_string_destroy(s);

  s = compel_script_evaluate_expression(script, "$o.b . $o.a", false, 0);
  compel_string_destroy(s);

  compel_script_interpret_line(script, "echoln $o");
}