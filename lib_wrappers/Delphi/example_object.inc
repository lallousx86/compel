procedure example_object(var script: compel_script_t);
var
  i: integer;
  obj: compel_object_t;
  val: compel_value_t; 
  s: string;
begin

  // create OBJ1 using SCRIPT
  compel_script_load_lines(script, 'var $obj1.|var $obj1.name john|var $obj1.lastname doe', '|');
  compel_script_run(script);

  // create another object "obj2"
  obj := compel_object_create(script, '$obj2');

  // create attribute in "obj2"
  for i := 1 to 10 do
  begin
    s := 'attr' + IntToStr(i);
    compel_object_add_attr(script, obj, PChar(s), PChar(s));
  end;

  compel_script_interpret_line(script, 'echoln obj2: $obj2');

  // delete the object "obj2"
  compel_object_destroy(script, '$obj2');

  // find object
  obj := compel_object_find(script, '$obj1');

  // find attribute inside object
  val := compel_object_find_attr(
    script,
    obj,
    'name');

  // adjust the attribute's value
  compel_value_set(script, val, 'elias');

  // show object info
  compel_script_interpret_line(script, 'echoln before_lastname_remove: $obj1');

  // remove an attribute
  compel_object_remove_attr(script, obj, 'lastname');

  // show object info - after removing an attribute
  compel_script_interpret_line(script, 'echoln after_lastname_remove: $obj1');

end;

