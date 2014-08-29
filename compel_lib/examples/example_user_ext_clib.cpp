int example_user_ext_clib(compel_script_t &script)
{
  int err;

  err = compel_script_load_lines(script, 
    "compellib.dl ext ext_dll.dll;"
    "var $s \"HElLo WoRld\";"
    "ext_lowercase $s;"
    "echoln \"x=\" $ext_lowercase;"
    ,
    ";"
    );
  err = compel_extension_load(script, "ext", "..\\Debug\\ext_dll.dll");

  err = compel_script_run(script);

  return err;
}
