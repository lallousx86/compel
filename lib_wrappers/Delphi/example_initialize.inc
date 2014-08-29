procedure example_initialize(var script: compel_script_t);
var
  init: compel_init_t;
begin
  FillChar(init, sizeof(init), 0);
  init.extensions := compel_script_avail_extensions;
  script := compel_script_init(@init);
end;
