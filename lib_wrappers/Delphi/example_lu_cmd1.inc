function my_cb1(script: compel_script_t; argc: Integer;argv: TCompel_C_PCharArray): Integer;stdcall;
var
  i: Integer;
begin
  for i := 0 to argc-1 do
  begin
    WriteLn('cb1: arg#',i, ' =', argv[i]);
  end;
  Result := compel_error_success;
end;

function my_cb2(script: compel_script_t; argc: Integer;argv: PChar): Integer;stdcall;
var
  i: Integer;
  s: string;
begin
  for i := 0 to argc-1 do
  begin
    s := PChar(Pointer(argv)^);
    Inc(argv, 4);
    WriteLn('cb2: arg#',i, ' =', s);
  end;
  Result := compel_error_success;
end;

procedure example_lu_cmd1(var script: compel_script_t);
begin
  compel_lu_cmd_register3(script, my_cb1, 'ml', 1, 0);
  compel_lu_cmd_register2(script, my_cb2, 'mu', 1, 0);
  compel_script_load_lines(script, 'echoln welcome!|ml 1 2 3 4|mu 4 3 2 1', '|');
  compel_script_run(script);
end;

