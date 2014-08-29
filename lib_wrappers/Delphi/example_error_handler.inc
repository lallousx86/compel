// the error handler
function script_error_handler(script: compel_script_t; lineno: Integer; liberr: Integer): Integer;stdcall;
var
  err_code: PChar;
begin
  err_code := compel_script_get_line(script, lineno);
  WriteLn('error handler caught error ', liberr, ' @ ', lineno+1, ': >', err_code, '<');
  Result := compel_error_success;
end;

// shows how to install an error handler to handle script errors
procedure example_error_handler(script: compel_script_t);
begin
  compel_script_set_error_handler(script, script_error_handler);
  compel_script_load_lines(script,
    'echoln "okay code";'+
    'some_bad_command;'+
    'echoln "after bad command"',
    ';');
  compel_script_run(script);
end;
