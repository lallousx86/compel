library compel_canvas_ext;

uses
  Windows,
  compel_canvas,
  compel_lib;

function compel_ext_init(script: compel_script_t;var ext: TCompelCanvasExt): Integer;stdcall;export;
begin
  ext := TCompelCanvasExt.Create;
  ext.AttachToScript(script);
  ext.RegisterFunctions;
  Result := compel_error_success;
end;

function compel_ext_deinit(script: compel_script_t;ext: TCompelCanvasExt): Integer;stdcall;export;
begin
  ext.Free;
  Result := compel_error_success;
end;

exports
  compel_ext_init,
  compel_ext_deinit;

begin

end.