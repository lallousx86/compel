{$APPTYPE CONSOLE}

program ext_test;

uses Windows, CompelScript, compel_lib;

var
  ext_init: compel_ext_init_t;

function load_ext: Boolean;
var
  h: THandle;
begin
  Result := False;
  h := LoadLibrary('compel_canvas_ext.dll');
  if h = 0 then
    Exit;
    
  ext_init := GetProcAddress(h, 'compel_ext_init');
  if not Assigned(ext_init) then
    Exit;
  Result := True;
end;

var
  script: TCompelScript;
  x: Pointer;
begin
  if not load_ext then
    Exit;
  
  script := TCompelScript.Create(nil);

  ext_init(script.GetCompelScript, x);

//  compel_canvas_ext.compel_ext_init(script, x);
  script.ScriptLoad('test1.compel');
  script.ScriptRunNow;
  
  script.Free;  
end.