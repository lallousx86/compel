type myclass = class
  private
    Fscript: TCompelScript;
    name: string;
  public
    constructor Create;
    destructor Destroy;override;
    function cb1(Script: TCompelScript;Args: array of string): Integer;
    procedure test1;
end;

{ myclass }
function myclass.cb1(Script: TCompelScript;
  Args: array of string): Integer;
var
  i: Integer;
  Me: myclass;
begin
  Me := TObject(Script.Tag) as myclass;
  writeln('name=', Me.name);
  for i := 0 to Length(Args) - 1 do
    writeln('arg[', i, ']=', args[i]);
  Result := 0;
end;

constructor myclass.Create;
var
  init: compel_init_t;
begin
  FillChar(init, sizeof(init), 0);
  Fscript := TCompelScript.Create(nil, test_client_guid, @init);
  Fscript.LU_Register('x', cb1, 1, 0);
  Fscript.Tag := Self;
end;

destructor myclass.Destroy;
begin
  Fscript.Free;
  inherited;
end;

procedure myclass.test1;
var
  script: compel_script_t;
begin
  script := Fscript.GetCompelScript;
  compel_script_load_lines(script , 'echoln welcome!|x 1 2 3', '|');
  compel_script_run(script);
end;

procedure example_lu_cmd2;
var
  x: myclass;
begin
  x := myclass.Create;
  x.Name := 'haha!';
  x.test1;
  x.Free;
end;

