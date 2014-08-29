unit CompelWatches;

interface

uses Classes, CompelScript;

type

  { Watches items definition }
  TCompelWatchItemType = (witUnknown, witExpression, witValue, witObject);
  PCompelWatchItem = ^TCompelWatchItem;
  TCompelWatchItem = record
    Variable: string;
    Value: string;
    ItemType: TCompelWatchItemType;
  end;

  { Watched items list }
  TCompelWatchList = class(TObject)
  private
    FList: TList;
    Fscript: TCompelScript;
  public
    function FindIndex(const aName: string): Integer;
    property Script: TCompelScript read Fscript write Fscript;
    constructor Create(script: TCompelScript = nil);
    destructor Destroy;override;
    procedure AddItem(const aVariable: string);
    function GetItem(const aIdx: integer;var aVariable, aValue: string): Boolean;
    function GetName(const aIdx: integer): string;
    function Count: Integer;
    function DeleteItem(const aVariable: string): Boolean;overload;
    function DeleteItem(const AIdx: Integer): Boolean;overload;
    procedure DeleteAll;
  end;

implementation

{ TCompelWatchList }

procedure TCompelWatchList.AddItem(const aVariable: string);
var
  item: ^TCompelWatchItem;
begin
  New(item);

  item.Variable := aVariable;
  item.Value := '';
  item.ItemType := witUnknown;

  FList.Add(item);
end;

function TCompelWatchList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCompelWatchList.Create(script: TCompelScript);
begin
  FList := TList.Create;
  Fscript := script;
end;

procedure TCompelWatchList.DeleteAll;
var
  i: Integer;
begin
  for i := 0 to FList.Count-1 do
    Dispose(FList.Items[i]);
  FList.Clear;
end;

function TCompelWatchList.DeleteItem(const aVariable: string): Boolean;
var
  idx: Integer;
begin
  Result := False;

  idx := FindIndex(aVariable);

  if (idx = -1) then
    Exit;

  DeleteItem(idx);

  Result := True;
end;

function TCompelWatchList.DeleteItem(const AIdx: Integer): Boolean;
begin
  Result := False;

  if AIdx > FList.Count then
    Exit;

  Dispose(FList.Items[AIdx]);
  FList.Delete(AIdx);
  Result := True;
end;

destructor TCompelWatchList.Destroy;
begin
  inherited;
  FList.Free;
end;

function TCompelWatchList.FindIndex(const aName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    if PCompelWatchItem(FList.Items[i])^.Variable = aName then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TCompelWatchList.GetItem(
  const aIdx: integer;
  var aVariable, aValue: string): Boolean;
var
  item: ^TCompelWatchItem;
begin
  Result := False;
  try
    item := FList.Items[aIdx];
    if Assigned(Fscript) then
      item.Value := Fscript.ScriptEvaluateExpression(item.Variable)
    else
      item.Value := '<undefined>';  
    aVariable := item.Variable;
    aValue := item.Value;
  except
    Exit;
  end;
  Result := True;
end;

function TCompelWatchList.GetName(const aIdx: integer): string;
begin
  Result := '';
  if aIdx > FList.Count then
    Exit;
  Result := TCompelWatchItem(FList.Items[aIdx]^).Variable;
end;

end.


