unit CompelWorkspace;

interface

uses Classes, Windows,
  SysUtils, IniFiles,
  CompelScript, CompelUtil;

type
  TCompelWorkspaceDimensions = record
    WindowState: Integer;
    Width, Height: Integer;
    Top, Left: Integer;
  end;


  TCompelWorkspace = class(TObject)
  private
    Fini : TIniFile;
    FFileName: string;
    FListWatches: TStringList;
    FBreakpoints: TCompelBreakpointInfoList;
  public
    constructor Create(const AFileName: string);
    property Watches: TStringList read FListWatches;
    property Breakpoints: TCompelBreakpointInfoList read FBreakpoints;
    function LoadSaveWatches(const ALoad: Boolean): Integer;
    function LoadSaveBreakpoints(const ALoad: Boolean): Integer;
    procedure EraseBreakpointsSettings;
    procedure EraseWatchesSettings;
    destructor Destroy;override;
    function LoadSaveDimensions(
      const ALoad: Boolean;
      const ATag: string;
      const AState: Integer;
      var ADim: TCompelWorkspaceDimensions): Boolean;
  end;

implementation

{ TCompelWorkspace }

const
  str_Watches = 'Watches';
  str_Breakpoints = 'Breakpoints';
  str_Dimensions = 'Dimensions';

constructor TCompelWorkspace.Create(const AFileName: string);
begin
  FFileName := AFileName;
  Fini := TIniFile.Create(AFileName);
  FListWatches := TStringList.Create;
  FBreakpoints := TCompelBreakpointInfoList.Create;
end;

destructor TCompelWorkspace.Destroy;
begin
  Fini.Free;
  FListWatches.Free;
  FBreakpoints.Free;
  if (ComputerFileSize(FFileName) = 0) then
  begin
    SysUtils.DeleteFile(FFileName);
    Exit;
  end;

  if (ComputerFileSize(FFileName) = 0) then
  begin
    SysUtils.DeleteFile(FFileName);
    Exit;
  end;

  inherited;
end;

procedure TCompelWorkspace.EraseBreakpointsSettings;
begin
  FIni.EraseSection(str_Breakpoints);
end;

procedure TCompelWorkspace.EraseWatchesSettings;
begin
  FIni.EraseSection(str_Watches);
end;

function TCompelWorkspace.LoadSaveBreakpoints(
  const ALoad: Boolean): Integer;
var
  i, j: Integer;
  Item: TCompelBreakpointInfo;
  s: string;
begin
  if ALoad then
  begin
    FBreakpoints.Clear;
    i := 0;
    repeat
      s := FIni.ReadString(str_Breakpoints, 'Bp'+IntToStr(i), '');
      if (Length(s) = 0) then
        Break;

      j := Pos(',', s);
      if j = 0 then
        Break;

      // Parse breakpoint info
      Item.LineNo := StrToIntDef(Copy(s, 1, j-1), 0);
      Item.Enabled := Boolean( StrToIntDef(Copy(s, j+1, Length(s)-j), 0) );

      // Insert BP to list
      FBreakpoints.Add(Item);
      Inc(i);
    until False;
    Result := i;
  end else
  begin
    EraseBreakpointsSettings;
    for i := 0 to FBreakpoints.Count - 1 do
    begin
      Item := FBreakpoints[i];
      FIni.WriteString(str_Breakpoints, 'Bp'+IntToStr(i),
        Format('%d,%d',[Item.LineNo, Integer(Item.Enabled)]));
    end;
    Result := FBreakpoints.Count;
  end;
end;

function TCompelWorkspace.LoadSaveDimensions(
  const ALoad: Boolean;
  const ATag: string;
  const AState: Integer;
  var ADim: TCompelWorkspaceDimensions): Boolean;

var
  tok: TCompelTokenizer;
  key: string;
begin
  // Create tokenizer
  tok := TCompelTokenizer.Create;

  // Assume failure
  Result := False;

  // Form key name
  key := Format('%s_%d', [ATag, AState]);

  // Loading dimensions?
  if ALoad and (tok.Parse(FIni.ReadString(str_Dimensions, key, ''), ',') = 5) then
  begin
    ADim.Width     := tok.GetInt(0);
    ADim.Height    := tok.GetInt(1);
    ADim.Top       := tok.GetInt(2);
    ADim.Left      := tok.GetInt(3);
    ADim.WindowState := tok.GetInt(4);

    Result := True;
  end
  // Saving dimensions?
  else if (not ALoad) then
  begin
    FIni.WriteString(str_Dimensions, key,
      Format('%d,%d,%d,%d, %d', [ADim.Width, ADim.Height, ADim.Top, ADim.Left, ADim.WindowState]));
    Result := True;
  end;
  //
  tok.Free;
end;

function TCompelWorkspace.LoadSaveWatches(const ALoad: Boolean): Integer;
var
  i: Integer;
  s: string;
begin
  if ALoad then
  begin
    Watches.Clear;
    Watches.BeginUpdate;
    i := 0;
    while True do
    begin
      s := FIni.ReadString(str_Watches, IntToStr(i), '');
      if (Length(s) = 0) then
        Break;
      Watches.Add(s);
      Inc(i);
    end;
    Watches.EndUpdate;
  end
  else begin
    EraseWatchesSettings;
    for i := 0 to Watches.Count - 1 do
      FIni.WriteString(str_Watches, IntToStr(i), Watches[i]);
  end;
  Result := Watches.Count;
end;

end.
