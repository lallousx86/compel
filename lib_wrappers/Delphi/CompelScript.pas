unit CompelScript;

{ Last updated on 05/31/2006 }

interface

uses Classes, compel_lib, SysUtils, Windows, CompelUtil, Messages;

const
  // COMPEL messages base
  WM_COMPEL_BASE              = Messages.WM_APP;
  
  WM_COMPEL_ONSCRIPTSUSPENDED = WM_COMPEL_BASE + 1;
  WM_COMPEL_ONSCRIPTFINISHED  = WM_COMPEL_BASE + 2;

  // Use messages from this constants and up
  WM_COMPEL_LAST              = WM_COMPEL_BASE + 100;
  
type

  { Types of breakpoints }
  TCompelBPTypes = (bpTemp, bpNormal);

  { Breakpoint item definition }
  PCompelBreakpointItem = ^TCompelBreakpointItem;
  TCompelBreakpointItem = class(TObject)
  private
    FBptType: TCompelBPTypes;
    FEnabled: Boolean;
    FHits: Integer;
    FLineNo: Integer;
//    FExpression: string;
  public
    procedure SetType(const AType: TCompelBPTypes);
    constructor Create(const AType: TCompelBPTypes = bpNormal);
    destructor Destroy;override;
    function IsHit(const AEPLine: Integer): Boolean;
    property Hits: Integer read FHits write FHits;
    function GetLineNo: Integer;
    property Enabled : Boolean read FEnabled write FEnabled;
    procedure SetLine(const ALine: Integer);
  end;

  PCompelBreakpointInfo = ^TCompelBreakpointInfo;
  TCompelBreakpointInfo = record
    LineNo: Integer;
    Enabled: Boolean;
  end;

  TCompelBreakpointInfoList = class
  private
    FList: TList;

    function Get(Index: Integer): TCompelBreakpointInfo;
    procedure Put(Index: Integer;Info: TCompelBreakpointInfo);
  public
    constructor Create;
    function Count: Integer;
    destructor Destroy;override;
    function Add(var AItem: TCompelBreakpointInfo): PCompelBreakpointInfo;overload;
    function Add(const ALineNo: Integer; const AEnabled: Boolean): PCompelBreakpointInfo;overload;
    procedure Clear;
    property Items[Index: Integer]: TCompelBreakpointInfo read Get write Put; default;
    function Delete(Index: Integer): Boolean;
  end;

  { Breakpoint item definition }
  PCompelBreakpointList = ^TCompelBreakpointList;
  TCompelBreakpointList = class(TObject)
  private
    FList: TList;

    FCrit: TRTLCriticalSection;

    procedure Protect(const AStart: Boolean = True);

    function Get(Index: Integer): TCompelBreakpointItem;
  public

    function FindBpIndex(const ALineNo: Integer): Integer;

    // Finds a BPItem @ Line
    function FindBpItem(const ALineNo: Integer): TCompelBreakpointItem;

    // Creates or destroys a breakpoint
    function ToggleBP(const ALineNo: Integer;var Idx: Integer): Boolean;

    // Constructor
    constructor Create;

    // Returns breakpoint information
    function GetBPInfo(const AIndex: Integer; var AInfo: TCompelBreakpointInfo): Boolean;

    // Checks if any BP hits this line
    function IsHit(const AEPLine: Integer): Integer;

    // Adds or updates a BP at this line
    function AddBP(const ALineNo: Integer;
      const AType: TCompelBPTypes = bpNormal;
      const AEnabled: Boolean = True): Boolean;

    // Deletes a BP by index
    function Delete(const AIndex: Integer): Boolean;

    // Returns the count of breakpoints
    function Count: Integer;

    property Items[Index: Integer]: TCompelBreakpointItem read Get;default;
    procedure Clear;
    function DeleteBpByLine(const ALineNo: Integer): Boolean;
    procedure GetBptsLines(ALines: TList);
    destructor Destroy;override;
  end;

  TCompelRuntimeEventCode =
  (
    rteRun,
    rteStepNext,
    rteSingleStep,
    rteExitThread,
    rtePause,
    rteNothing
  );

  PCompelRuntimeEventQueueItem = ^TCompelRuntimeEventQueueItem;
  TCompelRuntimeEventQueueItem = record
    Code: TCompelRuntimeEventCode;
  end;

  TCompelScript = class;

  PCompelScriptLUCallback = ^TCompelScriptLUCallback;
  TCompelScriptLUCallback = function(script: TCompelScript;Args: array of string): Integer of object;

  TCompelRuntimeEventQueue = class(TObject)
  private
    FList: TList;
    F_hSempahore: THandle;
    Fq: TCompelRuntimeEventQueueItem;
  public
    constructor Create;
    procedure Queue(const ACode: TCompelRuntimeEventCode);
    function Pop: PCompelRuntimeEventQueueItem;
    function Count: Integer;
    procedure ClearAll;
    function  PopWait(const ATime: Dword = Windows.INFINITE): PCompelRuntimeEventQueueItem;
    destructor Destroy;override;
  end;

  { COMPEL_LIB wrapper }
  TCompelScript = class
  private
    FScriptLines: TStringList;
    FRawScriptLines: TStringList;
    Fscript: compel_script_t;
    FOwner: TObject;
    FRtQ: TCompelRuntimeEventQueue;
    FGuid: string;
    // Temporary breakpoint (for RunToCursor and StepNext)
    FTempBpt: TCompelBreakpointItem;

    F_hThread: THandle;

    FbFreeScriptHandle: Boolean;

    FNotifyWindow: HWnd;

    FBpts: TCompelBreakpointList;

    FLastScriptError: Integer;
    FTag: Pointer;

    FLastHitLine, FLastFaultLine: Integer;

    Ferror_handler: compel_script_error_handler_cb_t;
    FWrappedLU: TList;
    
    function  ScriptRunThreadProc: Integer;
    procedure DoOnSuspended(Sender: TObject);
    procedure DoOnRunFinished(Sender: TObject);

    function IsBpHit(const AEPLine: Integer): Boolean;
    procedure CommonConstructor;

    function GetScriptContext(AString: string): compel_user_context_t;
    procedure SetScriptContext(AString: string;
      const Value: compel_user_context_t);

  public
    OnlyRawLines: Boolean;

    property ScriptContext[AString: string]: compel_user_context_t read GetScriptContext write SetScriptContext;

    property Tag: Pointer read FTag write FTag;

    class function FromScript(
      const ARawScript: compel_script_t;
      const AGuid: string): TCompelScript;

    class function FromScriptContext(
      const ARawScript: compel_script_t;
      const AGuid: string): TCompelScript;

    constructor Create(AOwner: TObject;
      const AGuid: string;
      const AInit: p_compel_init_t = nil);overload;
    constructor Create;overload;

    destructor Destroy;override;

    function RefreshScriptLines: Boolean;

    property Breakpoints: TCompelBreakpointList read FBpts write FBpts;

    property GetCompelScript: compel_script_t read Fscript;

    function ScriptLoad(const aFileName: string): Integer;

    function InterpretLine(const ALine: string): Integer;
    function ScriptGetLines: TStringList;
    function ScriptGetRawLines: TStringList;

    function ScriptGetLinesCount: Integer;
    procedure ScriptSetLineNo(const aLineNo: Integer);
    procedure ScriptSetLineCode(const Index: Integer;const ACode: string);

    function  ScriptGetLastError: Integer;
    function  ScriptGetPreparseFailingLine: Integer;
    function  ScriptEvaluateExpression(const AExpression: string;
      bKeepQuotes : Boolean = false;
      chDelim: Integer = 0): string;

    function  ScriptGetLineNo: Integer;
    function  ScriptGetLineCode(const LineNo: Integer): string;

    class function GetErrorString(const aErrNo: Integer): string;

    function ScriptGetFlowInfo(
      var aLineNo: integer;
      var aLineStr, aRawLineStr: string): Boolean;

    function ObjectGetValue(const AObjName: string;var AValue: string): Boolean;

    function ScriptInitRuntime: Boolean;

    function ScriptStepNext: Boolean;
    function ScriptRunNow: Integer;
    function ScriptRun: Boolean;
    function ScriptSingleStep: Boolean;
    function ScriptSingleStepNow: Integer;
    function ScriptStop: Boolean;
    function ScriptStopNow: Boolean;
    function ScriptPause: Boolean;
    function ScriptRunToLine(const ALineNo: Integer): Boolean;
    procedure WaitForRuntimeThread(const AWait: Cardinal = Windows.INFINITE);
    procedure SetNotifyWindow(AHwnd: Hwnd);
    procedure SetErrorHandler(cb: compel_script_error_handler_cb_t);

    class function ParseNumber(const AStr: string): Integer;
    function  LU_Register(const aCmdName: string;ACallBack: TCompelScriptLUCallback;const AMin, AMax: Integer): Integer;
    procedure LU_SetRetVal(const ARetVal: string);
  end;

  TCompelValue = class (TObject)
  private
    FRawScript: compel_script_t;
    FCompelScript: TCompelScript;
    FValue: compel_value_t;
    FValueName: string;

    procedure SetValue(const AVal: string);
    function  GetValue: string;
    function  GetIntValue: Integer;
    procedure SetIntValue(const Value: Integer);
  public
    constructor Create;
    function Refresh: Boolean;
    class function FindValue(AScript: TCompelScript;const AName: string): TCompelValue;
    property Value: string read GetValue write SetValue;
    property IntValue: Integer read GetIntValue write SetIntValue;
  end;

  TCompelTokenizer = class(TObject)
  private
    Ftok: compel_tokenizer_t;
    function GetItems(Index: Integer): string;
    procedure SetItems(Index: Integer; const Value: string);
  public
    constructor Create;
    function Count: Integer;
    function Parse(const AStr, ADelim: string;const AQuotes : string = ''; const AEscape: string = ''): Integer;
    destructor Destroy;override;
    property Items[Index: Integer]: string read GetItems write SetItems;default;
    function GetInt(Index: Integer): Integer;
    function Join(ASep: string = ','; AFrom: Integer = 0; ATo: Integer = 0): string;
  end;
implementation

uses Math;

{ TCompelScript }

function _ScriptRunThreadProc(Self: TCompelScript): Dword;stdcall;forward;

constructor TCompelScript.Create;
begin
  inherited;
end;

// Builds a script instance from an existing script handle
class function TCompelScript.FromScript(
  const ARawScript: compel_script_t;
  const AGuid: string): TCompelScript;
var
  _Self: TCompelScript;
begin
  // Create a new instance
  _Self := TCompelScript.Create;

  // Don't free the script object because we are just a link to it
  _Self.FbFreeScriptHandle := False;

  _Self.FGuid := AGuid;
  
  // Assign passed script handle
  _Self.Fscript := ARawScript;
  // Initialize other stuff
  _Self.CommonConstructor;

  _Self.RefreshScriptLines;

  Result := _Self;
end;

procedure TCompelScript.CommonConstructor;
begin
  // Create script lines
  FScriptLines := TStringList.Create;
  FRawScriptLines := TStringList.Create;

  // Clear the thread handle
  F_hThread := 0;

  // Create debugging thread queue
  FRtQ := TCompelRuntimeEventQueue.Create;

  // Create temporary breakpoint
  FTempBpt := TCompelBreakpointItem.Create(bpTemp);

  // Clear notify window
  FNotifyWindow := 0;

  // Reset breakpoints list pointer
  FBpts := nil;

  FLastScriptError := compel_error_success;

  OnlyRawLines := False;

  ScriptContext[FGuid] := Pointer(Self);

  FWrappedLU := TList.Create;
end;

constructor TCompelScript.Create(
  AOwner: TObject;
  const AGuid: string;
  const AInit: p_compel_init_t);
var
  init: compel_init_t;
begin
  // Initialization structure passed?
  if Assigned(AInit) then
  begin
    init := AInit^;
  end else
  begin
    FillChar(init, sizeof(init), 0);
  end;

  // Extensions passed?
  if not Assigned(init.extensions) then
    init.extensions := compel_script_avail_extensions;

  // Create compel_lib object
  Fscript := compel_script_init(@init);


  // Pass the owner
  FOwner := AOwner;

  FGuid := AGuid;

  CommonConstructor;

  FbFreeScriptHandle := True;
end;

destructor TCompelScript.Destroy;
begin
  inherited Destroy;

  FScriptLines.Free;
  FRawScriptLines.Free;
  FRtQ.Free;
  FTempBpt.Free;

  while FWrappedLU.Count <> 0 do
  begin
    Dispose(FWrappedLU.First);
    FWrappedLU.Delete(0);
  end;
  FWrappedLU.Free;
  
  WaitForRuntimeThread(1000);

  if FbFreeScriptHandle and (Fscript<>0) then
    compel_script_deinit(Fscript);
end;

function TCompelScript.ScriptGetFlowInfo(var aLineNo: integer;
  var aLineStr, aRawLineStr: string): Boolean;
begin
  aLineNo     := compel_script_get_lineno(Fscript);
  if (aLineNo >= ScriptGetLinesCount) then
  begin
    Result := False;
    Exit;
  end;

  aLineStr    := FScriptLines.Strings[aLineNo];
  aRawLineStr := compel_script_get_line(Fscript, aLineNo);
  Result      := True;
end;

function TCompelScript.ScriptGetLines: TStringList;
begin
  Result := FScriptLines;
end;

function TCompelScript.ScriptGetRawLines: TStringList;
begin
  Result := FRawScriptLines;
end;

function TCompelScript.ScriptLoad(const aFileName: string): Integer;
var
  fn: string;
  delta: Integer;
begin
  try
    // Load lines into string list
    FScriptLines.LoadFromFile(aFileName);

    // Clear previous lines
    compel_script_clear_lines(Fscript);

    //
    Result := compel_script_load_file(Fscript, PChar(aFileName));
    if Result <> compel_error_success then
      Exit;

    fn := CompelUtil.ComputeTempFileName('compel');
    if compel_internal(Fscript, compel_internal_writeraw, Integer(PChar(fn)), 0, 0) = 1 then
    begin
      FRawScriptLines.LoadFromFile(fn);
      Delta := Abs(FRawScriptLines.Count - FScriptLines.Count);
      OnlyRawLines := Delta > 3;
      if OnlyRawLines then
        FScriptLines.Assign(FRawScriptLines);
    end;

    SysUtils.DeleteFile(fn);

    Result := compel_error_success;
  except
    Result := compel_error_syntax_error;
    Exit;
  end;
end;

procedure TCompelScript.ScriptSetLineNo(const aLineNo: Integer);
begin
  compel_script_set_lineno(Fscript, aLineNo);
  FLastFaultLine := -1;
  FLastHitLine := -1;
end;

function TCompelScript.ScriptSingleStepNow: Integer;
begin
  Result := compel_script_step(Fscript);
  if Result = compel_error_no_operation then
    Result := compel_error_success;
end;

class function TCompelScript.GetErrorString(const aErrNo: Integer): string;
begin
  Result := compel_error_code_to_string(aErrNo);
end;

function TCompelScript.GetScriptContext(AString: string): compel_user_context_t;
begin
  Result := compel_script_context_get(Fscript, PChar(AString));
end;

function TCompelScript.ScriptGetLinesCount: Integer;
begin
  Result := compel_script_get_lines_count(Fscript);
end;

function TCompelScript.ScriptGetPreparseFailingLine: Integer;
begin
  Result := compel_internal(FScript, compel_internal_preparse_failing_line, 0, 0, 0);
end;

function TCompelScript.ScriptGetLineNo: Integer;
begin
  Result := compel_script_get_lineno(Fscript);
end;

function TCompelScript.ObjectGetValue(const AObjName: string;
  var AValue: string): Boolean;
var
  s: PChar;
begin
  Result := False;
  s := compel_object_to_string(Fscript, PChar(AObjName));
  if not Assigned(s) then
    Exit;
  AValue := s;
  compel_string_destroy(s);  
  Result := True;
end;

class function TCompelScript.ParseNumber(const AStr: string): Integer;
begin
  Result := compel_parse_number(PChar(AStr));
end;

function TCompelScript.RefreshScriptLines: Boolean;
var
  fn: string;
begin
  // Get a temp file name
  fn := CompelUtil.ComputeTempFileName('compel');
  // Get the loaded script's lines into a file
  if compel_internal(Self.Fscript, compel_internal_writeraw, Integer(PChar(fn)), 0, 0) = 1 then
  begin
    // Load parsed lines
    Self.FRawScriptLines.LoadFromFile(fn);

    // In this case, non-parsed lines = parsed lines
    Self.FScriptLines.Assign(Self.FRawScriptLines);

    // Delete parsed (temp) script file
    SysUtils.DeleteFile(fn);

    Result := True;
  end
  else
    Result := False;
end;

function TCompelScript.ScriptEvaluateExpression(const AExpression: string;
      bKeepQuotes : Boolean = false;
      chDelim: Integer = 0): string;
var
  s: PChar;
begin
  Result := '';
  s := compel_script_evaluate_expression(Fscript, PChar(AExpression), Integer(bKeepQuotes), chDelim);
  if not Assigned(s) then
    Exit;
  Result := s;
  compel_string_destroy(s);
end;

function TCompelScript.ScriptInitRuntime: Boolean;
var
  tid: Cardinal;
begin
  // Already initialized?
  if (F_hThread <> 0) then
  begin
    Result := True;
    Exit;
  end;
  
  F_hThread := Windows.CreateThread(nil, 0, @_ScriptRunThreadProc, Self, 0, tid);
  if (F_hThread = 0) then
  begin
    Result := False;
    Exit;
  end;  
  Result := True;
end;

function TCompelScript.ScriptStepNext: Boolean;
begin
  // Set temp BP on next instruction
  FTempBpt.SetLine(compel_script_get_lineno(Fscript) + 1);
  FTempBpt.Enabled := True;

  // Run the script
  Result := Self.ScriptRun;
end;

function _ScriptRunThreadProc(Self: TCompelScript): Dword;stdcall;
begin
  Result := Self.ScriptRunThreadProc;
end;

function TCompelScript.ScriptRunThreadProc: Integer;
var
  err: Integer;
  q: PCompelRuntimeEventQueueItem;
  CurLine: Integer;
begin
  err := compel_error_success;

  FLastHitLine := -1;
  FLastFaultLine := -1;

  while (err = compel_error_success) do
  begin
    // Wait for an event
    q := FRtQ.PopWait;

    // No event? Wait more
    if (q = nil) then
      Continue;

    // Exit thread?
    if (q.Code = rteExitThread) then
      Break;

    // Get current ExecPoint
    CurLine := Self.ScriptGetLineNo;

    // Check temporary breakpoint
    if FTempBpt.IsHit(CurLine) then
    begin
      // Cause suspend
      q.Code := rteNothing;
      FLastHitLine := CurLine;
    end
    else if (FLastHitLine <> CurLine) and IsBpHit(CurLine) then
    begin
      q.Code := rteNothing;
      FLastHitLine := CurLine;
    end else
    begin
      // Reset last hit line
      FLastHitLine := -1;
    end;

    if (q.Code = rtePause) then
    begin
      // Clear the queue to give space for PAUSE
      FRtQ.ClearAll;
      q.Code := rteNothing;
    end else
    // Run?
    if (q.Code = rteRun) then
    begin
      // Run is a series of steps till the end of script is reached
      err := ScriptSingleStepNow;

      // No error? then run more
      if (err = compel_error_success) then
        Self.ScriptRun;
    end
    else if (q.Code = rteSingleStep) then
    begin
      // Do a single step
      err := ScriptSingleStepNow;
    end;

    FLastScriptError := err;

    if (err <> compel_error_script_stopped) and
       (err <> compel_error_success) then
    begin
      if FLastFaultLine <> CurLine then
      begin
        err := Ferror_handler(Fscript, CurLine, err)
      end
      else begin
        // Skip faulty line
        Self.ScriptSetLineNo(CurLine+1);
        err := compel_error_success;
      end;
      FLastFaultLine := CurLine;
    end
    else begin
      FLastFaultLine := -1;
    end;

    // No more events? Signal OnSuspend
    if (err = compel_error_success) then
    begin
      if (FRtQ.Count = 0) then
        DoOnSuspended(Self);
    end;
  end;

//  if (err = compel_error_script_stopped) then
//    DoOnRunFinished(Self);
  DoOnRunFinished(Self);
  
  Result := 0;
end;

procedure TCompelScript.DoOnRunFinished(Sender: TObject);
begin
  if FNotifyWindow = 0 then
    Exit;

  Windows.PostMessage(FNotifyWindow, WM_COMPEL_ONSCRIPTFINISHED, Integer(Sender), 0);
end;

procedure TCompelScript.DoOnSuspended(Sender: TObject);
begin
  if FNotifyWindow = 0 then
    Exit;
  Windows.PostMessage(FNotifyWindow, WM_COMPEL_ONSCRIPTSUSPENDED, Integer(Sender), 0);
end;

function TCompelScript.ScriptRun: Boolean;
begin
  FRtQ.Queue(rteRun);
  Result := True;
end;

function TCompelScript.ScriptRunNow: Integer;
begin
  Result := compel_script_run(Fscript);
end;

function TCompelScript.ScriptSingleStep: Boolean;
begin
  FRtQ.Queue(rteSingleStep);
  Result := True;
end;

function TCompelScript.ScriptRunToLine(const ALineNo: Integer): Boolean;
begin
  FTempBpt.SetLine(ALineNo);
  FTempBpt.Enabled := True;
  Result := Self.ScriptRun;
end;

// Tells the runtime system to stop script execution
function TCompelScript.ScriptStop: Boolean;
begin
  FRtQ.Queue(rteExitThread);
  Result := True;
end;

function TCompelScript.ScriptPause: Boolean;
begin
  FRtQ.Queue(rtePause);
  Result := True;
end;

procedure TCompelScript.WaitForRuntimeThread(const AWait: Cardinal);
begin
  // Thread assigned?
  if (F_hThread <> 0) then
  begin
    // Try to wait for the thread
    if Windows.WaitForSingleObject(F_hThread, AWait) <> Windows.WAIT_OBJECT_0 then
    begin
      // Wait timed out? Kill the thread
      Windows.TerminateThread(F_hThread, 2);
    end;
    Windows.CloseHandle(F_hThread);
  end;
  F_hThread := 0;  
end;

function TCompelScript.ScriptStopNow: Boolean;
begin
  // Queue STOP request
  if not ScriptStop then
  begin
    Result := False;
    Exit;
  end;

  // Wait for thread to terminate
  WaitForRuntimeThread;

  Result := True;
end;

procedure TCompelScript.SetNotifyWindow(AHwnd: Hwnd);
begin
  FNotifyWindow := AHwnd;
end;


procedure TCompelScript.SetScriptContext(AString: string;
  const Value: compel_user_context_t);
begin
  compel_script_context_set(Fscript, PChar(AString), Value);
end;

function TCompelScript.IsBpHit(const AEPLine: Integer): Boolean;
var
  nBp: Integer;
begin
  Result := False;

  // Do we have breakpoints list?
  if not Assigned(FBpts) then
    Exit;

  // Is BP hit?
  nBp := FBpts.IsHit(AEPLine);
  
  if nBp = -1 then
    Exit;

  // Yes, hit  
  Result := True;  
end;


function TCompelScript.ScriptGetLastError: Integer;
begin
  Result := FLastScriptError;
end;

function TCompelScript.ScriptGetLineCode(const LineNo: Integer): string;
begin
  Result := compel_script_get_line(Fscript, LineNo);
end;

procedure TCompelScript.ScriptSetLineCode(const Index: Integer;
  const ACode: string);
begin
  compel_script_set_line(Fscript, Index, PChar(ACode));
end;

class function TCompelScript.FromScriptContext(
  const ARawScript: compel_script_t;
  const AGuid: string): TCompelScript;
begin
  Result := TCompelScript(compel_script_context_get(ARawScript, PChar(AGuid)));
end;

procedure TCompelScript.SetErrorHandler(
  cb: compel_script_error_handler_cb_t);
begin
  compel_script_set_error_handler(Fscript, cb);
  Ferror_handler := cb;
end;


type
  pcompel_script_lu_wrapper_t = ^compel_script_lu_wrapper_t;
  compel_script_lu_wrapper_t = record
    Self: TCompelScript;
    cb: TCompelScriptLUCallback;
  end;

function compel_script_lu_obj_wrapper(script: compel_script_t; argc: Integer;argv: PChar): Integer;stdcall;
var
  i: Integer;
  args: array of string;
  info: p_lib_usercommand_info_t;
  wrapper: pcompel_script_lu_wrapper_t;
begin
  info := compel_lu_cmd_get_info(script);
  wrapper := info.context;
  
  SetLength(args, argc);

  for i := 0 to argc-1 do
  begin
    args[i] := PChar(Pointer(argv)^);
    Inc(argv, SizeOf(PChar));
  end;

  i := wrapper.cb(wrapper.Self, args);

  Finalize(args);

  Result := i;
end;

function TCompelScript.LU_Register(const ACmdName: string;
  ACallBack: TCompelScriptLUCallback; const AMin, AMax: Integer): Integer;
var
  info: lib_usercommand_info_t;
  wrapper: pcompel_script_lu_wrapper_t;
begin
  info.name := PChar(ACmdName);
  info.minargs := AMin;
  info.maxargs := AMax;
  info.desc := nil;
  info.cb := compel_script_lu_obj_wrapper;

  New(wrapper);
  FWrappedLU.Add(wrapper);
  
  wrapper.Self := Self;
  wrapper.cb := ACallBack;
  info.context := wrapper;
  Result := compel_lu_cmd_register(Fscript, @info);
end;

procedure TCompelScript.LU_SetRetVal(const ARetVal: string);
begin
  compel_lu_cmd_set_retval(Fscript, PChar(ARetVal));
end;

function TCompelScript.InterpretLine(const ALine: string): Integer;
begin
  Result := compel_script_interpret_line(Fscript, PChar(ALine));
end;

{ TCompelRuntimeEventQueue }

function TCompelRuntimeEventQueue.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCompelRuntimeEventQueue.Create;
begin
  FList := TList.Create;
  F_hSempahore := Windows.CreateSemaphore(nil, 0, $7FFFFFFF, nil);
end;

destructor TCompelRuntimeEventQueue.Destroy;
begin
  ClearAll;
  FList.Free;
  Windows.CloseHandle(F_hSempahore);
  inherited;
end;

procedure TCompelRuntimeEventQueue.ClearAll;
var
  i: Integer;
begin

  // Free the allocated items
  for i := 0 to Count - 1 do
    Dispose(FList[i]);
    
  // Clear the list  
  FList.Clear;
end;

function TCompelRuntimeEventQueue.Pop: PCompelRuntimeEventQueueItem;
begin

  if Count = 0 then
  begin
    Result := nil;
    Exit;
  end;

  Fq := PCompelRuntimeEventQueueItem(FList.First)^;

  Dispose(FList.First);

  FList.Delete(0);
  
  Result := @Fq;
end;

function TCompelRuntimeEventQueue.PopWait(
  const ATime: Dword): PCompelRuntimeEventQueueItem;
begin
  Result := nil;
  if Windows.WaitForSingleObject(F_hSempahore, ATime) <> WAIT_OBJECT_0 then
    Exit;
  Result := Self.Pop;  
end;

procedure TCompelRuntimeEventQueue.Queue(
  const ACode: TCompelRuntimeEventCode);
var
  q: PCompelRuntimeEventQueueItem;
begin
  New(q);
  q.Code := ACode;
  FList.Add(q);
  Windows.ReleaseSemaphore(F_hSempahore, 1, nil);
end;

{ TCompelBreakpointItem }

constructor TCompelBreakpointItem.Create(const AType: TCompelBPTypes);
begin
  Self.FHits := 0;
  Self.FEnabled := False;
  Self.SetType(AType);
end;

destructor TCompelBreakpointItem.Destroy;
begin

  inherited;
end;

function TCompelBreakpointItem.GetLineNo: Integer;
begin
  Result := FLineNo;
end;

function TCompelBreakpointItem.IsHit(const AEPLine: Integer): Boolean;
begin
  if Self.Enabled and (AEPLine = Self.GetLineNo) then
  begin
    Inc(Self.FHits);
    Result := True;

    // Disable breakpoint if it was of temporary type
    if FBptType = bpTemp then
    begin
      Self.Enabled := False;
    end;
  end
  else
  begin
    Result := False;
  end;
end;

procedure TCompelBreakpointItem.SetLine(const ALine: Integer);
begin
  Self.FLineNo := ALine;
end;

procedure TCompelBreakpointItem.SetType(const AType: TCompelBPTypes);
begin
 FBptType := AType;
end;

{ TCompelBreakpointList }

function TCompelBreakpointList.AddBP(const ALineNo: Integer;
  const AType: TCompelBPTypes;
  const AEnabled: Boolean): Boolean;
var
  bp: TCompelBreakpointItem;
begin
  // Begin protection
  Protect;

  // Find previous BPs with this line
  bp := Self.FindBpItem(ALineno);

  // Did we find an existing BP?
  if not Assigned(bp) then
  begin
    // Create new breakpoint
    bp := TCompelBreakpointItem.Create(AType);
  end;

  // Set the line no
  bp.SetLine(ALineNo);

  // Enable or Disable
  bp.Enabled := AEnabled;

  // Insert BP into the list
  FList.Add(bp);

  // End protection
  Protect(False);

  Result := True;
end;

function TCompelBreakpointList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCompelBreakpointList.Create;
begin
  FList := TList.Create;
  Windows.InitializeCriticalSection(FCrit);
end;

procedure TCompelBreakpointList.Clear;
var
  i: Integer;
begin
  Protect;

  for i := 0 to Count - 1  do
    TCompelBreakpointItem(FList[i]).Free;

  // Clear the list
  FList.Clear;

  Protect(False);
end;

function TCompelBreakpointList.Delete(const AIndex: Integer): Boolean;
begin
  //
  if AIndex >= Count then
  begin
    Result := False;
    Exit;
  end;

  Protect;

  // Free item
  TCompelBreakpointItem(FList[AIndex]).Free;

  // Delete list item
  FList.Delete(AIndex);

  Protect(False);

  Result := True;
end;

function TCompelBreakpointList.DeleteBpByLine(
  const ALineNo: Integer): Boolean;
var
  idx: Integer;
begin
  idx := FindBpIndex(ALineNo);
  if (idx = -1) then
  begin
    Result := False;
    Exit;
  end;
  Result := Delete(idx);
end;

destructor TCompelBreakpointList.Destroy;
begin
  // Delete all breakpoints
  Clear;

  // Free BP items list
  FList.Free;

  // Delete critical section
  Windows.DeleteCriticalSection(FCrit);

  inherited;
end;

function TCompelBreakpointList.FindBpIndex(
  const ALineNo: Integer): Integer;
var
  i: Integer;
  bp: TCompelBreakpointItem;
begin
  Protect;
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    bp := TCompelBreakpointItem(FList[i]);
    if (bp.GetLineNo = ALineNo) then
    begin
      Result := i;
      Break;
    end;
  end;
  Protect(False);
end;

function TCompelBreakpointList.GetBPInfo(const AIndex: Integer;
  var AInfo: TCompelBreakpointInfo): Boolean;
var
  bp: TCompelBreakpointItem;  
begin
  if AIndex >= Count then
  begin
    Result := False;
    Exit;
  end;

  bp := TCompelBreakpointItem(FList[AIndex]);

  AInfo.LineNo := bp.GetLineNo;
  AInfo.Enabled := bp.Enabled;

  Result := True;
end;

procedure TCompelBreakpointList.GetBptsLines(ALines: TList);
var
  i: Integer;
  bp: TCompelBreakpointItem;
begin
  Protect;

  ALines.Clear;
  for i := 0 to Count - 1 do
  begin
    bp := TCompelBreakpointItem(FList[i]);
    ALines.Add(Pointer(bp.GetLineNo));
  end;
  Protect(False);
end;

procedure TCompelBreakpointList.Protect(const AStart: Boolean);
begin
  if AStart then
    Windows.EnterCriticalSection(FCrit)
  else
    Windows.LeaveCriticalSection(FCrit);
end;

function TCompelBreakpointList.IsHit(const AEPLine: Integer): Integer;
var
  bp: TCompelBreakpointItem;
  idx: Integer;
begin
  Result := -1;

  // Do we have a BP with this line?
  idx := FindBpIndex(aEPLine);
  if idx = -1 then
    Exit;

  bp := TCompelBreakpointItem(FList[idx]);
  if not Assigned(bp) then
    Exit;

  // BP hits?
  if not bp.IsHit(AEPLine) then
    Exit;

  Result := idx; 
end;

function TCompelBreakpointList.FindBpItem(
  const ALineNo: Integer): TCompelBreakpointItem;
var
  idx: Integer;
begin
  idx := FindBpIndex(ALineNo);
  if idx = -1 then
  begin
    Result := nil;
    Exit;
  end;
  Result := TCompelBreakpointItem(FList[idx]);
end;

function TCompelBreakpointList.ToggleBP(const ALineNo: Integer;
  var Idx: Integer): Boolean;
begin
  // Do we have a BP there?
  Idx := FindBpIndex(ALineNo);

  Protect;

  // No breakpoint, we need to create one
  if Idx = - 1 then
  begin
    Idx := Count;
    Result := AddBP(ALineNo);
  end else
  begin
    Result := Delete(Idx);
    Idx := -1;
  end;
  
  Protect(False);
end;

function TCompelBreakpointList.Get(Index: Integer): TCompelBreakpointItem;
begin
  Result := TCompelBreakpointItem(FList[Index]);
end;

{ TCompelValue }

constructor TCompelValue.Create;
begin
  FRawScript := 0;
  FCompelScript := nil;
  Fvalue := 0;
end;

class function TCompelValue.FindValue(AScript: TCompelScript;const AName: string): TCompelValue;
var
  script: compel_script_t;
  v: compel_value_t;
  _Self: TCompelValue;
begin
  Result := nil;

  // Get the RAW script handle
  script := AScript.GetCompelScript;
  if script = 0 then
    Exit;

  // can we find a value?
  v := compel_value_find(script, PChar(AName));
  if v = 0 then
    Exit;

  _Self := TCompelValue.Create;
  _Self.FValueName := AName;
  _Self.FRawScript := script;
  _Self.FCompelScript := AScript;
  _Self.Fvalue := v;

  Result := _Self;
end;

function TCompelValue.GetIntValue: Integer;
begin
  Result := TCompelScript.ParseNumber(Self.GetValue);
end;

function TCompelValue.GetValue: string;
begin
  Result := compel_value_get(FRawScript, FValue);
end;

function TCompelValue.Refresh: Boolean;
begin
  FValue := compel_value_find(FRawScript, PChar(FValueName));
  if FValue = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

procedure TCompelValue.SetIntValue(const Value: Integer);
begin
  Self.Value := IntToStr(Value);
end;

procedure TCompelValue.SetValue(const AVal: string);
begin
  compel_value_set(FRawScript, FValue, PChar(AVal));
end;

{ TCompelBreakpointInfoList }

function TCompelBreakpointInfoList.Add(const ALineNo: Integer;
  const AEnabled: Boolean): PCompelBreakpointInfo;
var
  Item: TCompelBreakpointInfo;
begin
  Item.LineNo := ALineNo;
  Item.Enabled := AEnabled;
  Result := Add(Item);
end;

function TCompelBreakpointInfoList.Add(
  var AItem: TCompelBreakpointInfo): PCompelBreakpointInfo;
var
  pItem: PCompelBreakpointInfo;
begin
  New(pItem);
  pItem^ := AItem;
  FList.Add(pItem);
  Result := pItem;
end;

procedure TCompelBreakpointInfoList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Delete(i);
  FList.Clear;  
end;

function TCompelBreakpointInfoList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCompelBreakpointInfoList.Create;
begin
  FList := TList.Create;
end;

function TCompelBreakpointInfoList.Delete(Index: Integer): Boolean;
begin
  if Index >= Count then
  begin
    Result := False;
    Exit;
  end;
  FList.Delete(Index);
  Result := True;
end;

destructor TCompelBreakpointInfoList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TCompelBreakpointInfoList.Get(
  Index: Integer): TCompelBreakpointInfo;
begin
  if Index >= Count then
    Exit;
  Result := PCompelBreakpointInfo(FList[Index])^;
end;

procedure TCompelBreakpointInfoList.Put(Index: Integer;
  Info: TCompelBreakpointInfo);
begin
  if Index >= Count then
    Exit;
  PCompelBreakpointInfo(FList[Index])^ := Info;
end;

{ TCompelTokenizer }

function TCompelTokenizer.Count: Integer;
begin
  Result := compel_tokenize_parsed_count(Ftok);
end;

constructor TCompelTokenizer.Create;
begin
  Ftok := compel_tokenize_init(nil, nil, nil, nil);
  if Ftok = 0 then
    raise Exception.Create('Could not create tokenizer object!');
end;

destructor TCompelTokenizer.Destroy;
begin
  compel_tokenize_free(Ftok);
  inherited;
end;

function TCompelTokenizer.GetInt(Index: Integer): Integer;
begin
  Result := compel_parse_number(compel_tokenize_get(Ftok, Index));
end;

function TCompelTokenizer.GetItems(Index: Integer): string;
begin
  Result := compel_tokenize_get(Ftok, Index)
end;

// ;! untested
function TCompelTokenizer.Join(ASep: string; AFrom, ATo: Integer): string;
var
  i: Integer;
  s: string;
begin
  s := '';
  if ATo = 0 then
    ATo := Count;
  for i := AFrom to ATo - 1 do
  begin
    s := s + Self[i] + ASep;
  end;

  i := Length(ASep);
  
  Delete(s, Length(s)-i, i);
  Result := s;
end;

function TCompelTokenizer.Parse(const AStr, ADelim, AQuotes,
  AEscape: string): Integer;
var
  pStr, pDelim, pQuotes, pEsc: PChar;
begin
  if Length(AStr) <> 0 then
    pStr := PChar(AStr)
  else
    pStr := nil;

  if Length(ADelim) <> 0 then
    pDelim := PChar(ADelim)
  else
    pDelim := nil;

  if Length(AQuotes) <> 0 then
    pQuotes := PChar(AQuotes)
  else
    pQuotes := nil;

  if Length(AEscape) <> 0 then
    pEsc := PChar(AEscape)
  else
    pEsc := nil;

  Result := compel_tokenize_parse(Ftok, pStr, pDelim, pQuotes, pEsc)
end;

procedure TCompelTokenizer.SetItems(Index: Integer; const Value: string);
begin
  compel_tokenize_set(Ftok, Index, PChar(Value));
end;

end.
