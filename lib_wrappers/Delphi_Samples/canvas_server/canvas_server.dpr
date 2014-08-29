program canvas_server;

{$DEFINE d bg}
{$DEFINE server}

uses
  Forms,
  Windows,
  SysUtils,
  Dialogs,
  uCanvasServer in 'uCanvasServer.pas' {frmCanvasExt},
  uCanvasExtShapes in 'uCanvasExtShapes.pas',
  CanvasExtTypes in 'CanvasExtTypes.pas',
  uCanvasExtUtil in 'uCanvasExtUtil.pas';

{$R *.res}

procedure server;
var
  si: TStartupInfo;
  hPipe: THandle;
  pipe_name: string;
  dwMode: Cardinal;
  handle_msg: TCanvasClientMsg_Handle;

begin
  Windows.GetStartupInfo(si);

{$IFDEF dbg}
  pipe_name := '\\.\pipe\canvastest';
{$ELSE}
  pipe_name := Format(CANVAS_PIPE_NAME_FMT, [si.dwX, si.dwY]);
{$ENDIF}
  if not WaitNamedPipe(PChar(pipe_name), 20000) then
    Exit;

  hPipe := CreateFile(
         PChar(pipe_name), // pipe name
         GENERIC_READ or  // read and write access
         GENERIC_WRITE,
         0,              // no sharing
         nil,           // default security attributes
         OPEN_EXISTING,  // opens existing pipe
         0,              // default attributes
         0);          // no template file
  if (hPipe = Windows.INVALID_HANDLE_VALUE) then
    Exit;

  dwMode := PIPE_READMODE_MESSAGE;
  if not SetNamedPipeHandleState(
     hPipe,    // pipe handle
     dwMode,  // new pipe mode
     nil,     // don't set maximum bytes
     nil)    // don't set maximum time
  then begin
    Exit;
  end;

  Application.Initialize;
  Application.CreateForm(TfrmCanvasExt, frmCanvasExt);
  // Pass our window handle
  handle_msg.Handle := frmCanvasExt.Handle;
  WriteFile(hPipe, handle_msg, SizeOf(handle_msg), dwMode, nil);

  // Close PIPE
  CloseHandle(hPipe);

{$IFDEF dbg_msg}
  ShowMessage(Format('hnd:%d X:%d Y:%d Xsize:%d Ysize:%d',
    [frmCanvasExt.Handle, si.dwX, si.dwY, si.dwXSize, si.dwYSize])
  );
{$ENDIF}

{$IFDEF dbg_exit}
  Application.Terminate;
  Exit;
{$endif}

  Application.Run;
end;

begin
{$IFDEF server}
  server;
{$ELSE}
  Application.Initialize;
  Application.CreateForm(TfrmCanvasExt, frmCanvasExt);
  Application.Run;
{$ENDIF}  
end.
