unit CompelIDEConsts;

interface

uses CompelScript;

type
  TIDEStates = (stateEmpty, stateEditing, stateRunning, stateSuspended);

const
  // consts for create xxxx form function
  ideconstsCannotInitialize = 0;
  ideconstsFirstInitialization = 1;
  ideconstsAlreadyInitialized = 2;

  // wm_ide_refresh consts
  ideconstsRedrawBreakpoints = 1;

  WM_IDE_BASE = WM_COMPEL_LAST + 1;
  WM_IDE_REFRESH = WM_IDE_BASE + 0;
  WM_IDE_SHOWSCRIPTERROR = WM_IDE_BASE + 1;

  strDbgTag   = 'dbg';
  strEdtTag   = 'edt';
  strBptTag   = 'bpt';
  strWatchTag = 'watch';
  strIdeTag   = 'ide';
  strDBG_CLIENT = '6B84C274-B110-4af2-BED0-2C4B809B13DD';

implementation

end.
