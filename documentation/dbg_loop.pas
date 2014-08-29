procedure DebuggerLoop;
var
  err: Integer;
  q: PCompelRuntimeEventQueueItem;
  CurLine: Integer;
begin

  // Initialize variables
  err := compel_error_success;
  LastHitLine := NO_LINE;
  LastFaultLine := NO_LINE;

  // Loop as long as we encountered no unhandled errors or script end
  while (err = compel_error_success) do
  begin
    // Wait for an event
    q := WaitForCompelDebugEvent;

    // No event? Wait more
    if NO_DEBUG_EVENT then
      Continue;

    // Exit thread?
    if DEBUGEVENT = EXIT_DEBUGGER_LOOP then
      Break;

    // Get current ExecPoint
    CurLine := ScriptGetLineNo;

    // Check temporary breakpoint
    if TempoporaryBreakPoint.IsHit(CurLine) then
    begin
      // Cause suspend
      q.Code := rteNothing;
      LastHitLine := CurLine;
    end
    // Breakpoint at a new line?
    else if BreakPointsList.IsBpHit(CurLine) then
    begin
      q.Code := rteNothing;
      LastHitLine := CurLine;
    end else
    begin
      // Reset last hit line
      LastHitLine := NO_LINE;
    end;

    if (DEBUGEVENT = PAUSE_SCRIPT) then
    begin
      // Clear the queue to give space for PAUSE
      FRtQ.ClearAll;
      q.Code := rteNothing;
    end else
    // Run?
    if DEBUGEVENT = RUN_SCRIPT then
    begin
      // Run is a series of steps till the end of script is reached
      err := ScriptSingleStepNow;

      // No error? then run more
      if (err = compel_error_success) then
        ScriptRun;
    end
    else if DEBUGEVENT = SINGLE_STEP then
    begin
      // Do a single step
      err := ScriptSingleStepNow;
    end;

    // Script error occured
    if (err <> compel_error_script_stopped) and
       (err <> compel_error_success) then
    begin
      // A new error?
      if LastFaultLine <> CurLine then
      begin
        err := CallErrorHandler(Fscript, CurLine, err)
      end
      // Error already handled
      else begin
        // Skip faulty line
        ScriptSetLineNo(CurLine+1);
        // No error
        err := compel_error_success;
      end;
      LastFaultLine := CurLine;
    end
    else begin
      LastFaultLine := NO_LINE;
    end;

    // No more events? Signal OnSuspend
    if ((err = compel_error_success) and (QueuedDebugEventsCount = 0))then
    begin
        // Set SUSPENDED state
        SetState(SUSPENDED);
    end;
  end;

  // Notify GUI about script end
  OnRunFinished;
end;
