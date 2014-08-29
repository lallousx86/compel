void DebuggerLoop()
{
	while (ErrorCode == Success)
	{
	  switch(Event)
	  {
	    case EVENT_BREAKPOINT:
	      SuspendExecution();
	      break;
	    case EVENT_PAUSE:
	      StopAllTasks();
	      SuspendExecution();
	      break;
	    case EVENT_RUN:
	      ResumeExecution();
	      break;
	    case EVENT_SINGLE_STEP:
	      TraceOneLine();
	      break;
	    case EVENT_SCRIPT_ERROR:
	      DisplayErrorHandler();
	      if (WantToContinue)
		ResumeExecution();
	      else
		AbortScriptExecution();
	  }
	}
}