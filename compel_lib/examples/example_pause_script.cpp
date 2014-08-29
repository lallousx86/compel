static DWORD __stdcall PauseThread(compel_script_t script)
{
  _getch();
  printf("will pause...");
  compel_internal(script, compel_internal_pause, 0, 0, 0);
  printf("paused...\n");
  return 0;
}

void example_pause_script(compel_script_t script)
{
  ::CreateThread(0, 0, PauseThread, (LPVOID)script, 0, 0);
  compel_script_load_lines(script, "var $i 0|for $i 0 to 1000|{|echoln \"i=\" $i|}|", "|");
  compel_script_run(script);
}
