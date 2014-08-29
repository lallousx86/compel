// ext_dll.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include "ext_dll.h"

/*!
\brief lowercase user command
This command takes the first parameter by value and returns the lowercase version
*/
int COMPEL_API ext_lowercase(compel_script_t compel_script, int argc, char *argv[])
{
  size_t len = strlen(argv[0]);
  char *s = new char[len+1];
  strcpy(s, argv[0]);
  strlwr(s);

  compel_lu_cmd_set_retval(compel_script, s);

  delete [] s;

  return 0;
}

int __stdcall compel_ext_init(compel_script_t script, compel_user_context_t *ctx)
{
  int err = 0;

  compel_value_create(script, "$hey", "asdf!");

  err = compel_lu_cmd_register2(script, ext_lowercase, "ext_lowercase", 1, 1);

  return err;
}

BOOL APIENTRY DllMain(
  HMODULE hModule,
  DWORD  ul_reason_for_call,
  LPVOID lpReserved)
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
    return TRUE;
}