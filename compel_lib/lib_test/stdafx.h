// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once


#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <stdio.h>
#include <tchar.h>

#include <windows.h>
#include <crtdbg.h>
#include <string.h>
#include <string>
#include <map>
#include <assert.h>
#include <windows.h>
#include <map>
#include <assert.h>
#include <conio.h>
#include "../compel_lib.h"

class CCrtDebug
{
public:
	CCrtDebug()
	{
		_CrtSetDbgFlag(
			_CrtSetDbgFlag(_CRTDBG_REPORT_FLAG)
			| _CRTDBG_LEAK_CHECK_DF
			| _CRTDBG_ALLOC_MEM_DF
			| _CRTDBG_CHECK_ALWAYS_DF);
	}
} crtdebug;

