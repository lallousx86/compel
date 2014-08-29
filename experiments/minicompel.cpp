#pragma warning (disable: 4996)
#define WIN32_LEAN_AND_MEAN

#include <typeinfo>
#include <windows.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <map>
#include <string>
#include <algorithm>
#include <list>
#include <iterator>
#include <fstream>
#include <set>
#include <crtdbg.h>

#include "fwd.h"
#include "object.h"
#include "symbol_table.h"
#include "compel_string_tokenizer.h"
#include "function.h"
#include "function_helper.h"
#include "interpreter.h"
#include "fnc/fnc_crt.h"
#include "fnc/fnc_vars.h"
#include "fnc/fnc_usercommand.h"
#include "fnc/fnc_scopes.h"
#include "fnc/fnc_arith.h"
#include "fnc/fnc_comments.h"
#include "fnc/fnc_string.h"
#include "fnc/fnc_dirs.h"
#include "fnc/fnc_thread.h"
#include "fnc/fnc_com.h"
#include "obj/com_object.h"
#include "../compel_lib/compel_lib.h"

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


void reg_built_in_functions(interpreter_t &_int)
{
	bool bUseFullNS = false;
	function_t *reg[] =
	{
		new fnc_vars,
		new fnc_crt(bUseFullNS),
		new fnc_scopes,
		new fnc_comments,
		new fnc_string(bUseFullNS),
		new fnc_binary_arith(bUseFullNS),
		new fnc_usercommand,
		new fnc_dirs,
    new fnc_com,
		new fnc_thread
	};

	for (size_t i=0;i<sizeof(reg)/sizeof(reg[0]);i++)
	{
		reg[i]->register_function(&_int);
		delete reg[i];
	}
}

/*
class fnc_rtti : public function_t
{
public:
fnc_rtti(interpreter_t *);

fnc_rtti();

parse_errors_e execute();
parse_errors_e prepare(size_t passno, int &start_or_failing_line);
parse_errors_e register_function(interpreter_t *);
};
*/

int start()
{
	interpreter_t _int;
	symbol_table_t symtbl;

	_int.set_symbol_table(&symtbl);
	reg_built_in_functions(_int);

	union
	{
		int starting_line;
		int failing_line;
	};

	starting_line = 0;
	_int.load_lines_from_file("..\\test_scripts\\fcd_prog1.compel", false);
	_int.prepare(starting_line);
	parse_errors_e err = parse_error_none;
	while (err == parse_error_none)
	{
		err = _int.interpret_line();
		if (err == parse_stop_parsing)
			break;

		switch (err)
		{
		case parse_error_line_is_comment:
		case parse_error_line_is_void:
		case parse_line_is_empty:
			err = parse_error_none;
			break;
		}

		if (err != parse_error_none)
		{
			printf("err %d @ %d\n", (int) err, _int.get_cur_source_lineno());
		}
	}
	return 0;
}

void obj_test()
{
	object_t obj1;
	object_t obj2;
	value_t *v1;
	std::string s;

	v1 = obj1.insert_attribute("v1", value_t(123));

	obj2 = obj1;

	obj2.to_string(s);
	printf("%s\n", s.c_str());
}

void crt_test()
{
	interpreter_t _int;
	symbol_table_t symtbl;

	_int.set_symbol_table(&symtbl);
	reg_built_in_functions(_int);

	int test_type;

	test_type = 1;

	// should we test console allocation / deallocation?
	if (test_type == 1)
	{
		value_t *ch = new value_t;
		_int.add_symbol("$ch", ch);
		_int.interpret_new_line("yesnobox $ch are_you quest");
		_int.interpret_new_line("echoln ch= $ch");

//		_int.interpret_new_line("inputline $ch");
		FreeConsole();
		AllocConsole();
		_int.interpret_new_line("getch $ch");
//		_int.interpret_new_line("inputline $ch");
		_int.interpret_new_line("echo $ch");
	}
	// test echoln w/o parameters?
	else if(test_type == 2)
	{
		_int.interpret_new_line("echo hi!");
		_int.interpret_new_line("echoln");
		_int.interpret_new_line("echoln \"ho! \" adsd");
	}

}

void com_obj_test()
{
  com_object_t c;

  c.create("AutoItX.Control");

  c["argc"] = 2;
  c["callsig"] = "ii";
  c.insert_attribute("0", value_t("20"));
  c.insert_attribute("1", value_t("20"));

  c.invoke("MouseMove");
    //ct.invoke_fmt("MouseMove", "ii", "0", "0", 0);
}

int main()
{
  com_obj_test();
  return -1;
	//  obj_test();
	//  return 0;
	//  return start();
	crt_test();
	return 0;
}