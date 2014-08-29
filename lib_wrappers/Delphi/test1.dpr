{$APPTYPE CONSOLE}

program test1;

uses compel_lib, Windows, SysUtils, CompelScript;

const
  test_client_guid = 'test_1234_51323';

{$I example_initialize.inc}
{$I example_tokenizer.inc}
{$I example_value.inc}
{$I example_object.inc}
{$I example_error_handler.inc}
{$I example_lu_cmd1.inc}
{$I example_lu_cmd2.inc}


var
  script: compel_script_t;

begin
  example_initialize(script);

  example_lu_cmd2;
  //example_lu_cmd1(script);
  //example_error_handler(script);
  //example_object(script);
  //example_value(script);
  //example_tokenizer;
  compel_script_deinit(script);
end.