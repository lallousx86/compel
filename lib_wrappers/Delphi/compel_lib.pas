unit compel_lib;

{
. last updated on: 05/24/2006
. !! please refer to compel_lib.h documentation !! 
}

{
/*!
\brief User context variable that will be passed to extension initialisation and deinitialization
*/
typedef void *compel_extension_user_context_t;

/*!
\brief Prototype of user extension initialization procedure
When this is called, you are responsible of introducing new commands or manipulate the script
\param compel_script_t
\param user_extension [out] The user modifies this value, later this value will be passed to deinit call
*/
typedef int (COMPEL_API *compel_extension_init_cb_t)(compel_script_t, compel_extension_user_context_t *);

/*!
\brief Prototype of user extension de-initialization procedure
This is called when the script instance is being freed
\param compel_script_t
\param user_extension [in] This value was previously initialized by the extension when init was called
*/
typedef int (COMPEL_API *compel_extension_deinit_cb_t)(compel_script_t, compel_extension_user_context_t);
}

interface

const
  DLLNAME = 'compel_lib.dll';

  // internal commands
  compel_internal_writeraw = 1;
  compel_internal_setdbgout = 2;
  compel_internal_showlines = 3;
  compel_internal_showsymtbl = 4;
  compel_internal_pause = 5;
  compel_internal_exectime = 6;
  compel_internal_preparse_failing_line = 7;

  // compel error codes
  compel_error_success                       = 0;
  compel_error_symbol_undefined              = 1;
  compel_error_syntax_error                  = 2;
  compel_error_error_handler_continue_search = 3;
  compel_error_wrong_param_count             = 4;
  compel_error_function_expected             = 5;
  compel_error_script_stopped                = 6;
  compel_error_symbol_redefined              = 7;
  compel_error_unknown_error                 = 8;
  compel_error_branch_handled                = 9;
  compel_error_symbol_expected               = 10;
  compel_error_wrong_param_value             = 11;
  compel_error_no_operation                  = 12;

type
  p_compel_init_t = ^compel_init_t;
  compel_init_t = packed record
    b_usefullns : Boolean;
    b_dbgout_script : Boolean;
    script_args : PChar;
    script_file : PChar;
    extensions : PChar;
  end;

  compel_value_t = Integer;
  compel_object_t = Integer;

  compel_tokenizer_t = Integer;
  compel_script_t = Integer;

  compel_user_context_t = Pointer;
  
  TCompel_C_PCharArray = array[0..200] of PChar;

  compel_lib_usercommand_cb_t = function(script: compel_script_t; argc: Integer;argv: PChar): Integer;stdcall;
  compel_lib_usercommand_cb_t3 = function(script: compel_script_t; argc: Integer;argv: TCompel_C_PCharArray): Integer;stdcall;

  compel_script_error_handler_cb_t = function(script: compel_script_t; lineno: Integer;err: Integer): Integer;stdcall;

  compel_ext_init_t = function (script: compel_script_t;var Context: compel_user_context_t): Integer;stdcall;

  pcompel_script_error_handler_cb_params_t = ^compel_script_error_handler_cb_params_t;
  compel_script_error_handler_cb_params_t = record
    script: compel_script_t;
    lineno: Integer;
    err: Integer;
    Return: Integer;
  end;

  p_lib_usercommand_info_t = ^lib_usercommand_info_t;
  lib_usercommand_info_t = packed record
    name: PChar;
    minargs: Integer;
    maxargs: Integer;
    cb: compel_lib_usercommand_cb_t;
    desc: PChar;
    context: compel_user_context_t;
  end;

  function compel_error_code_to_string(lib_err: Integer): PChar;stdcall;external DLLNAME;

  // -------------------------------------------------------
  // value_t manipulation
  // -------------------------------------------------------

  function compel_value_create(
    script: compel_script_t;
    varname: PChar;
    initial_value: PChar): compel_value_t;stdcall;external DLLNAME;

  function compel_value_destroy(
    script: compel_script_t;
    varname: PChar): Boolean;stdcall;external DLLNAME;

  function compel_value_find(
    script: compel_script_t;
    varname: PChar): compel_value_t;stdcall;external DLLNAME;

  procedure compel_value_set(
    compel_script: compel_script_t;
    compel_value: compel_value_t;
    value: PChar);stdcall;external DLLNAME;

  function compel_value_value_get(
    script: compel_script_t;
    val_name: PChar): PChar;stdcall;external DLLNAME;

  function compel_value_value_set(
    script: compel_script_t;
    val_name : PChar;
    val_val : PChar): Boolean;stdcall;external DLLNAME;

  function  compel_value_get(
    script: compel_script_t;
    value: compel_value_t): PChar; stdcall;external DLLNAME;


  // -------------------------------------------------------
  // object manipulation
  // -------------------------------------------------------

  function compel_object_create(
    script: compel_script_t;
    objname: PChar): compel_object_t;stdcall;external DLLNAME;

  function compel_object_destroy(
    script: compel_script_t;
    objname: PChar): Boolean;stdcall;external DLLNAME;

  function compel_object_find(
    script:compel_script_t;
    objname: PChar): compel_object_t;stdcall;external DLLNAME;

  function compel_object_add_attr(
    script: compel_script_t;
    obj: compel_object_t;
    attrname: PChar;
    initial_value: PChar): compel_value_t;stdcall;external DLLNAME;

  function compel_object_to_string(
    compel_script: compel_script_t;
    objname: PChar): PChar;stdcall;external DLLNAME;

  function compel_object_find_attr(
    script: compel_script_t;
    obj: compel_object_t;
    attrname: PChar): compel_value_t;stdcall;external DLLNAME;

  function compel_object_remove_attr(
    script: compel_script_t;
    obj: compel_object_t;
    attrname: PChar): Boolean;stdcall;external DLLNAME;


  // -------------------------------------------------------
  // script engine functions
  // -------------------------------------------------------

  function compel_script_evaluate_expression(
  compel_script: compel_script_t;
  expr: PChar;
  bKeepQuotes: Integer;
  chDelim: Integer): PChar;stdcall;external DLLNAME;

  function compel_script_init(
    init: p_compel_init_t): compel_script_t;stdcall;external DLLNAME;

  procedure compel_script_deinit(
    script: compel_script_t);stdcall;external DLLNAME;

  function compel_script_avail_extensions(): PChar;stdcall;external DLLNAME;

  function compel_script_get_line(
    script: compel_script_t;
    lineno: Integer): PChar;stdcall;external DLLNAME;

  function compel_script_get_lines_count(
      script: compel_script_t): Integer;stdcall;external DLLNAME;
      
  function compel_script_set_line(
    script: compel_script_t;
    lineno: Integer;
    value: PChar): Boolean;stdcall;external DLLNAME;

  procedure compel_script_set_lineno(
    script: compel_script_t;
    lineno: Integer);stdcall;external DLLNAME;

  function compel_script_get_lineno(
    script: compel_script_t): Integer;stdcall;external DLLNAME;

  procedure compel_script_context_set(
    script: compel_script_t;
    guid: PChar;
    context: compel_user_context_t);stdcall;external DLLNAME;

  function compel_script_context_get(
    script: compel_script_t;
    guid: PChar): compel_user_context_t;stdcall;external DLLNAME;

  procedure compel_script_set_error_handler(
    script: compel_script_t;
    cb: compel_script_error_handler_cb_t);stdcall;external DLLNAME;

  function compel_script_load_file(
    script: compel_script_t;
    filename: PChar): Integer;stdcall;external DLLNAME;

  function compel_script_load_lines(
    script: compel_script_t;
    lines,
    delims: PChar): boolean;stdcall;external DLLNAME;

  procedure compel_script_clear_lines(
    script: compel_script_t);stdcall;external DLLNAME;

  function compel_script_run(
    script: compel_script_t): Integer;stdcall;external DLLNAME;

  function compel_script_step(script: compel_script_t): Integer;stdcall;external DLLNAME;

  function compel_script_interpret_line(script: compel_script_t; line: PChar): Integer;stdcall;external DLLNAME;

  function compel_lu_cmd_set_retval(
    script: compel_script_t;
    retval: PChar): Boolean;stdcall;external DLLNAME;

  function compel_lu_cmd_get_info(
  compel_script: compel_script_t): p_lib_usercommand_info_t;stdcall;external DLLNAME;

  function compel_lu_cmd_register(
    script: compel_script_t;
    uc: p_lib_usercommand_info_t): Integer;stdcall;external DLLNAME;

  function compel_lu_cmd_register2(
    script: compel_script_t;
    cb: compel_lib_usercommand_cb_t;
    name: PChar;
    minargs,
    maxargs: Integer): Integer;stdcall;external DLLNAME;

  function compel_lu_cmd_register3(
    script: compel_script_t;
    cb: compel_lib_usercommand_cb_t3;
    name: PChar;
    minargs,
    maxargs: Integer): Integer;stdcall;external DLLNAME name 'compel_lu_cmd_register2';

  function compel_internal(
    script: compel_script_t;
    arg1, arg2, arg3, arg4: Integer): Integer;stdcall;external DLLNAME;

  // -------------------------------------------------------
  // tokenizing utility
  // -------------------------------------------------------

  function compel_tokenize_init(
    str,
    delim,
    quote,
    escape: PChar): compel_tokenizer_t;stdcall;external DLLNAME;

  function compel_tokenize_parse(
    tok: compel_tokenizer_t;
    str,
    delim,
    quote,
    escape: PChar): Integer;stdcall;external DLLNAME;

  procedure compel_tokenize_free(tok: compel_tokenizer_t);stdcall;external DLLNAME;

  function compel_tokenize_parsed_count(tok: compel_tokenizer_t): Integer;stdcall;external DLLNAME;

  function compel_tokenize_get(
    tok: compel_tokenizer_t;
    idx: Integer): PChar;stdcall;external DLLNAME;

  function compel_tokenize_set(
    tok: compel_tokenizer_t;
    idx: Integer;
    str: PChar): PChar;stdcall;external DLLNAME;


  // ----------------------------------------------------------------
  // misc and utility functions
  // ----------------------------------------------------------------
  function compel_parse_number(value: PChar): Longint;stdcall;external DLLNAME;

  // -------------------------------------------------------
  // string utility functions
  // -------------------------------------------------------

  function compel_string_create(str: PChar; sz: Integer): PChar;stdcall;external DLLNAME;

  procedure compel_string_destroy(str: PChar);stdcall;external DLLNAME;

implementation

end.
