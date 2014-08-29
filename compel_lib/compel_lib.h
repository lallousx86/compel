/*! \file compel_lib.h
\brief COMPEL library header

 This library exports methods to be used by COMPEL users.
 All calls are exported and can be called from almost any language that supports stdcall calling convention

 Last updated on 05/15/2006, 05:06 PM
*/

#ifndef __COMPEL_LIB_03292006__
#define __COMPEL_LIB_03292006__

/*! \def COMPEL_API 
\brief Defines the calling convention used.
*/
#define COMPEL_API __stdcall

/*! \def COMPEL_EXPORT
\brief Designates whether this library is used as a DLL or static library.
*/
#ifdef __COMPEL_DLL__
  #define COMPEL_EXPORT extern "C" __declspec(dllexport)
#else
  /*! \def __COMPEL_STATIC_LIB__
  \brief Define it when linking against the static version of COMPEL library
  */
  #ifdef __COMPEL_STATIC_LIB__
    #define COMPEL_EXPORT
  #else
    #define COMPEL_EXPORT extern "C" __declspec(dllimport)
  #endif
#endif

/*!
\brief COMPEL internal command codes. These codes are used with compel_internal().
The \a arg1 parameter can hold any of the listed values.
The rest of the arguments depend on the \a arg1 value
*/
enum compel_internal_e
{
  /*!
  This allows you to write the prepared (processed) script lines into a file
  arg2 can optionally point to a pointer to the raw file name.
  It returns 0 if failure to write or 1 if write successful
  */
  compel_internal_writeraw = 1,
  /*!
  This will enable or disable the outputing of the interpreted lines.
  arg2 can be 0 or 1 to enable or disable
  \sa _compel_init_t b_dbgout_script
  */
  compel_internal_setdbgout = 2,

  /*!
  This flag will allow COMPEL engine to dump all the script lines to stdout
  */
  compel_internal_showlines = 3,

  /*!
  This flag will allow COMPEL to dump the contents of the symbol table to stdout
  */
  compel_internal_showsymtbl = 4,

  /*!
  Interrupts the execution of the script
  */
  compel_internal_pause = 5,
  /*!
  Get the execution time in milliseconds
  */
  compel_internal_exectime = 6,

  /*!
  Returns the preparse failing line number
  */
  compel_internal_preparse_failing_line = 7,
};
/*!
  \brief COMPEL library error codes returned.

  These error codes are returned by some of the library calls
*/
enum compel_errors_e
{
  //! Success
  compel_error_success                       = 0, 

  //! An undefined symbol occured
  compel_error_symbol_undefined              = 1, 

  //! Script syntax error
  compel_error_syntax_error                  = 2, 

  /*! 
  \brief The error handler decided to pass the error to next handlers
  \sa compel_script_set_error_handler()
  
  */
  compel_error_error_handler_continue_search = 3,

  //! Wrong parameters passed to a given command, eiter more or less commands passed
  compel_error_wrong_param_count             = 4, 

  //! A command call was expected
  compel_error_function_expected             = 5,

  //! The script has been stoped either because it encountered END or fatal script error
  compel_error_script_stopped                = 6,

  //! Symbol was redefined
  compel_error_symbol_redefined              = 7,

  //! An unknown error occured. This should usually never occur.
  compel_error_unknown_error                 = 8,

  /*! \brief We adjusted script line number, the engine should not advance the script line number.
  The user command changed the source line number, thus the engine should not advance to next line.
  This is useful if you write some sort of branching commands.
  */
  compel_error_branch_handled                = 9,

  //! A symbol rather than a constant was expected. The symbol may 
  compel_error_symbol_expected               = 10,

  /*! \brief A wrong parameter value was generated or passed.
  For example, a divide by zero operation might return this value
  */
  compel_error_wrong_param_value             = 11,

  /*! \brief The instruction yields nothing because it can be:
  - empty line
  - void line
  - comment line
  */
  compel_error_no_operation                  = 12,

  /*! 
  \brief The extension structure is not valid!
  */
  compel_error_invalid_extension             = 13
};

#ifdef _MSC_VER
#pragma pack(1)
#endif

/*!
  \brief COMPEL library initialization structure.
  This is used when you call script engine initialization function.
  \sa compel_script_init()
*/
typedef struct _compel_init_t
{
  //! Should the built-in extensions be registered with their full names?
  bool b_usefullns;

  //! Should the executed script lines be sent to debugger (as dbgout)?
  bool b_dbgout_script;

  /*!
  \brief The arguments passed to the script.
  You may access these arguments from the script using the $ARGS variable.
  The $ARGS.0 value is always set to the script's file name (if loaded from a file), or to "<string>" if
  script was loaded from a file
  */
  const char *script_args;

  /*! 
  \brief The script's file name.
  If this is set, you later no need to specify the script file name in the "compel_script_load_file".
  \sa compel_script_load_file()
  */
  const char *script_file;

  /*!
  \brief The desired built-in extensions you want to enable in the scripting engine.
  The extension names must be \a ; separated.
  \sa compel_script_avail_extensions()
  */
  const char *extensions;

} compel_init_t, *p_compel_init_t;

#ifdef _MSC_VER
#pragma pack()
#endif

//
// abstract/opaque types
//


/*! \typedef compel_value_t
\brief An abstract data type to define value_t (the basic data type)
*/
typedef void *compel_value_t;

/*! \typedef compel_object_t
\brief ADT defining object_t types.
\sa compel_value_create compel_value_find
*/
typedef void *compel_object_t;

/*!
  \brief ADT defining the scripting engine type
  This type is passed to most function calls, to select which script should the call be applied on.

  \sa compel_script_init
*/
typedef void *compel_script_t;

/*! \typedef compel_tokenizer_t

   \brief ADT defining the string tokenizer object

   \sa compel_tokenize_init()
*/
typedef void *compel_tokenizer_t;


/*!
\brief User context variable that will:
- be passed to extension initialisation and deinitialization
- be used in set_context and get_context
*/
typedef void *compel_user_context_t;

/*!
\brief Prototype of user extension initialization procedure
When this is called, you are responsible of introducing new commands or manipulate the script
\param compel_script_t
\param user_extension [out] The user modifies this value, later this value will be passed to deinit call
*/
typedef int (COMPEL_API *compel_extension_init_cb_t)(compel_script_t, compel_user_context_t *);

/*!
\brief Prototype of user extension de-initialization procedure
This is called when the script instance is being freed
\param compel_script_t
\param user_extension [in] This value was previously initialized by the extension when init was called
*/
typedef int (COMPEL_API *compel_extension_deinit_cb_t)(compel_script_t, compel_user_context_t);

/*! 
  \typedef compel_lib_usercommand_cb_t

  \brief This is the prototype for the user defined commands.

  \sa compel_lu_cmd_register2()
*/
typedef int (COMPEL_API *compel_lib_usercommand_cb_t)(compel_script_t, int argc, char **argv);

/*! \typedef compel_script_error_handler_cb_t

  \brief Error handler callback prototype.

  \sa compel_script_set_error_handler()
*/
typedef int (COMPEL_API *compel_script_error_handler_cb_t)(compel_script_t, size_t lineno, int err);

/*! \struct _lib_usercommand_info_t
  
  \brief User command registration structure. Used when registering a user command

  \sa compel_lu_cmd_register()
*/
typedef struct _lib_usercommand_info_t
{
  //! the name of the user function
  const char *name;

  //! min arguments of the command
  size_t minargs;

  //! max arguments of the command
  size_t maxargs;

  /*! \brief the callback
  \sa compel_lib_usercommand_cb_t
  */
  compel_lib_usercommand_cb_t cb;

  //! user command's description
  const char *desc;

  //! arbitrary user information
  void *context;
} lib_usercommand_info_t, *p_lib_usercommand_info_t;


/*!
 \brief Converts from engine to lib error (this method is used internally).
 \param engine_err the error returned from the parsing engine
 \returns library error
 \internal
*/
COMPEL_EXPORT int COMPEL_API compel_engine_to_lib_error(int engine_err);

/*!
 \brief Converts from lib error to engine error (this method is used internally).
 \param lib_err the error returned from the parsing engine
 \returns engine error
 \internal
*/
COMPEL_EXPORT int COMPEL_API compel_lib_to_engine_error(int lib_err);

/*!
  \brief Converts error code to error string
*/
COMPEL_EXPORT const char *COMPEL_API compel_error_code_to_string(int lib_err);

// -------------------------------------------------------
// value_t manipulation
// -------------------------------------------------------

/*!
 \brief Creates a variable
 \param compel_script script instance
 \param varname the variable name
 \param initial_value the initial value of the variable
 \returns The create value reference object
*/
COMPEL_EXPORT compel_value_t COMPEL_API compel_value_create(
  compel_script_t compel_script, 
  const char *varname, 
  const char *initial_value);

/*!
 \brief Destroys a variable
 \param compel_script_t script instance
 \param varname variable name
 \returns Success or failure
 \sa compel_value_create
*/
COMPEL_EXPORT bool COMPEL_API compel_value_destroy(
  compel_script_t compel_script, 
  const char *varname);

/*!
 \brief finds a value by name
 \param compel_script script instance
 \param varname variable's name
 \returns The value reference object
*/
COMPEL_EXPORT compel_value_t COMPEL_API compel_value_find(
  compel_script_t compel_script, 
  const char *varname);

/*!
 \brief updates a value_t's value
 \param compel_script script instance
 \param compel_value_t variable's reference
 \param value The new value
*/
COMPEL_EXPORT void COMPEL_API compel_value_set(
  compel_script_t compel_script, 
  compel_value_t compel_value, 
  const char *value);


/*!
 \brief Returns a compel_value_t's value by variable name
 \param compel_script script instance
 \param val_name variable's name
 \returns The variable's value
*/
COMPEL_EXPORT const char * COMPEL_API compel_value_value_get(
  compel_script_t compel_script, 
  const char *val_name);

/*!
 \brief Sets a variable's value by variable name
 \param compel_script script instance
 \param val_name variable's name
 \param val_val variable's value
 \returns Success if variable was found and its value updated
*/
COMPEL_EXPORT bool COMPEL_API compel_value_value_set(
  compel_script_t compel_script, 
  const char *val_name,
  const char *val_val);

/*!
 \brief Returns a variable's value
 \param compel_script script instance
 \param compel_value_t variable's reference
 \returns Success if variable was found and its value updated
*/
COMPEL_EXPORT const char * COMPEL_API compel_value_get(
  compel_script_t, 
  compel_value_t);


// -------------------------------------------------------
// object manipulation
// -------------------------------------------------------

/*!
  \brief Creates an object.
  \param objname The object's name
  \returns Returns compel_object_t newly created reference
*/
COMPEL_EXPORT compel_object_t COMPEL_API compel_object_create(
  compel_script_t, 
  const char *objname);

/*!
  \brief Destroys an object
  \param objname The object's name
  \returns Success if the object was found and destroyed
*/
COMPEL_EXPORT bool COMPEL_API compel_object_destroy(
  compel_script_t, 
  const char *objname);

/*!
  \brief Finds an object by name
  \param Object name
  \returns Object Reference or 0 if not found
*/
COMPEL_EXPORT compel_object_t COMPEL_API compel_object_find(
  compel_script_t, 
  const char *objname);

/*!
  \brief Adds an attribute to an object
  \param compel_object_t compel cbject
  \param attrname Attribute's name
  \param initial_value Initial attribute value
  \returns Returns the attribute's reference as a variable
*/
COMPEL_EXPORT compel_value_t COMPEL_API compel_object_add_attr(
  compel_script_t, 
  compel_object_t, 
  const char *attrname, 
  const char *initial_value);

/*!
  \brief Finds an attribute within an object
  \param compel_object_t compel object
  \param attrname 
  \returns 
*/
COMPEL_EXPORT compel_value_t COMPEL_API compel_object_find_attr(
  compel_script_t, 
  compel_object_t, 
  const char *attrname);

/*!
  \brief Removes an attribute from an object
  \param attrname Attribute's name
  \returns Success if attribute was found and removed
*/
COMPEL_EXPORT bool COMPEL_API compel_object_remove_attr(
  compel_script_t, 
  compel_object_t,
  const char *attrname);

/*!
\brief Invokes the object's to_string method
\param output The string value of the whole object and attributes
\returns Pointer ZERO if object not found or a pointer to the string value of the object
\note You have to free the return buffer through compel_string_destroy()
\sa compel_string_destroy
*/
COMPEL_EXPORT char *COMPEL_API compel_object_to_string(
  compel_script_t compel_script, 
  const char *objname);

// -------------------------------------------------------
// script engine functions
// -------------------------------------------------------

/*!
  \brief Retrieves the source line value
  \param lineno The line number you want to return
*/
COMPEL_EXPORT const char *COMPEL_API compel_script_get_line(
  compel_script_t compel_script,
  size_t lineno);

/*!
  \brief Patches the source line value
  \param lineno The line number you want to adjust
  \param value The new line value
*/
COMPEL_EXPORT bool COMPEL_API compel_script_set_line(
  compel_script_t compel_script,
  size_t lineno,
  const char *value);

/*!
  \brief Changes the current script execution line
  \param lineno New lines number
*/
COMPEL_EXPORT void COMPEL_API compel_script_set_lineno(
  compel_script_t,
  size_t);

/*!
\brief Evaluates an expression.
The expression is a variable name or names of any sort.
The expression value is the same value as one would use \a var extension to create an initialed variable
\param expr The expression to evaluate
\returns A compel_string_create() string containing the expression value.
Make sure you free the returned buffer through compel_string_destroy
\sa compel_string_destroy
*/
COMPEL_EXPORT char *COMPEL_API compel_script_evaluate_expression(
  compel_script_t compel_script,
  const char *expr,
  bool bKeepQuotes,
  char chDelim);

/*!
\brief Returns the script lines count
*/
COMPEL_EXPORT int COMPEL_API compel_script_get_lines_count(compel_script_t);

/*!
  \brief Returns the script execution line (that is about to be executed).
  \param compel_script Scripting engine reference
*/
COMPEL_EXPORT size_t COMPEL_API compel_script_get_lineno(
  compel_script_t);

/*!
  \brief Initializes the scripting engine.\n
  This is the most important function, for it creates the scripting engine object that is used in most function calls.
  \sa _compel_init_t
*/
COMPEL_EXPORT compel_script_t COMPEL_API compel_script_init(
  p_compel_init_t);

/*!
  \brief Deinitializes and frees the scripting engine
*/
COMPEL_EXPORT void COMPEL_API compel_script_deinit(
  compel_script_t);

/*!
  \brief Associates a user tag with the given script object.\n
  This is useful if you want to refer to some special value from inside a user commands
  \param context Any tag value that user wants to associate with the script object
*/
COMPEL_EXPORT void COMPEL_API compel_script_context_set(
  compel_script_t, 
  const char *guid,
  compel_user_context_t context);

/*!
  \brief Returns the tag associated with the given script object
*/
COMPEL_EXPORT compel_user_context_t COMPEL_API compel_script_context_get(
  compel_script_t compel_script,
  const char *guid);

/*! 
  \brief Sets scripting error handler.
  \param compel_script_error_handler_cb_t The error handler.
  \sa compel_script_error_handler_cb_t
*/
COMPEL_EXPORT void COMPEL_API compel_script_set_error_handler(
  compel_script_t, 
  compel_script_error_handler_cb_t);

/*!
  \brief Loads a script from a file
  \param filename Script's filename
  \returns Compel error if preparse failed or Success.
*/
COMPEL_EXPORT int COMPEL_API compel_script_load_file(
  compel_script_t, 
  const char *filename);

/*!
  \brief Loads a script from a string, separated by the delim
  \param lines The program lines
  \param delims The delimiters characters used to separate each script command
*/
COMPEL_EXPORT bool COMPEL_API compel_script_load_lines(
  compel_script_t, 
  const char *lines, 
  const char *delims);

/*!
  \brief Clears the script lines.\n
*/
COMPEL_EXPORT void COMPEL_API compel_script_clear_lines(
  compel_script_t);

/*!
  \brief Runs the whole script.
  \sa compel_script_step()
*/
COMPEL_EXPORT int COMPEL_API compel_script_run(
  compel_script_t);

/*!
  \brief Executes the current line and stops.
  Executes only one line at a time.
  \returns Library engine error number
  \sa compel_errors_e
*/
COMPEL_EXPORT int COMPEL_API compel_script_step(compel_script_t);

/*! 
  \brief Executes the user's line.\n
  You may use this command to continously interpret commands without clearing previous commands.
  This command will add lines to previously loaded script.
  So if you reset the execution line to ZERO then you will run everything all over again
*/
COMPEL_EXPORT int COMPEL_API compel_script_interpret_line(compel_script_t, const char *);

/*! 
\brief Use this method to announce an external user extension arrival
\param compel_script the script instance
\param extname The extension friendly name
\param extpath The extension path
\returns
- compel_error_symbol_undefined: extension not found
- compel_error_invalid_extension: missing callbacks
- compel_error_success: the extension was loaded
- compel_error_symbol_redefined: if extension name clashes with built-in extension names
- other values: what the init callback returns
*/
COMPEL_EXPORT int COMPEL_API compel_extension_load(
  compel_script_t compel_script, 
  const char *extname,
  const char *extpath);

/*!
\brief Returns the available extensions.\n
The extensions are semicolon separated. Extensions are built-in language commands.
You may chose to disable all built-in extensions by passing no extension to the compel_script_init()
\sa _compel_init_t
*/
COMPEL_EXPORT const char * COMPEL_API compel_script_avail_extensions();

/*!
\brief Returns the lib_usercommand_info_t associated with the currently executing function
This is called from within the user command only
\param compel_script
\returns The pointer to the info struct or NULL
*/
COMPEL_EXPORT p_lib_usercommand_info_t COMPEL_API compel_lu_cmd_get_info(
  compel_script_t compel_script);

/*!
  \brief Allows a user's command to return a value.\n
  This is called from within the user command.
  \param retval The return value.
  \returns Success or failure depending on whether the return value could be changed.
*/
COMPEL_EXPORT bool COMPEL_API compel_lu_cmd_set_retval(
  compel_script_t, 
  const char *retval);

/*!
  \brief Registers a user command.
  \sa _lib_usercommand_info_t
*/
COMPEL_EXPORT int COMPEL_API compel_lu_cmd_register(
  compel_script_t, 
  p_lib_usercommand_info_t);

/*!
  \brief Registers user's command without using the usercommand_info_t structure.
  User defined commands are commands provided by the 3rd party developer and are exposed to the scripting engine
*/
COMPEL_EXPORT int COMPEL_API compel_lu_cmd_register2(
  compel_script_t,
  compel_lib_usercommand_cb_t cb,
  const char *name,
  const size_t minargs,
  const size_t maxargs);

/*!
  \brief Issues an internal command to COMPEL engine.
  This method is used internally only and its interface is partly documented.
  \internal
  \sa compel_internal_e
*/
COMPEL_EXPORT int COMPEL_API compel_internal(compel_script_t, int arg1, int arg2, int arg3, int arg4);

// ----------------------------------------------------------------
// misc and utility functions
// ----------------------------------------------------------------


/*!
  \brief Parses a numeric value from the string.\n
  Given a string, it returns a number
  It can parse hex numbers of the form: 0xNNNN and NNNNh, 
  also parses octal numbers of the form: 0NNN
  \param value_str the string value
  \returns The numeric value or 0 if not a valid numerical string was passed
*/
COMPEL_EXPORT long COMPEL_API compel_parse_number(const char *value);

// -------------------------------------------------------
// string utility functions
// -------------------------------------------------------

/*!
  \brief Creates a string with the given size.\n
  This function simply uses COMPEL's memory manager to allocate a string.
  \param str Initial string value
  \param sz The required size or 0 means length of \a str
  \returns The string pointer
*/ 
COMPEL_EXPORT char * COMPEL_API compel_string_create(const char *str, size_t sz);

/*!
  \brief Destroys a previously allocated string.\n
  This is useful when COMPEL returns strings that you may need to free later using this function.

  \sa compel_string_create
*/
COMPEL_EXPORT void COMPEL_API compel_string_destroy(char *str);

// -------------------------------------------------------
// tokenizing utility
// -------------------------------------------------------

/*!
  \brief Creates a string tokenizer object.\n
  This function also tokenizes, so you can directly extract the values
  \param str The input string
  \param delim The delimiters
  \param quote The quote character
  \param escape The escape characters
  \returns Returns tokenizer object reference
  \sa compel_tokenize_get
*/
COMPEL_EXPORT compel_tokenizer_t COMPEL_API compel_tokenize_init(
  const char *str, 
  const char *delim, 
  const char *quote, 
  const char *escape);

/*!
  \brief Re-tokenizes a string
  \sa compel_tokenize_init
*/
COMPEL_EXPORT int COMPEL_API compel_tokenize_parse(
  compel_tokenizer_t,
  const char *str, 
  const char *delim, 
  const char *quote, 
  const char *escape);

/*!
  \brief Frees the string tokenizer object.
  \param compel_tokenizer Tokenizer object
*/
COMPEL_EXPORT void COMPEL_API compel_tokenize_free(compel_tokenizer_t);

/*!
  \brief Returns the number of tokenized strings
  \param compel_tokenizer_t tokenizer object
  \returns The number of tokenized items.
*/
COMPEL_EXPORT size_t COMPEL_API compel_tokenize_parsed_count(compel_tokenizer_t);

/*!
  \brief Returns a tokenized string at the given index
  \param compel_tokenizer_t tokenizer object
  \param compel_tokenizer_t index
  \returns The value at the given index. It might return NULL if the index is not valid
*/
COMPEL_EXPORT const char * COMPEL_API compel_tokenize_get(
  compel_tokenizer_t, 
  const size_t idx);

/*!
 \brief Patches a tokenized string value at the index.
 \param compel_tokenizer_t tokenizer object
 \param idx The index
 \param str The new value
 \returns The newly patched value
*/
COMPEL_EXPORT const char * COMPEL_API compel_tokenize_set(
  compel_tokenizer_t, 
  const size_t idx, 
  const char *str);

#endif