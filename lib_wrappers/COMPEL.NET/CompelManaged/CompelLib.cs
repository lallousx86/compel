using System;
using System.Runtime.InteropServices;

namespace CompelManaged
{
	#region Enumerators
	/// <summary>
	/// The Compel System Definitions
	/// </summary>
	public enum compel_errors_e : int
	{
		/// <summary>
		/// Success
		/// </summary>
		compel_error_success                       = 0, 

		/// <summary>
		/// An undefined symbol occured
		/// </summary>
		compel_error_symbol_undefined              = 1, 

		/// <summary>
		/// Script syntax error
		/// </summary> 
		compel_error_syntax_error                  = 2, 

		/// <summary>
		/// The error handler decided to pass the error to next handlers
		/// </summary>
		compel_error_error_handler_continue_search = 3,

		/// <summary>
		/// Wrong parameters passed to a given command, eiter more or less commands passed
		/// </summary>
		compel_error_wrong_param_count             = 4, 

		/// <summary>
		/// A command call was expected
		/// </summary>
		compel_error_function_expected             = 5,

		/// <summary>
		/// The script has been stoped either because it encountered END or fatal script error
		/// </summary>
		compel_error_script_stopped                = 6,

		/// <summary>
		/// Symbol was redefined
		/// </summary>
		compel_error_symbol_redefined              = 7,

		/// <summary>
		/// An unknown error occured. This should usually never occur.
		/// </summary>
		compel_error_unknown_error                 = 8,

		/// <summary>
		/// We adjusted script line number, the engine should not advance the script line number.
		///	The user command changed the source line number, thus the engine should not advance to next line.
		///	This is useful if you write some sort of branching commands.
		/// </summary>
		compel_error_branch_handled                = 9,

		/// <summary>
		/// A symbol rather than a constant was expected. The symbol may
		/// </summary> 
		compel_error_symbol_expected               = 10,

		/// <summary>
		/// A wrong parameter value was generated or passed.
		///	For example, a divide by zero operation might return this value
		/// </summary>
		compel_error_wrong_param_value             = 11,

		/// <summary>
		/// The instruction yields nothing because it can be:
		///	- empty line
		///	- void line
		///	- comment line
		/// </summary>
		compel_error_no_operation                  = 12,

		/// <summary>
		/// The extension structure is not valid!
		/// </summary>
		compel_error_invalid_extension             = 13

	}

	/// <summary>
	/// COMPEL internal command codes. These codes are used with compel_internal().
	///	The \a arg1 parameter can hold any of the listed values.
	///	The rest of the arguments depend on the \a arg1 value
	/// </summary>
	enum compel_internal_e : int
	{
		/// <summary>
		/// This allows you to write the prepared (processed) script lines into a file
		///	arg2 can optionally point to a pointer to the raw file name.
		///	It returns 0 if failure to write or 1 if write successful
		/// </summary>
		compel_internal_writeraw = 1,
		
		/// <summary>
		/// This will enable or disable the outputing of the interpreted lines.
		/// arg2 can be 0 or 1 to enable or disable
		/// \sa _compel_init_t b_dbgout_script
		/// </summary>
		compel_internal_setdbgout = 2,

		/// <summary>
		/// This flag will allow COMPEL engine to dump all the script lines to stdout
		/// </summary>
		compel_internal_showlines = 3,

		/// <summary>
		/// This flag will allow COMPEL to dump the contents of the symbol table to stdout
		/// </summary>
		compel_internal_showsymtbl = 4,

		/// <summary>
		/// Interrupts the execution of the script
		/// </summary>
		compel_internal_pause = 5,
		/// <summary>
		/// Get the execution time in milliseconds
		/// </summary>
		compel_internal_exectime = 6,

    /// <summary>
    /// Returns the preparse failing line number
    /// </summary>
    compel_internal_preparse_failing_line = 7
	};
	#endregion

	#region delegates
	/// <summary>
	/// This is the callback function used by the user defined commands.
	/// </summary>
	public delegate int CommandCallback(IntPtr compel_script, int argc, IntPtr argv);
	/// <summary>
	/// This is the callback function used when error occurrs.
	/// </summary>
	public delegate int ErrorHandlerCallback(IntPtr compel_script, uint lineno, int err);
	#endregion

	#region _compel_init_t
	/// <summary>
	/// COMPEL library initialization structure.
	/// </summary>
	[StructLayout(LayoutKind.Sequential,Pack=1)]
	public struct _compel_init_t 
	{
		/// <summary>
		/// 
		/// </summary>
		public byte      b_usefullns;
		/// <summary>
		/// 
		/// </summary>
		public byte      b_dbgout_script;
		/// <summary>
		/// The arguments passed to the script.
		/// You may access these arguments from the script using the $ARGS variable.
		///	The $ARGS.0 value is always set to the script's file name (if loaded from a file), or to "string" if
		///	the string was loaded from command line
		/// </summary>
		public string    script_args;
		/// <summary>
		/// The script's file name.
		/// If this is set, you later no need to specify the script file name in the "compel_script_load_file".
		/// </summary>
		public string    script_file;
		/// <summary>
		/// The desired built-in extensions you want to enable in the scripting engine.
		/// The extension names must be \a ; separated.
		/// </summary>
		public string    extensions;
	}
	#endregion

	#region _lib_usercommand_info_t
	/// <summary>
	/// User command registration structure. Used when registering a user command
	/// </summary>
	[StructLayout(LayoutKind.Sequential,Pack=1)]
	public struct _lib_usercommand_info_t 
	{
		/// <summary>
		/// the name of the user function
		/// </summary>
		public string	name;
		/// <summary>
		/// min arguments of the command
		/// </summary>
		public uint		minargs;
		/// <summary>
		/// max arguments of the command
		/// </summary>
		public uint		maxargs;
		/// <summary>
		/// the callback
		/// </summary>
		public CommandCallback   cb;
		/// <summary>
		/// user command's description
		/// </summary>
		public string   desc;
		/// <summary>
		/// user defined information
		/// </summary>
		public IntPtr context;
	}
	#endregion

	#region CompelLib class
	/// <summary>
	/// Provides the P-Invoke Managed interface that interacts with the Compel Engine
	/// </summary>
	public class CompelLib
	{
		/// <summary>
		/// compel_script_avail_extensions
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern string compel_script_avail_extensions();

		/// <summary>
		/// Initializes the scripting engine.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern IntPtr compel_script_init(ref _compel_init_t p_compel_init_t);

		/// <summary>
		/// Deinitializes and frees the scripting engine
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void compel_script_deinit(IntPtr compel_script_t);

		/// <summary>
		/// Executes the user's line.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern int compel_script_interpret_line(IntPtr compel_script_t, string lineValue);

		/// <summary>
		/// Clears script lines
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public static extern int compel_script_clear_lines(IntPtr compel_script_t);

		/// <summary>
		/// finds a value by name
		/// </summary>
		[DllImport(@"compel_lib.dll" , CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern IntPtr compel_value_find(IntPtr compel_script_t, string variable);

		/// <summary>
		/// Sets the script's line number
		/// </summary>
		[DllImport(@"compel_lib.dll" , CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
    public static extern IntPtr compel_script_set_lineno(IntPtr compel_script_t, int lineno);

    
		/// <summary>
		/// Returns a variable's value
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern string compel_value_get(IntPtr compel_script_t, IntPtr compel_value);

		/// <summary>
		/// Returns a compel_value_t's value by variable name
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern string compel_value_value_get(IntPtr compel_script_t, string val_name);

		/// <summary>
		/// updates a value_t's value
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void compel_value_set(IntPtr compel_script_t, IntPtr compel_value, string newValue);

		/// <summary>
		/// Creates a variable
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern IntPtr compel_value_create(IntPtr compel_script_t, string varname, string initial_value);

		/// <summary>
		/// Issues an internal command to COMPEL engine.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern int compel_internal(IntPtr compel_script_t, int arg1,int arg2,int arg3,int arg4);

		/// <summary>
		/// Loads a script from a string, separated by the delim
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern bool compel_script_load_lines(IntPtr compel_script_t, string lines,string delims);
		
    /// <summary>
    /// Loads a script from a string, separated by the delim
    /// </summary>
    [DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
    public static extern int compel_script_load_file(IntPtr compel_script_t, string filename);

    /// <summary>
		/// Runs the whole script.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern int compel_script_run(IntPtr compel_script_t);

		/// <summary>
		/// Creates an object.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern IntPtr compel_object_create(IntPtr compel_script_t, string objname);

		/// <summary>
		/// Adds an attribute to an object
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern IntPtr compel_object_add_attr(IntPtr compel_script_t, IntPtr compelObj, string attrname, string initial_value);

		/// <summary>
		/// Destroys an object
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern bool compel_object_destroy(IntPtr compel_script_t, string objName);

		/// <summary>
		/// Finds an object by name
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern IntPtr compel_object_find(IntPtr compel_script_t, string objName);

		/// <summary>
		/// Finds an attribute within an object
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern IntPtr compel_object_find_attr(IntPtr compel_script_t, IntPtr compelObj, string attrname);

		/// <summary>
		/// Removes an attribute from an object
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern bool compel_object_remove_attr(IntPtr compel_script_t, IntPtr compelObj, string attrname);

		/// <summary>
		/// Registers a user command
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern int compel_lu_cmd_register(IntPtr compel_script_t, ref _lib_usercommand_info_t userCommandStruct);

		/// <summary>
		/// Registers user's command without using the usercommand_info_t structure.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern int compel_lu_cmd_register2(IntPtr compel_script_t, CommandCallback cb, string name, uint minargs,uint maxargs);

		/// <summary>
		/// Allows a user's command to return a value.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern bool compel_lu_cmd_set_retval(IntPtr compel_script_t, string retval);


		/// <summary>
		/// Creates a string tokenizer object.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern IntPtr compel_tokenize_init(string str, string delim, string  quote, string escape);

		/// <summary>
		/// Creates a string tokenizer object.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void compel_tokenize_free(IntPtr tok);

		/// <summary>
		/// Creates a string tokenizer object.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern uint compel_tokenize_parsed_count(IntPtr tok);
		
		/// <summary>
		/// Re-tokenizes a string
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern int compel_tokenize_parse(IntPtr tok, string str, string delim, string  quote, string escape);

		/// <summary>
		/// Returns a tokenized string at the given index
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern string compel_tokenize_get(IntPtr tok, uint idx);

		/// <summary>
		/// Patches a tokenized string value at the index.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern string compel_tokenize_set(IntPtr tok, uint idx, string str);

		/// <summary>
		/// Sets scripting error handler.
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern int compel_script_set_error_handler(IntPtr compel_script_t, ErrorHandlerCallback cb);

		/// <summary>
		/// Retrieves the source line value
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern string compel_script_get_line(IntPtr compel_script_t, uint lineNumber);

		/// <summary>
		/// Retrieves the source line value
		/// </summary>
		[DllImport(@"compel_lib.dll", CallingConvention=CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern IntPtr compel_lu_cmd_get_info(IntPtr compel_script_t);

    /// <summary>
    /// Constructor
    /// </summary>
    public CompelLib()	
		{
		}
	}
	#endregion
}
