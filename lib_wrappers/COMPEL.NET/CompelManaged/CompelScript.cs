using System;
using System.Runtime.InteropServices;
namespace CompelManaged
{

	#region Delegates
	/// <summary>
	/// Represents the method that will handle the ErrorOccurred Event
	/// </summary>
	public delegate void UserCommandCB(CompelScript scriptObj, string[] arguments);
	#endregion

	#region CompelException
	/// <summary>
	/// Provides exception handling for the Compel Library
	/// </summary>
	public class CompelException : System.Exception
	{
		/// <summary>
		/// Initializes a new instance of the CompelException class.
		/// </summary>
		/// <param name="exceptionMessage">the exception message value</param>
		public CompelException(string exceptionMessage) : base(exceptionMessage)
		{
		}
	}
	#endregion

	#region ErrorEventArgs
	/// <summary>
	/// Provides data for the ErrorOccurred Event
	/// </summary>
	public class ErrorEventArgs : System.EventArgs
	{
		/// <summary>
		/// the error number
		/// </summary>
		private int libErr;
		/// <summary>
		/// the line number where the error occurred
		/// </summary>
		private int lineNum;
		/// <summary>
		/// the line of code
		/// </summary>
		private string faultyCode;
		/// <summary>
		/// Initializes a new instance of the ErrorEventArgs class.
		/// </summary>
		/// <param name="libErr">the error number</param>
		/// <param name="lineNum">the line number where the error occurred</param>
		/// <param name="faultyCode">the line of code</param>
		public ErrorEventArgs(int libErr,int lineNum,string faultyCode)
		{
			this.libErr=libErr;
			this.lineNum=lineNum;
			this.faultyCode=faultyCode;
		}

		/// <summary>
		/// Gets the error number
		/// </summary>
		public int ErrorNumber
		{
			get{return libErr;}
		}

		/// <summary>
		/// Gets the Line number where the error occurred
		/// </summary>
		public int LineNumber
		{
			get{return lineNum;}
		}

		/// <summary>
		/// Gets the line of code where the error occurred
		/// </summary>
		public string FaultyCode
		{
			get{return faultyCode;}
		}
	}
	#endregion

	#region Event Handlers
	/// <summary>
	/// Represents the method that will handle the ErrorOccurred Event
	/// </summary>
	public delegate void ErrorEventHandler(object sender, ErrorEventArgs e);
	#endregion

	#region CompelScript
	/// <summary>
	/// Provides methods for accessing and manipulating a Compel script
	/// </summary>
	public class CompelScript : IDisposable
	{
		#region class members
		/// <summary>
		/// The script handle
		/// </summary>
		private IntPtr script;
		/// <summary>
		/// The script handle
		/// </summary>
		private System.Collections.ArrayList cbList = new System.Collections.ArrayList();
		/// <summary>
		/// Occurs when an error is found in the script
		/// </summary>
		public event ErrorEventHandler ErrorOccurred;
		#endregion

		#region Constructors
		/// <summary>
		/// Initializes a new instance of the CompelScript class.
		/// </summary>
		public CompelScript()
		{
			InitializeScript();
		}
		
		/// <summary>
		/// Disposes the CompelScript object
		/// </summary>
		~CompelScript()
		{
			this.Dispose();
		}
		/// <summary>
		/// Initializes a new instance of the CompelScript class.
		/// </summary>
		/// <param name="scriptHandle">the handle to an available script</param>
		public CompelScript(IntPtr scriptHandle)
		{
			if(scriptHandle==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));
			this.script = scriptHandle;
		}

		#endregion

		#region Dispose
		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		public void Dispose()
		{
			if (script!=IntPtr.Zero)
			{
				CompelLib.compel_script_deinit(script);	
				script = IntPtr.Zero;
			}
		}
		#endregion
		
		#region Private Methods
		/// <summary>
		/// Initializes a new script
		/// </summary>
		private void InitializeScript()
		{
			_compel_init_t init = new _compel_init_t();

			init.b_usefullns = 0;
			init.b_dbgout_script = 0;

			// get all the avail extensions
			init.extensions = CompelLib.compel_script_avail_extensions();
			
			script = CompelLib.compel_script_init(ref init);
			if(script==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));
			
			// Install an error handler to handle script errors
			CompelLib.compel_script_set_error_handler(script, new ErrorHandlerCallback(this.ScriptErrorHandler));
		}

		/// <summary>
		/// Callback method that handles all script errors
		/// </summary>
		/// <param name="compel_script">the script handle</param>
		/// <param name="lineno">the line number wher the error occurred</param>
		/// <param name="err">the error number</param>
		/// <returns>compel_error_success if successful</returns>
		private int ScriptErrorHandler(IntPtr compel_script, uint lineno, int err)
		{
			// if event handler is registered launch event
			if (ErrorOccurred!=null) 
				ErrorOccurred(this,new ErrorEventArgs(err,(int)lineno,CompelLib.compel_script_get_line(script, lineno)));
			return (int)compel_errors_e.compel_error_success;
		}

		/// <summary>
		/// Retrieve unmanaged allocated string[] from address
		/// </summary>
		/// <param name="args"></param>
		/// <param name="argNumber"></param>
		/// <returns></returns>
		private string[] GetStringArrFromPtr(IntPtr args, int argNumber)
		{
			string[] argVal = new string[argNumber];

			for(int i=0;i<argNumber;i++)
			{				
				//each element in the array contains a 32bit address
				//retrieve the address
				IntPtr ptrArr = Marshal.ReadIntPtr(args, 4*i);

				//retrieve the null terminated string from the address
				argVal[i] = Marshal.PtrToStringAnsi(ptrArr);
			}
			return argVal;
		}

		/// <summary>
		/// Method handling the user command callbacks
		/// </summary>
		/// <param name="compel_script_t">the script handle</param>
		/// <param name="argc">the number of arguments</param>
		/// <param name="argv">the arguments provided</param>
		/// <returns>the resulting error value</returns>
		int ComandCallBack(IntPtr compel_script_t, int argc, IntPtr argv)
		{
			string[] argVal = this.GetStringArrFromPtr(argv, argc);
			IntPtr structHandle = CompelLib.compel_lu_cmd_get_info(script);

			_lib_usercommand_info_t info = 
        (_lib_usercommand_info_t) Marshal.PtrToStructure(structHandle, typeof(_lib_usercommand_info_t));

			UserCommandCB crntCB = (UserCommandCB)cbList[info.context.ToInt32() - 1];
			crntCB(this, argVal);

			return (int)compel_errors_e.compel_error_success;
		}
		#endregion

		#region Public Methods
		/// <summary>
		/// Interprets a line of script
		/// </summary>
		/// <param name="scriptLine">the line to interpret</param>
		public void InterpretLine(string scriptLine)
		{
			if(script==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));

			// Interpret line in script
			CompelLib.compel_script_interpret_line(script, scriptLine);
		}

		/// <summary>
		/// Loads deimiter seperated lines of script
		/// </summary>
		/// <param name="scriptLines">the line to load</param>
		/// <param name="delimiter">the seperator string being used</param>
		public void LoadLines(string scriptLines, string delimiter)
		{
			if(script==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));

			CompelLib.compel_script_load_lines(script, scriptLines, delimiter);
		}

    /// <summary>
    /// Loads deimiter seperated lines of script
    /// </summary>
    /// <param name="filename">the file name to load</param>
    public int LoadFile(string filename)
    {
      if(script==IntPtr.Zero)
        throw(new CompelException("Script handle cannot be null"));

      return CompelLib.compel_script_load_file(script, filename);
    }

		/// <summary>
		/// Finds a variable within the script
		/// </summary>
		/// <param name="variableName">the name of the variable to find</param>
		/// <returns>the found variable</returns>
		public CompelValue FindVariable(string variableName)
		{
			if(script==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));
			// find the value "$"
			if(variableName.Substring(0,1)=="$")
				variableName = variableName.Substring(1,variableName.Length-1);
			IntPtr val = CompelLib.compel_value_find(script, "$" + variableName);
			return new CompelValue(script,val,variableName);
		}

		/// <summary>
		/// Sets the user command return value
		/// </summary>
		/// <param name="retVal">the return value</param>
		/// <returns>true if successful else false</returns>
		public bool SetReturnValue(string retVal)
		{
			return CompelLib.compel_lu_cmd_set_retval(script, retVal);
		}
		/// <summary>
		/// Creates a new variable in the designated script
		/// </summary>
		/// <param name="variableName">the name of the variable</param>
		/// <param name="variableValue">the value of the variable</param>
		/// <returns>the newly created variable</returns>
		public CompelValue CreateVariable(string variableName, string variableValue)
		{
			if(script==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));
			// create a new variable
			IntPtr val = CompelLib.compel_value_create(script, "$" + variableName, variableValue);
			return new CompelValue(script,val,variableName);
		}

		/// <summary>
		/// Creates a new object in the designated script
		/// </summary>
		/// <param name="objectName">the name of the object</param>
		/// <returns>the newly created object</returns>
		public CompelObject CreateObject(string objectName)
		{
			if(script==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));
			// create a new variable
			IntPtr obj = CompelLib.compel_object_create(script, "$" + objectName);
			return new CompelObject(script, obj, objectName);
		}

		/// <summary>
		/// Finds an object within the script
		/// </summary>
		/// <param name="objectName">the name of the object to find</param>
		/// <returns>the found object</returns>
		public CompelObject FindObject(string objectName)
		{
			if(script==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));

			// find the value "$"
			IntPtr obj = CompelLib.compel_object_find(script, "$" + objectName);
			return new CompelObject(script,obj,objectName);
		}

		/// <summary>
		/// Creates a custom user command in the designated script
		/// </summary>
		/// <param name="commandName">the command name</param>
		/// <param name="argMin">the minimum number of arguments for the command</param>
		/// <param name="argMax">the maximum number of arguments for the command</param>
		/// <param name="cmdCB">the callback funtion that is called</param>
		/// <returns>the resulting value of creating the user command</returns>
		public int CreateUserCommand(string commandName, int argMin, int argMax, UserCommandCB cmdCB)
		{
			if(script==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));

			// create a new user command
			// register a command, the long way
			_lib_usercommand_info_t cmd = new _lib_usercommand_info_t();
			cmd.cb = new CommandCallback(this.ComandCallBack);
			cmd.minargs = (uint)argMin;
			cmd.maxargs = (uint)argMax;
			cmd.name = commandName;
			cmd.context = (IntPtr)(cbList.Count+1);
			cbList.Add(cmdCB);
			return CompelLib.compel_lu_cmd_register(script, ref cmd);
		}

    //
    /// <summary>
    /// Clear script lines
    /// </summary>
    public void ClearLines()
    {
      CompelLib.compel_script_clear_lines(script);
    }

    //
    /// <summary>
    /// Sets the script executing line
    /// </summary>
    public void SetLineNo(int lineno)
    {
      CompelLib.compel_script_set_lineno(script, lineno);
    }

		/// <summary>
		/// Run the currently loaded script
		/// </summary>
		/// <returns>the resulting error value</returns>
		public int Run()
		{
			return CompelLib.compel_script_run(script);
		}

		/// <summary>
		/// Run the currently loaded script
		/// </summary>
		/// <returns>the resulting error value</returns>
		public int Pause()
		{
			return CompelLib.compel_internal(script,(int)compel_internal_e.compel_internal_pause,0,0,0);
		}

		/// <summary>
		/// Gets the designated variable handle.
		/// </summary>
		public IntPtr ScriptHandle
		{
			get
			{
				return this.script;
			}
		}
		#endregion
	}
	#endregion
}
