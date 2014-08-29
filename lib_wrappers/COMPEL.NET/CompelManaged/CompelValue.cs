using System;

namespace CompelManaged
{
	/// <summary>
	/// Provides methods for accessing a script value
	/// </summary>
	public class CompelVariable
	{
		/// <summary>
		/// the handle of the variable
		/// </summary>
		protected IntPtr val;
		/// <summary>
		/// the script handle to which this variable belongs
		/// </summary>
		protected IntPtr script;
		/// <summary>
		/// the variable name
		/// </summary>
		protected string name;
		/// <summary>
		/// the variable value
		/// </summary>
		protected string varVal;
		/// <summary>
		/// Initializes a new instance of the CompelVariable class.
		/// </summary>
		/// <param name="script">the script handle to which this variable belongs</param>
		/// <param name="val">the handle of the variable</param>
		/// <param name="name">the variable name</param>
		public CompelVariable(IntPtr script,IntPtr val,string name)
		{
			this.val=val;
			this.script=script;
			this.name=name;
		}
		
		/// <summary>
		/// Gets the designated variable handle.
		/// </summary>
		public IntPtr Handle
		{
			get
			{
				return this.val;
			}
		}
	}
}
