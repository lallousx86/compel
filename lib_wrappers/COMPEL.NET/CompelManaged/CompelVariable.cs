using System;

namespace CompelManaged
{
	/// <summary>
	/// Provides methods for accessing a script variable value
	/// </summary>
	public class CompelValue : CompelVariable
	{
		/// <summary>
		/// Initializes a new instance of the CompelValue class.
		/// </summary>
		/// <param name="script">the script handle to which this variable belongs</param>
		/// <param name="val">the handle of the variable</param>
		/// <param name="name">the variable name</param>
		public CompelValue(IntPtr script,IntPtr val,string name) : base (script,val,name)
		{
		}

		/// <summary>
		/// Prints to the console the variable value
		/// </summary>
		private void PrintValue()
		{
			CompelLib.compel_script_interpret_line(this.script, "echo $" + this.name);
		}

		/// <summary>
		/// Gets the designated variable name.
		/// </summary>
		public string Name
		{
			get
			{
				// get the variable's value
				return this.name;
			}
		}

		/// <summary>
		/// Gets or Sets the designated variable value.
		/// </summary>
		public string Value
		{
			get
			{
				// get the variable's value
				return CompelLib.compel_value_get(script, val);
			}
			set
			{
				// update the variable's value
				CompelLib.compel_value_set(script, val, value);
			}
		}

	}
}
