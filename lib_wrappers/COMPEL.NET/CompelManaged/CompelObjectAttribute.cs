using System;

namespace CompelManaged
{
	/// <summary>
	/// Provides methods for accessing a script Object Attribute
	/// </summary>
	public class CompelObjectAttribute : CompelVariable
	{
		/// <summary>
		/// Initializes a new instance of the CompelObjectAttribute class.
		/// </summary>
		/// <param name="script">the script handle to which this variable belongs</param>
		/// <param name="val">the handle of the variable</param>
		/// <param name="name">the variable name</param>
		public CompelObjectAttribute(IntPtr script,IntPtr val,string name) : base (script,val,name)
		{
		}

		/// <summary>
		/// Gets the designated attribute name.
		/// </summary>
		public string Name
		{
			get
			{
				// get the attribute's value
				return this.name;
			}
		}

		/// <summary>
		/// Gets or Sets the designated attribute value.
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
