using System;

namespace CompelManaged
{
	/// <summary>
	/// Provides methods for accessing a script object
	/// </summary>
	public class CompelObject : CompelVariable,IDisposable
	{
		/// <summary>
		/// Initializes a new instance of the CompelValue class.
		/// </summary>
		/// <param name="script">the script handle to which this object belongs</param>
		/// <param name="obj">the handle of the object</param>
		/// <param name="name">the object name</param>
		public CompelObject(IntPtr script,IntPtr obj,string name) : base (script,obj,name)
		{
		}

		/// <summary>
		/// Disposes the CompelObject object
		/// </summary>
		~CompelObject()
		{
			this.Dispose();
		}
		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		public void Dispose()
		{
			if(val!=IntPtr.Zero)
			{
				// delete the object "obj2"
				CompelLib.compel_object_destroy(script, "$" + name);
				val=IntPtr.Zero;
			}
		}


		/// <summary>
		/// Creates a new attribute in the designated object
		/// </summary>
		/// <param name="attributeName">the name of the attribute</param>
		/// <param name="attributeValue">the value of the attribute</param>
		/// <returns>the newly created attribute</returns>
		public CompelObjectAttribute AddAttribute(string attributeName, string attributeValue)
		{
			if(script==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));
			if(val==IntPtr.Zero)
				throw(new CompelException("Object handle cannot be null"));
			// create a new attribute
			IntPtr attr = CompelLib.compel_object_add_attr(script, val, attributeName, attributeValue);
			return new CompelObjectAttribute(script,attr,attributeName);
		}

		/// <summary>
		/// Finds an attribute within the object
		/// </summary>
		/// <param name="attributeName">the name of the attribute to find</param>
		/// <returns>the found attribute</returns>
		public CompelObjectAttribute FindAttribute(string attributeName)
		{
			if(script==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));
			if(val==IntPtr.Zero)
				throw(new CompelException("Object handle cannot be null"));
			// find the value "$"
			IntPtr attr = CompelLib.compel_object_find_attr(script,val, attributeName);
			return new CompelObjectAttribute(script,attr,attributeName);
		}

		/// <summary>
		/// Removes an attribute from the object
		/// </summary>
		/// <param name="attributeName">the name of the attribute to remove</param>
		/// <returns>true if attribute removed successfully else false</returns>
		public bool RemoveAttribute(string attributeName)
		{
			if(script==IntPtr.Zero)
				throw(new CompelException("Script handle cannot be null"));
			if(val==IntPtr.Zero)
				throw(new CompelException("Object handle cannot be null"));
			// remove an attribute
			return CompelLib.compel_object_remove_attr(script, val, attributeName);
		}
	}
}
