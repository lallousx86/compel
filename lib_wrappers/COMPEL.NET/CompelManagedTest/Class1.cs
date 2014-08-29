using System;
using CompelManaged;
namespace CompelManagedTest
{
	public delegate void RunCommandDelegate(CompelScript script);
	/// <summary>
	/// Summary description for Class1.
	/// </summary>
	class ClassTest
	{
		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main(string[] args)
		{
			ClassTest test = new ClassTest();
			//test.TestCompelVariableManaged();
			//test.TestCompelObjectManaged();
			test.TestCompelCommandManaged();
			//test.TestCompelTokenizedManaged();
			//test.TestCompelPauseScriptManaged();
			//test.TestCompelErrorHandlingManaged();
			Console.ReadLine();
		}

		public void TestCompelErrorHandlingManaged()
		{
			CompelScript script = new CompelScript();
			script.ErrorOccurred+=new ErrorEventHandler(script_ErrorOccurred);
			script.LoadLines("echoln okay;" +
				"some_bad_command;" +
				"echoln after_bad_command", ";");
			script.Run();
		}

		private void script_ErrorOccurred(object sender, ErrorEventArgs e)
		{
			Console.WriteLine("error handler caught error %d @ %d:\n>%s<\n", e.ErrorNumber.ToString(), e.LineNumber.ToString(), e.FaultyCode);
		}

		public void TestCompelPauseScriptManaged()
		{
			CompelScript script = new CompelScript();
			RunCommandDelegate fxn = new RunCommandDelegate(this.PauseThread);
			fxn.BeginInvoke(script,null,null);
			script.LoadLines("var $i 0|for $i 0 to 10000|{|echoln \"i=\" $i|}|", "|");
			script.Run();
		}

		private void PauseThread(CompelScript script)
		{
			Console.ReadLine();
			Console.WriteLine("will pause...");
			script.Pause();
			Console.WriteLine("paused...\n");
		}

		public void TestCompelTokenizedManaged()
		{
			CompelTokenizer tokenize = new CompelTokenizer("a;b;c;d",";");
			Console.WriteLine("Count = " + tokenize.Count.ToString());
			int count =tokenize.Count;
			for(int i=0; i<count ; i++)
				Console.WriteLine(i.ToString() + "= " + tokenize[i]);
			tokenize.Dispose();

		}
		
		public void TestCompelCommandManaged()
		{
			CompelScript script1 = new CompelScript();

			script1.CreateUserCommand("myLowerCase",1,1, new UserCommandCB(this.myLowerCase));
			script1.CreateUserCommand("myUpperCase",2,2, new UserCommandCB(this.myUpperCase));

			string lines = "var $s1 \"tHiS iS a StRiNg\";" +
				"var $s2 $s1;" +
				"echoln \"before user command calls: \" $s1;" +
				"myLowerCase $s1;" +
				"echoln \"lowercase: \" $myLowerCase;" +
				"myUpperCase &$s2 $s1;" +
				"echoln \"uppercase: \" $s2;";
			script1.LoadLines(lines, ";");
			script1.Run();
			script1.Dispose();
		}

		public void TestCompelVariableManaged()
		{
			CompelScript script = new CompelScript();
			script.InterpretLine("var $s value!");
			CompelValue var = script.FindVariable("s");
			string s = var.Value;
			
			// patch string - enclose in angle brackets
			s = "<" + s + ">";

			// update the variable's value
			var.Value=s;
			script.InterpretLine("echoln s= $s");

			CompelValue var2 = script.CreateVariable("el","elias");
			script.InterpretLine("echoln el = $el");

			script.Dispose();

		}
		public void TestCompelObjectManaged()
		{
			CompelScript script = new CompelScript();

			script.LoadLines("var $obj1.|var $obj1.name john|var $obj1.lastname doe", "|");
			script.Run();
			
			CompelObject obj = script.CreateObject("obj2");

			// create attribute in obj2
			for (int i=0;i<10;i++)
			{
				obj.AddAttribute("attr" + i.ToString(), "attr" + i.ToString());
			}

			script.InterpretLine("echoln obj2: $obj2");
			obj.Dispose();

			obj = script.FindObject("obj1");
			
			CompelObjectAttribute attr = obj.FindAttribute("name");
			
			attr.Value="elias";

			script.InterpretLine("echoln before_lastname_remove: $obj1");
			
			obj.RemoveAttribute("lastname");
			
			script.InterpretLine("echoln after_lastname_remove: $obj1");

			//			obj = CompelLib.compel_object_find(script, "$_COMPEL");


			script.Dispose();		
			Console.ReadLine();

		}
		
		void myUpperCase(CompelScript scriptObj, string[] argVal)
		{
			CompelValue var = scriptObj.FindVariable(argVal[0]);
			var.Value=argVal[1].ToUpper();
		}

		void myLowerCase(CompelScript scriptObj, string[] argVal)
		{
			scriptObj.SetReturnValue(argVal[0].ToLower());
		}
	
	}
}
