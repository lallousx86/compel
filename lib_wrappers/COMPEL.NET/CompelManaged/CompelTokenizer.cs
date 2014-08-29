using System;

namespace CompelManaged
{
	/// <summary>
	/// Provides methods for tokenizing strings using the Compel Library
	/// </summary>
	public class CompelTokenizer : IDisposable
	{
		/// <summary>
		/// the handle of the tokenizer
		/// </summary>
		private IntPtr tok;
		/// <summary>
		/// The input string
		/// </summary>
		private string inputStr;
		/// <summary>
		/// The delimiters
		/// </summary>
		private string delimiter;
		/// <summary>
		/// The quote character
		/// </summary>
		private string quote;
		/// <summary>
		/// The escape characters
		/// </summary>
		private string escape;

		/// <summary>
		///	Initializes a new instance of the CompelTokenizer class.
		/// </summary>
		/// <param name="inputStr">The input string</param>
		/// <param name="delimiter">The delimiters</param>
		public CompelTokenizer(string inputStr, string delimiter)
		{
			this.inputStr = inputStr;
			this.delimiter=delimiter;
			this.quote="\"";
			this.escape=null;
			tok = CompelLib.compel_tokenize_init(inputStr,delimiter, quote, escape);
		}

		/// <summary>
		/// Disposes the CompelObject object
		/// </summary>
		~CompelTokenizer()
		{
			this.Dispose();
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		public void Dispose()
		{
			if(tok!=IntPtr.Zero)
			{
				CompelLib.compel_tokenize_free(tok);	
				tok=IntPtr.Zero;
			}
		}

		/// <summary>
		/// Gets the number of elements after being tokenized
		/// </summary>
		public int Count
		{
			get
			{
				return (int)CompelLib.compel_tokenize_parsed_count(tok);
			}
		}

		/// <summary>
		/// Gets or Sets the delimiter
		/// </summary>
		public string Delimiter
		{
			get
			{
				return this.delimiter;
			}
			set
			{
				this.delimiter = value;

				// Reparse string
				CompelLib.compel_tokenize_parse(tok,inputStr,delimiter, quote, escape);
			}
		}

		/// <summary>
		/// Gets or Sets the Quote character
		/// </summary>
		public string Quote
		{
			get
			{
				return this.quote;
			}
			set
			{
				this.quote = value;
				// Reparse string
				CompelLib.compel_tokenize_parse(tok,inputStr,delimiter, quote, escape);
			}
		}

		/// <summary>
		/// Gets or Sets the escape character
		/// </summary>
		public string Escape
		{
			get
			{
				return this.escape;
			}
			set
			{
				this.escape = value;
				//reparse string
				CompelLib.compel_tokenize_parse(tok,inputStr,delimiter, quote, escape);
			}
		}

		/// <summary>
		/// Gets or Sets the input string
		/// </summary>
		public string InputString
		{
			get
			{
				return this.inputStr;
			}
			set
			{
				this.inputStr = value;
				// Reparse string
				CompelLib.compel_tokenize_parse(tok,inputStr,delimiter, quote, escape);
			}
		}

		/// <summary>
		/// returns the value at the index
		/// </summary>
		public string this[int index]  
		{
			get  
			{
				if(tok==IntPtr.Zero)
					throw(new CompelException("Tokenizer handle cannot be null"));
				return( CompelLib.compel_tokenize_get(tok, (uint)index) );
			}
			set  
			{
				if(tok==IntPtr.Zero)
					throw(new CompelException("Tokenizer handle cannot be null"));
				CompelLib.compel_tokenize_set(tok, (uint)index, value);

			}
		}

	}
}
