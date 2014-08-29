using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace CompelPaint
{
	/// <summary>
	/// Summary description for frmConsole.
	/// </summary>
	public class frmConsole : System.Windows.Forms.Form
	{
		public System.Windows.Forms.TextBox txtScript;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public frmConsole()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if(components != null)
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
      this.txtScript = new System.Windows.Forms.TextBox();
      this.SuspendLayout();
      // 
      // txtScript
      // 
      this.txtScript.Dock = System.Windows.Forms.DockStyle.Fill;
      this.txtScript.Font = new System.Drawing.Font("Courier New", 14.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
      this.txtScript.Location = new System.Drawing.Point(0, 0);
      this.txtScript.Multiline = true;
      this.txtScript.Name = "txtScript";
      this.txtScript.ScrollBars = System.Windows.Forms.ScrollBars.Both;
      this.txtScript.Size = new System.Drawing.Size(618, 132);
      this.txtScript.TabIndex = 0;
      this.txtScript.Text = "";
      // 
      // frmConsole
      // 
      this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
      this.ClientSize = new System.Drawing.Size(618, 132);
      this.Controls.Add(this.txtScript);
      this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
      this.Name = "frmConsole";
      this.Text = "Console";
      this.ResumeLayout(false);

    }
		#endregion

		/// <summary>
		/// catches the Windows Message to process. All messages are sent to 
		/// the WndProc method after getting filtered through the PreProcessMessage method.
		/// </summary>
		/// <param name="m">the message being caught</param>
		protected override void WndProc(ref Message m)
		{
			if(m.Msg ==0x112)
			{
				if(m.WParam.ToInt32() == 0xF060)
					this.Hide();
				else if (m.WParam.ToInt32() == 0xF030 || m.WParam.ToInt32() == 0xF120)
				{//catch SC_MAXIMIZE SC_RESTORE
					//catch the maximize windows menu click
					return;
				}
			}
			else
				base.WndProc(ref m);
		}
	}
}
