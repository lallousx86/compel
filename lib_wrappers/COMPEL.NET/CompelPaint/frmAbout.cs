#define PROTECT_APP
using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
//using SS.Util;
using System.Diagnostics;
using System.IO;
using System.Security.Cryptography;
using System.Resources;
using System.Threading;

namespace CompelPaint
{
	/// <summary>
	/// the Class frmAbout is an about dialog used to show the product information
	/// </summary>
	public class frmAbout : System.Windows.Forms.Form
	{
		#region Members
		/// <summary>
		/// variable of type string used to hold temporary data
		/// </summary>
		private string eEgg="";

		private System.Windows.Forms.Button btnOK;
		private System.Windows.Forms.PictureBox pictureBox1;
		private System.Windows.Forms.Label lblWarning;
		private System.Windows.Forms.Label lblTitle;
		private System.Windows.Forms.Label lblCopyRight;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;
		#endregion

		#region Constructor
		/// <summary>
		/// Class Constructor used to initialize the private data members of a class
		/// </summary>
		public frmAbout()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
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
		#endregion

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
      System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(frmAbout));
      this.lblWarning = new System.Windows.Forms.Label();
      this.btnOK = new System.Windows.Forms.Button();
      this.pictureBox1 = new System.Windows.Forms.PictureBox();
      this.lblTitle = new System.Windows.Forms.Label();
      this.lblCopyRight = new System.Windows.Forms.Label();
      this.SuspendLayout();
      // 
      // lblWarning
      // 
      this.lblWarning.Location = new System.Drawing.Point(16, 128);
      this.lblWarning.Name = "lblWarning";
      this.lblWarning.Size = new System.Drawing.Size(368, 80);
      this.lblWarning.TabIndex = 0;
      this.lblWarning.Text = @"Warning: this computer program is protected by copyright law and internaltional treaties. Unauthorized reproduction or distribution of this program, or any portion of it, may result in severe  civil and criminal penalties, and will be prosecuted to the maximum extent possible under the law.";
      // 
      // btnOK
      // 
      this.btnOK.FlatStyle = System.Windows.Forms.FlatStyle.System;
      this.btnOK.Font = new System.Drawing.Font("Tahoma", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
      this.btnOK.ForeColor = System.Drawing.SystemColors.WindowText;
      this.btnOK.Location = new System.Drawing.Point(288, 224);
      this.btnOK.Name = "btnOK";
      this.btnOK.Size = new System.Drawing.Size(80, 24);
      this.btnOK.TabIndex = 18;
      this.btnOK.Text = "OK";
      this.btnOK.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.btnOK_KeyPress);
      this.btnOK.Click += new System.EventHandler(this.btnOK_Click);
      // 
      // pictureBox1
      // 
      this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
      this.pictureBox1.Location = new System.Drawing.Point(16, 8);
      this.pictureBox1.Name = "pictureBox1";
      this.pictureBox1.Size = new System.Drawing.Size(90, 90);
      this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
      this.pictureBox1.TabIndex = 19;
      this.pictureBox1.TabStop = false;
      // 
      // lblTitle
      // 
      this.lblTitle.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
      this.lblTitle.Location = new System.Drawing.Point(120, 16);
      this.lblTitle.Name = "lblTitle";
      this.lblTitle.Size = new System.Drawing.Size(200, 40);
      this.lblTitle.TabIndex = 20;
      this.lblTitle.Text = "Compel Paint® 1.2";
      // 
      // lblCopyRight
      // 
      this.lblCopyRight.Location = new System.Drawing.Point(112, 72);
      this.lblCopyRight.Name = "lblCopyRight";
      this.lblCopyRight.Size = new System.Drawing.Size(248, 40);
      this.lblCopyRight.TabIndex = 21;
      this.lblCopyRight.Text = "Copyright© 2006 by Jimmy Badawi. All rights reserved.";
      // 
      // frmAbout
      // 
      this.AcceptButton = this.btnOK;
      this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
      this.ClientSize = new System.Drawing.Size(394, 264);
      this.Controls.Add(this.lblCopyRight);
      this.Controls.Add(this.lblTitle);
      this.Controls.Add(this.pictureBox1);
      this.Controls.Add(this.btnOK);
      this.Controls.Add(this.lblWarning);
      this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
      this.MaximizeBox = false;
      this.MinimizeBox = false;
      this.Name = "frmAbout";
      this.ShowInTaskbar = false;
      this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
      this.Text = "About";
      this.Load += new System.EventHandler(this.frmAbout_Load);
      this.Paint += new System.Windows.Forms.PaintEventHandler(this.frmAbout_Paint);
      this.ResumeLayout(false);

    }
		#endregion

		#region Events
		private void frmAbout_Load(object sender, System.EventArgs e)
		{
		}
		/// <summary>
		/// Function handeling the paint of the form
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private void frmAbout_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
		{
			System.Drawing.Graphics temp=System.Drawing.Graphics.FromHwnd(this.Handle);
			temp.DrawLine(new System.Drawing.Pen(System.Drawing.Color.LightGray,3),10,120,370,120);
			temp.Dispose();
		}

		/// <summary>
		/// when the OK button is pressed the form will close
		/// </summary>
		/// <param name="sender">the object sending the event</param>
		/// <param name="e">the event argument</param>
		private void btnOK_Click(object sender, System.EventArgs e)
		{
			this.Close();
		}

		/// <summary>
		/// Event Handling the keyPress on the Button
		/// </summary>
		/// <param name="sender">the object sending the event</param>
		/// <param name="e">the event argument</param>
		private void btnOK_KeyPress(object sender, System.Windows.Forms.KeyPressEventArgs e)
		{
			switch (eEgg.Length)
			{
				case 0:
					if (e.KeyChar.ToString().ToLower()=="j")
						eEgg+=e.KeyChar.ToString().ToLower();
					else
						eEgg="";
					break;
				case 1:
					if (e.KeyChar.ToString().ToLower()=="i")
						eEgg+=e.KeyChar.ToString().ToLower();
					else
						eEgg="";
					break;
				case 2:
					if (e.KeyChar.ToString().ToLower()=="m")
						eEgg+=e.KeyChar.ToString().ToLower();
					else
						eEgg="";
					break;
				case 3:
					if (e.KeyChar.ToString().ToLower()=="m")
						eEgg+=e.KeyChar.ToString().ToLower();
					else
						eEgg="";
					break;
				case 4:
					if (e.KeyChar.ToString().ToLower()=="y")
					{
						MessageBox.Show("Jimmy has Launched an Egg\nThe Egg says Hello.","Jimmy");
						eEgg="";
					}
					else
						eEgg="";
					break;
				default:
					eEgg="";
					break;
			}
		}
		#endregion
	}
}
