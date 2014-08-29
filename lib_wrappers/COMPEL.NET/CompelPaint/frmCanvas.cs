using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace CompelPaint
{
	public delegate void mouseMoveHandler(object sender, System.Windows.Forms.MouseEventArgs e);
	/// <summary>
	/// Summary description for frmCanvas.
	/// </summary>
	public class frmCanvas : System.Windows.Forms.Form
	{
		private System.Windows.Forms.Panel panel1;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		private ArrayList drawnSavObjList = new ArrayList();
		private ArrayList drawnObjList = new ArrayList();
		private Color drawColor = Color.Black;
		private Graphics gr =null;
		private ImageType imageType;
		private Point ptStart;
		public event mouseMoveHandler PanelMouseMove;
		private Image imgSav;
		private Graphics imgGr=null;

		private frmMain m_parent;

    public frmCanvas(frmMain parent)
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			gr = Graphics.FromHwnd(panel1.Handle);
			imageType = ImageType.Line;
			imgSav=new Bitmap(this.Width,this.Height);
			imgGr= Graphics.FromImage(imgSav);
			imgGr.Clear(Color.White);

      m_parent = parent;
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
				gr.Dispose();
				imgGr.Dispose();
				imgSav.Dispose();
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
      this.panel1 = new System.Windows.Forms.Panel();
      this.SuspendLayout();
      // 
      // panel1
      // 
      this.panel1.BackColor = System.Drawing.Color.White;
      this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
      this.panel1.Location = new System.Drawing.Point(0, 0);
      this.panel1.Name = "panel1";
      this.panel1.Size = new System.Drawing.Size(480, 397);
      this.panel1.TabIndex = 1;
      this.panel1.MouseUp += new System.Windows.Forms.MouseEventHandler(this.panel1_MouseUp);
      this.panel1.Paint += new System.Windows.Forms.PaintEventHandler(this.panel1_Paint);
      this.panel1.MouseMove += new System.Windows.Forms.MouseEventHandler(this.panel1_MouseMove);
      this.panel1.MouseDown += new System.Windows.Forms.MouseEventHandler(this.panel1_MouseDown);
      // 
      // frmCanvas
      // 
      this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
      this.ClientSize = new System.Drawing.Size(480, 397);
      this.Controls.Add(this.panel1);
      this.Name = "frmCanvas";
      this.Text = "frmCanvas";
      this.Closing += new System.ComponentModel.CancelEventHandler(this.frmCanvas_Closing);
      this.Move += new System.EventHandler(this.frmCanvas_Move);
      this.Closed += new System.EventHandler(this.frmCanvas_Closed);
      this.Activated += new System.EventHandler(this.frmCanvas_Activated);
      this.ResumeLayout(false);

    }
		#endregion

		protected override void OnPaint(PaintEventArgs e)
		{
			//this.panel1.Invalidate();
			base.OnPaint (e);
			
						
			
		}
		
		public void DrawRectangle(Rectangle rect,bool filled)
		{
			DrawRectangle rect1 = new DrawRectangle(new Point(rect.X,rect.Y),new Point(rect.X + rect.Width,rect.Y + rect.Height),drawColor,gr,filled);
			drawnObjList.Add(rect1);
			rect1.Draw();
			rect1 = new DrawRectangle(new Point(rect.X,rect.Y),new Point(rect.X + rect.Width,rect.Y + rect.Height),drawColor,imgGr,filled);
			drawnSavObjList.Add(rect1);
		}

		public void DrawEllipse(Rectangle rect,bool filled)
		{
			DrawEllipse ellipse = new DrawEllipse(new Point(rect.X,rect.Y),new Point(rect.X + rect.Width,rect.Y + rect.Height),drawColor,gr,filled);
			drawnObjList.Add(ellipse);
			ellipse.Draw();
			ellipse = new DrawEllipse(new Point(rect.X,rect.Y),new Point(rect.X + rect.Width,rect.Y + rect.Height),drawColor,imgGr,filled);
			drawnSavObjList.Add(ellipse);
		}

		public void DrawLine(Point start, Point end)
		{
			DrawLine line = new DrawLine(start,end,drawColor,gr);
			drawnObjList.Add(line);
			line.Draw();
			line = new DrawLine(start,end,drawColor,imgGr);
			drawnSavObjList.Add(line);
		}
		private void panel1_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
		{
			ptStart= new Point(e.X,e.Y);
		}

		private void panel1_MouseUp(object sender, System.Windows.Forms.MouseEventArgs e)
		{
			if(imageType==ImageType.Circle || imageType==ImageType.CircleFilled)
			{
				DrawEllipse ellipse = new DrawEllipse(ptStart,new Point(e.X,e.Y),drawColor,gr,imageType==ImageType.CircleFilled);
				drawnObjList.Add(ellipse);				
				ellipse.Draw();
				ellipse = new DrawEllipse(ptStart,new Point(e.X,e.Y),drawColor,imgGr,imageType==ImageType.CircleFilled);
				drawnSavObjList.Add(ellipse);
				//ellipse.Draw();
			}
			else if(imageType==ImageType.Rectangle || imageType==ImageType.RectangleFilled)
			{
				DrawRectangle rect = new DrawRectangle(ptStart,new Point(e.X,e.Y),drawColor,gr,imageType==ImageType.RectangleFilled);
				drawnObjList.Add(rect);
				rect.Draw();
				rect = new DrawRectangle(ptStart,new Point(e.X,e.Y),drawColor,imgGr,imageType==ImageType.RectangleFilled);
				drawnSavObjList.Add(rect);
				//rect.Draw();
			}
			else if(imageType==ImageType.Line)
			{
				DrawLine line = new DrawLine(ptStart,new Point(e.X,e.Y),drawColor,gr);
				drawnObjList.Add(line);
				line.Draw();
				line = new DrawLine(ptStart,new Point(e.X,e.Y),drawColor,imgGr);
				drawnSavObjList.Add(line);
				//line.Draw();
			}
		}

		private void panel1_MouseMove(object sender, System.Windows.Forms.MouseEventArgs e)
		{
			if(PanelMouseMove!=null)
				PanelMouseMove(this,e);
		}

		public ImageType DrawingType
		{
			get{return imageType;}
			set{this.imageType=value;}
		}

		public void UndoBefore()
		{
			if(this.drawnObjList.Count>0)
			{
				//this.panel1.Invalidate();
				this.panel1.Visible=false;
				this.drawnObjList.RemoveAt(this.drawnObjList.Count - 1);
				this.drawnSavObjList.RemoveAt(this.drawnSavObjList.Count - 1);
				this.panel1.Visible=true;
				
			}
		}

    public void ClearCanvas()
    {
      this.drawnObjList.Clear();
      this.drawnSavObjList.Clear();
      this.Refresh();
    }

		private void frmCanvas_Move(object sender, System.EventArgs e)
		{
//			foreach(object obj in drawnObjList)
//				((DrawObject)obj).Draw();
		}

		private void frmCanvas_Closing(object sender, System.ComponentModel.CancelEventArgs e)
		{
			
		}

		public void SaveCanvas(string filePath, System.Drawing.Imaging.ImageFormat format)
		{
//			Image img = new Bitmap(100,40);
//			Graphics grphObj = Graphics.FromImage(img); 
//			grphObj.FillRectangle(new System.Drawing.Drawing2D.LinearGradientBrush(new Point(0,0),new Point(100,40),Color.LightBlue,Color.LightCoral),0,0,100,40);
//			//t.DrawLine(new Pen(Brushes.Blue,2),0,40,100,0);
//			grphObj.DrawString("test jimmy",new Font("Times New Roman",18),Brushes.Red,5,5);	
//			img.Save(filePath,format);
//			return;
			imgGr.Clear(Color.White);
			foreach(object obj in drawnSavObjList)
				((DrawObject)obj).Draw();
			imgSav.Save(filePath,format);
		}

		private void panel1_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
		{
			foreach(object obj in drawnObjList)
				((DrawObject)obj).Draw();
		}

    private void frmCanvas_Activated(object sender, System.EventArgs e)
    {
      m_parent.m_active = this;
    }

    private void frmCanvas_Closed(object sender, System.EventArgs e)
    {
      m_parent.m_active = null;
    }

		public Color PanelForeColor
		{
			get{return drawColor;}
			set{this.drawColor=value;}
		}
	}
}
