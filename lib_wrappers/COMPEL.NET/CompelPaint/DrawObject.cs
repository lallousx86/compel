using System;
using System.Drawing;
namespace CompelPaint
{
	public enum ImageType : int
	{
		Circle=0, CircleFilled=1, Rectangle=2, RectangleFilled=3, Line=4
	}
	/// <summary>
	/// Summary description for DrawObject.
	/// </summary>
	public abstract class DrawObject
	{
		protected Point ptStart = new Point(0,0);
		protected Point ptEnd = new Point(0,0);
		protected Color color=Color.Black;
		protected Graphics grphcObj=null;
		public DrawObject(Point ptStart ,Point ptEnd, Color color,Graphics grphcObj)
		{
			this.ptEnd= ptEnd;
			this.ptStart=ptStart;
			this.color=color;
			this.grphcObj=grphcObj;			
		}

		public abstract void Draw();
	}

	public class DrawEllipse: DrawObject
	{
		private bool fillRect;
		public DrawEllipse(Point ptStart ,Point ptEnd, Color color,Graphics grphcObj,bool fill) : base(ptStart,ptEnd,color,grphcObj)
		{
			this.fillRect=fill;
		}

		public override void Draw()
		{
			Rectangle ptRect = new Rectangle(0,0,0,0);
			ptRect.Width = (ptEnd.X-ptStart.X);
			
			if(ptRect.Width<0)
				ptRect.Width*=-1;
			if(ptEnd.X<ptStart.X)
				ptRect.X=ptEnd.X;
			else
				ptRect.X=ptStart.X;
			
			ptRect.Height = (ptEnd.Y-ptStart.Y);
			if(ptRect.Height<0)
				ptRect.Height*=-1;
			if(ptEnd.Y<ptStart.Y)
				ptRect.Y=ptEnd.Y;
			else
				ptRect.Y=ptStart.Y;
			
				
			grphcObj.DrawEllipse(new Pen(this.color),ptRect);
			if(fillRect)
				grphcObj.FillEllipse(new SolidBrush(this.color),ptRect.X+1,ptRect.Y+1,ptRect.Width-1,ptRect.Height-1);
		}
 
	}

	public class DrawLine: DrawObject
	{
		public DrawLine(Point ptStart ,Point ptEnd, Color color,Graphics grphcObj) : base(ptStart,ptEnd,color,grphcObj)
		{
		}

		public override void Draw()
		{
			grphcObj.DrawLine(new Pen(this.color),ptStart,ptEnd);
		}
 
	}

	public class DrawRectangle : DrawObject
	{
		private bool fillRect;
		public DrawRectangle(Point ptStart ,Point ptEnd, Color color,Graphics grphcObj,bool fill) : base(ptStart,ptEnd,color,grphcObj)
		{
			this.fillRect=fill;
		}

		public override void Draw()
		{
			Rectangle ptRect = new Rectangle(0,0,0,0);
			ptRect.Width = (ptEnd.X-ptStart.X);
			
			if(ptRect.Width<0)
				ptRect.Width*=-1;
			if(ptEnd.X<ptStart.X)
				ptRect.X=ptEnd.X;
			else
				ptRect.X=ptStart.X;
			
			ptRect.Height = (ptEnd.Y-ptStart.Y);
			if(ptRect.Height<0)
				ptRect.Height*=-1;
			if(ptEnd.Y<ptStart.Y)
				ptRect.Y=ptEnd.Y;
			else
				ptRect.Y=ptStart.Y;
			
				
			grphcObj.DrawRectangle(new Pen(this.color),ptRect);
			if(fillRect)
				grphcObj.FillRectangle(new SolidBrush(this.color),ptRect.X+1,ptRect.Y+1,ptRect.Width-1,ptRect.Height-1);
		}
 
	}
}
