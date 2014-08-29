using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using CompelManaged;
namespace CompelPaint
{
    /// <summary>
    /// Summary description for frmMain.
    /// </summary>
    public class frmMain : System.Windows.Forms.Form
    {
        private System.Windows.Forms.StatusBar statusBar1;
        private System.Windows.Forms.StatusBarPanel statusBarPanel1;
        private System.Windows.Forms.ColorDialog colorDialog1;
        private System.Windows.Forms.MainMenu mainMenu1;
        private System.Windows.Forms.MenuItem menuItem1;
        private System.Windows.Forms.MenuItem menuItem3;
        private System.Windows.Forms.MenuItem mnuExit;
        private System.Windows.Forms.MenuItem menuItem2;
        private System.Windows.Forms.MenuItem mnuUndo;
        private System.Windows.Forms.MenuItem menuItem5;
        private System.Windows.Forms.MenuItem mnuItemNew;
        private System.Windows.Forms.MenuItem menuItem4;
        private System.Windows.Forms.MenuItem mnuItemLoadScript;
        private System.Windows.Forms.MenuItem mnuItemSave;
        private System.Windows.Forms.SaveFileDialog SaveDialog;
        private System.Windows.Forms.OpenFileDialog openDlg;
        private System.ComponentModel.IContainer components;
        private System.Windows.Forms.MenuItem menuItem6;
        private System.Windows.Forms.MenuItem mnuItemConsole;
        private System.Windows.Forms.MenuItem menuItem9;
        private System.Windows.Forms.MenuItem mnuItemRun;
        private System.Windows.Forms.MenuItem menuItem7;
        private System.Windows.Forms.ToolBar toolBar1;
        private System.Windows.Forms.ToolBarButton toolBarButton1;
        private System.Windows.Forms.ToolBarButton toolBarButton2;
        private System.Windows.Forms.ToolBarButton toolBarButton3;
        private System.Windows.Forms.ToolBarButton toolBarButton4;
        private System.Windows.Forms.ToolBarButton toolBarButton5;
        private System.Windows.Forms.ToolBarButton toolBarButton6;
        private frmConsole frmsCnsl = new frmConsole();
        private System.Windows.Forms.ImageList imageList1;
        private System.Windows.Forms.MenuItem menuItem8;

        private System.Resources.ResourceManager manager;

        public frmMain()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            frmsCnsl.Dock = DockStyle.Bottom;

            manager = new System.Resources.ResourceManager("CompelPaint.toolbar", System.Reflection.Assembly.GetExecutingAssembly());

            string[] arr = new string[] { "circle", "circlefill", "rect", "rectfill", "line", "color" };
            for (int i = 0; i < arr.Length; i++)
            {
                imageList1.Images.Add((Bitmap)manager.GetObject(arr[i]));
            }
            toolBar1.ImageList = imageList1;
            //toolBarButton4.
        }

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (components != null)
                {
                    components.Dispose();
                }
                for (int i = 0; i < frmCnvsList.Count; i++)
                {
                    ((frmCanvas)frmCnvsList[i]).Dispose();
                }
                frmsCnsl.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code
        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.statusBar1 = new System.Windows.Forms.StatusBar();
            this.statusBarPanel1 = new System.Windows.Forms.StatusBarPanel();
            this.colorDialog1 = new System.Windows.Forms.ColorDialog();
            this.mainMenu1 = new System.Windows.Forms.MainMenu();
            this.menuItem1 = new System.Windows.Forms.MenuItem();
            this.mnuItemNew = new System.Windows.Forms.MenuItem();
            this.mnuItemSave = new System.Windows.Forms.MenuItem();
            this.menuItem4 = new System.Windows.Forms.MenuItem();
            this.mnuItemLoadScript = new System.Windows.Forms.MenuItem();
            this.menuItem3 = new System.Windows.Forms.MenuItem();
            this.mnuExit = new System.Windows.Forms.MenuItem();
            this.menuItem2 = new System.Windows.Forms.MenuItem();
            this.mnuItemRun = new System.Windows.Forms.MenuItem();
            this.menuItem6 = new System.Windows.Forms.MenuItem();
            this.mnuUndo = new System.Windows.Forms.MenuItem();
            this.menuItem9 = new System.Windows.Forms.MenuItem();
            this.mnuItemConsole = new System.Windows.Forms.MenuItem();
            this.menuItem5 = new System.Windows.Forms.MenuItem();
            this.menuItem7 = new System.Windows.Forms.MenuItem();
            this.SaveDialog = new System.Windows.Forms.SaveFileDialog();
            this.openDlg = new System.Windows.Forms.OpenFileDialog();
            this.toolBar1 = new System.Windows.Forms.ToolBar();
            this.toolBarButton1 = new System.Windows.Forms.ToolBarButton();
            this.toolBarButton2 = new System.Windows.Forms.ToolBarButton();
            this.toolBarButton3 = new System.Windows.Forms.ToolBarButton();
            this.toolBarButton4 = new System.Windows.Forms.ToolBarButton();
            this.toolBarButton5 = new System.Windows.Forms.ToolBarButton();
            this.toolBarButton6 = new System.Windows.Forms.ToolBarButton();
            this.imageList1 = new System.Windows.Forms.ImageList(this.components);
            this.menuItem8 = new System.Windows.Forms.MenuItem();
            ((System.ComponentModel.ISupportInitialize)(this.statusBarPanel1)).BeginInit();
            this.SuspendLayout();
            // 
            // statusBar1
            // 
            this.statusBar1.Location = new System.Drawing.Point(0, 272);
            this.statusBar1.Name = "statusBar1";
            this.statusBar1.Panels.AddRange(new System.Windows.Forms.StatusBarPanel[] {
                                                                                  this.statusBarPanel1});
            this.statusBar1.Size = new System.Drawing.Size(536, 22);
            this.statusBar1.TabIndex = 5;
            this.statusBar1.Text = "statusBar1";
            // 
            // statusBarPanel1
            // 
            this.statusBarPanel1.Text = "statusBarPanel1";
            // 
            // mainMenu1
            // 
            this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                                                                              this.menuItem1,
                                                                              this.menuItem2,
                                                                              this.menuItem9,
                                                                              this.menuItem5});
            // 
            // menuItem1
            // 
            this.menuItem1.Index = 0;
            this.menuItem1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                                                                              this.mnuItemNew,
                                                                              this.mnuItemSave,
                                                                              this.menuItem4,
                                                                              this.mnuItemLoadScript,
                                                                              this.menuItem3,
                                                                              this.mnuExit});
            this.menuItem1.Text = "File";
            // 
            // mnuItemNew
            // 
            this.mnuItemNew.Index = 0;
            this.mnuItemNew.Shortcut = System.Windows.Forms.Shortcut.CtrlN;
            this.mnuItemNew.Text = "&New";
            this.mnuItemNew.Click += new System.EventHandler(this.mnuItemNew_Click);
            // 
            // mnuItemSave
            // 
            this.mnuItemSave.Index = 1;
            this.mnuItemSave.Shortcut = System.Windows.Forms.Shortcut.CtrlS;
            this.mnuItemSave.Text = "&Save As";
            this.mnuItemSave.Click += new System.EventHandler(this.mnuItemSave_Click);
            // 
            // menuItem4
            // 
            this.menuItem4.Index = 2;
            this.menuItem4.Text = "-";
            // 
            // mnuItemLoadScript
            // 
            this.mnuItemLoadScript.Index = 3;
            this.mnuItemLoadScript.Shortcut = System.Windows.Forms.Shortcut.CtrlL;
            this.mnuItemLoadScript.Text = "&Load Script";
            this.mnuItemLoadScript.Click += new System.EventHandler(this.mnuItemLoadScript_Click);
            // 
            // menuItem3
            // 
            this.menuItem3.Index = 4;
            this.menuItem3.Text = "-";
            // 
            // mnuExit
            // 
            this.mnuExit.Index = 5;
            this.mnuExit.Text = "E&xit";
            this.mnuExit.Click += new System.EventHandler(this.mnuExit_Click);
            // 
            // menuItem2
            // 
            this.menuItem2.Index = 1;
            this.menuItem2.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                                                                              this.mnuItemRun,
                                                                              this.menuItem6,
                                                                              this.mnuUndo});
            this.menuItem2.Text = "Edit";
            this.menuItem2.Popup += new System.EventHandler(this.menuItem2_Popup);
            // 
            // mnuItemRun
            // 
            this.mnuItemRun.Index = 0;
            this.mnuItemRun.Shortcut = System.Windows.Forms.Shortcut.F5;
            this.mnuItemRun.Text = "&Run";
            this.mnuItemRun.Click += new System.EventHandler(this.mnuItemRun_Click);
            // 
            // menuItem6
            // 
            this.menuItem6.Index = 1;
            this.menuItem6.Text = "-";
            // 
            // mnuUndo
            // 
            this.mnuUndo.Index = 2;
            this.mnuUndo.Shortcut = System.Windows.Forms.Shortcut.CtrlZ;
            this.mnuUndo.Text = "&Undo";
            this.mnuUndo.Click += new System.EventHandler(this.mnuUndo_Click);
            // 
            // menuItem9
            // 
            this.menuItem9.Index = 2;
            this.menuItem9.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                                                                              this.mnuItemConsole});
            this.menuItem9.Text = "View";
            this.menuItem9.Popup += new System.EventHandler(this.menuItem9_Popup);
            // 
            // mnuItemConsole
            // 
            this.mnuItemConsole.Index = 0;
            this.mnuItemConsole.Text = "&Command Console";
            this.mnuItemConsole.Click += new System.EventHandler(this.mnuItemConsole_Click);
            // 
            // menuItem5
            // 
            this.menuItem5.Index = 3;
            this.menuItem5.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                                                                              this.menuItem7,
                                                                              this.menuItem8});
            this.menuItem5.Text = "Help";
            this.menuItem5.Click += new System.EventHandler(this.menuItem5_Click);
            // 
            // menuItem7
            // 
            this.menuItem7.Index = 0;
            this.menuItem7.Text = "About";
            this.menuItem7.Click += new System.EventHandler(this.menuItem7_Click);
            // 
            // SaveDialog
            // 
            this.SaveDialog.FileName = "doc1";
            // 
            // toolBar1
            // 
            this.toolBar1.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
                                                                                this.toolBarButton1,
                                                                                this.toolBarButton2,
                                                                                this.toolBarButton3,
                                                                                this.toolBarButton4,
                                                                                this.toolBarButton5,
                                                                                this.toolBarButton6});
            this.toolBar1.DropDownArrows = true;
            this.toolBar1.Location = new System.Drawing.Point(0, 0);
            this.toolBar1.Name = "toolBar1";
            this.toolBar1.ShowToolTips = true;
            this.toolBar1.Size = new System.Drawing.Size(536, 28);
            this.toolBar1.TabIndex = 6;
            this.toolBar1.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.toolBar1_ButtonClick);
            // 
            // toolBarButton1
            // 
            this.toolBarButton1.ImageIndex = 0;
            this.toolBarButton1.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
            // 
            // toolBarButton2
            // 
            this.toolBarButton2.ImageIndex = 1;
            this.toolBarButton2.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
            // 
            // toolBarButton3
            // 
            this.toolBarButton3.ImageIndex = 2;
            this.toolBarButton3.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
            // 
            // toolBarButton4
            // 
            this.toolBarButton4.ImageIndex = 3;
            this.toolBarButton4.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
            // 
            // toolBarButton5
            // 
            this.toolBarButton5.ImageIndex = 4;
            // 
            // toolBarButton6
            // 
            this.toolBarButton6.ImageIndex = 5;
            // 
            // imageList1
            // 
            this.imageList1.ImageSize = new System.Drawing.Size(16, 16);
            this.imageList1.TransparentColor = System.Drawing.Color.Transparent;
            // 
            // menuItem8
            // 
            this.menuItem8.Index = 1;
            this.menuItem8.Text = "Command &reference";
            this.menuItem8.Click += new System.EventHandler(this.menuItem8_Click);
            // 
            // frmMain
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(536, 294);
            this.Controls.Add(this.toolBar1);
            this.Controls.Add(this.statusBar1);
            this.IsMdiContainer = true;
            this.Menu = this.mainMenu1;
            this.Name = "frmMain";
            this.Text = "Simple paint";
            this.Load += new System.EventHandler(this.frmMain_Load);
            ((System.ComponentModel.ISupportInitialize)(this.statusBarPanel1)).EndInit();
            this.ResumeLayout(false);

        }
        #endregion

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            //Application.EnableVisualStyles();
            Application.Run(new frmMain());
        }

        private void mnuExit_Click(object sender, System.EventArgs e)
        {
            this.Close();
        }

        ArrayList frmCnvsList = new ArrayList();
        private void mnuItemNew_Click(object sender, System.EventArgs e)
        {
            frmCnvsList.Add(new frmCanvas(this));
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).PanelMouseMove += new mouseMoveHandler(frmMain_PanelMouseMove);
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).Activated += new EventHandler(frmMain_Activated);
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).Closed += new EventHandler(frmMain_Closed);
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).MdiParent = this;
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).Text = "New Canvas";
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).Show();

        }

        private void frmMain_PanelMouseMove(object sender, MouseEventArgs e)
        {
            this.statusBar1.Text = "X= " + e.X.ToString() + " , Y= " + e.Y.ToString();
        }

        private void toolBar1_ButtonClick(object sender, System.Windows.Forms.ToolBarButtonClickEventArgs e)
        {
            if (e.Button != toolBar1.Buttons[5])
            {
                for (int i = 0; i < 5; i++)
                {
                    if (toolBar1.Buttons[i] != e.Button)
                    {
                        toolBar1.Buttons[i].Pushed = false;
                    }
                    else
                    {
                        for (int j = 0; j < frmCnvsList.Count; j++)
                        {
                            if (((frmCanvas)frmCnvsList[j]).Focused)
                            {
                                ((frmCanvas)frmCnvsList[j]).DrawingType = (ImageType)i;
                                break;
                            }
                        }
                    }
                }
            }
            else
            {
                if (colorDialog1.ShowDialog(this) == DialogResult.OK)
                {
                    for (int i = 0; i < frmCnvsList.Count; i++)
                    {
                        if (((frmCanvas)frmCnvsList[i]).Focused)
                        {
                            ((frmCanvas)frmCnvsList[i]).PanelForeColor = colorDialog1.Color;
                            break;
                        }
                    }
                }
            }
        }

        private void mnuUndo_Click(object sender, System.EventArgs e)
        {
            for (int j = 0; j < frmCnvsList.Count; j++)
            {
                if (((frmCanvas)frmCnvsList[j]).Focused)
                {
                    ((frmCanvas)frmCnvsList[j]).UndoBefore();
                    break;
                }
            }
        }

        private void frmMain_Activated(object sender, EventArgs e)
        {
            for (int i = 0; i < 5; i++)
            {
                if (toolBar1.Buttons[i].Pushed)
                {
                    for (int j = 0; j < frmCnvsList.Count; j++)
                    {
                        if (((frmCanvas)frmCnvsList[j]).Focused)
                        {
                            ((frmCanvas)frmCnvsList[j]).DrawingType = (ImageType)i;
                            break;
                        }
                    }
                }
            }
        }

        private void frmMain_Closed(object sender, EventArgs e)
        {
            ((frmCanvas)sender).Dispose();
            frmCnvsList.Remove(sender);
        }

        private CompelScript MakeScript()
        {
            CompelScript script = new CompelScript();

            script.CreateUserCommand("CvCreate", 0, 0, new UserCommandCB(this.CanvasCreate));//returns handle
            script.CreateUserCommand("CvTitle", 1, 1, new UserCommandCB(this.CanvasTitle));
            script.CreateUserCommand("CvClear", 0, 0, new UserCommandCB(this.ClearCanvas));
            script.CreateUserCommand("CvSelect", 1, 1, new UserCommandCB(this.SelectCanvas));
            script.CreateUserCommand("CvLine", 4, 4, new UserCommandCB(this.DrawLine));
            script.CreateUserCommand("CvRect", 4, 4, new UserCommandCB(this.DrawRect));
            script.CreateUserCommand("CvEllipse", 5, 5, new UserCommandCB(this.DrawEllipse));
            script.CreateUserCommand("CvSave", 1, 1, new UserCommandCB(this.SaveCanvas));
            script.CreateUserCommand("CvClose", 0, 0, new UserCommandCB(this.CloseCanvas));
            script.CreateUserCommand("CvColor", 3, 3, new UserCommandCB(this.SelectFgColor));
            return script;
        }
        private void mnuItemLoadScript_Click(object sender, System.EventArgs e)
        {
            this.openDlg.Filter = "Text Files(*.txt)|*.txt";
            if (this.openDlg.ShowDialog(this) != DialogResult.OK)
                return;

            CompelScript script = MakeScript();

            script.LoadFile(this.openDlg.FileName);
            script.Run();
            script.Dispose();
        }

        private void mnuItemSave_Click(object sender, System.EventArgs e)
        {
            if (frmCnvsList.Count == 0)
                return;
            this.SaveDialog.Filter = "Image Files(*.BMP;*.JPG;*.GIF)|*.BMP;*.JPG;*.GIF|Bitmap(*.BMP)|*.BMP|JPEG(*.JPG)|*.JPG|GIF(*.GIF)|*.GIF|All files (*.*)|*.*";
            if (this.SaveDialog.ShowDialog(this) != DialogResult.OK)
                return;
            if (SaveDialog.FileName != null && SaveDialog.FileName != "" && (SaveDialog.FileName.Substring(SaveDialog.FileName.Length - 3, 3).ToUpper() == "BMP" || SaveDialog.FileName.Substring(SaveDialog.FileName.Length - 3, 3).ToUpper() == "JPG" || SaveDialog.FileName.Substring(SaveDialog.FileName.Length - 3, 3).ToUpper() == "GIF"))
            {
                for (int j = 0; j < frmCnvsList.Count; j++)
                {
                    if (((frmCanvas)frmCnvsList[j]).Focused)
                    {
                        if (SaveDialog.FileName.Substring(SaveDialog.FileName.Length - 3, 3) == "BMP")
                            ((frmCanvas)frmCnvsList[j]).SaveCanvas(SaveDialog.FileName, System.Drawing.Imaging.ImageFormat.Bmp);
                        else if (SaveDialog.FileName.Substring(SaveDialog.FileName.Length - 3, 3) == "JPG")
                            ((frmCanvas)frmCnvsList[j]).SaveCanvas(SaveDialog.FileName, System.Drawing.Imaging.ImageFormat.Jpeg);
                        else
                            ((frmCanvas)frmCnvsList[j]).SaveCanvas(SaveDialog.FileName, System.Drawing.Imaging.ImageFormat.Gif);
                        break;
                    }
                }
            }
            else
            {
                MessageBox.Show("File Name is invalid", "Image Processing");
                //e.Cancel=true;
            }
        }

        public frmCanvas m_active = null;

        private void CanvasCreate(CompelScript scriptObj, string[] argVal)
        {
            frmCnvsList.Add(new frmCanvas(this));
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).PanelMouseMove += new mouseMoveHandler(frmMain_PanelMouseMove);
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).Activated += new EventHandler(frmMain_Activated);
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).Closed += new EventHandler(frmMain_Closed);
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).MdiParent = this;
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).Text = "New Canvas";
            ((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).Show();
            scriptObj.SetReturnValue(((frmCanvas)frmCnvsList[frmCnvsList.Count - 1]).Handle.ToString());
        }

        private void CanvasTitle(CompelScript scriptObj, string[] argVal)
        {
            if (m_active == null)
                return;

            m_active.Text = argVal[0];
        }

        private void ClearCanvas(CompelScript scriptObj, string[] argVal)
        {
            if (m_active == null)
                return;

            m_active.ClearCanvas();
        }

        private void SelectCanvas(CompelScript scriptObj, string[] argVal)
        {
            for (int j = 0; j < frmCnvsList.Count; j++)
            {
                frmCanvas c = ((frmCanvas)frmCnvsList[j]);
                if (c.Text == argVal[0])
                {
                    c.Select();
                    break;
                }
            }
        }

        private void DrawLine(CompelScript scriptObj, string[] argVal)
        {
            if (m_active == null)
                return;

            m_active.DrawLine(new Point(int.Parse(argVal[0]), int.Parse(argVal[1])), new Point(int.Parse(argVal[2]), int.Parse(argVal[3])));
        }

        private void DrawRect(CompelScript scriptObj, string[] argVal)
        {
            if (m_active == null)
                return;
            m_active.DrawRectangle(
        new Rectangle(
        new Point(int.Parse(argVal[0]), int.Parse(argVal[1])),
        new Size(int.Parse(argVal[2]), int.Parse(argVal[3]))),
        true);
        }

        private void DrawEllipse(CompelScript scriptObj, string[] argVal)
        {
            if (m_active == null)
                return;
            m_active.DrawEllipse(new Rectangle(new Point(int.Parse(argVal[0]), int.Parse(argVal[1])), new Size(int.Parse(argVal[2]), int.Parse(argVal[3]))), Convert.ToBoolean(int.Parse(argVal[4])));
        }

        private void SaveCanvas(CompelScript scriptObj, string[] argVal)
        {
            if (m_active == null)
                return;
            if (argVal[0].Substring(argVal[0].Length - 3, 3).ToUpper() == "BMP")
                m_active.SaveCanvas(argVal[0], System.Drawing.Imaging.ImageFormat.Bmp);
            else if (argVal[0].Substring(argVal[0].Length - 3, 3).ToUpper() == "JPG")
                m_active.SaveCanvas(argVal[0], System.Drawing.Imaging.ImageFormat.Jpeg);
            else
                m_active.SaveCanvas(argVal[0], System.Drawing.Imaging.ImageFormat.Gif);
        }

        private void CloseCanvas(CompelScript scriptObj, string[] argVal)
        {
            if (m_active == null)
                return;
            m_active.Close();
            m_active = null;
        }

        private void SelectFgColor(CompelScript scriptObj, string[] argVal)
        {
            if (m_active == null)
                return;
            m_active.PanelForeColor = Color.FromArgb(int.Parse(argVal[0]), int.Parse(argVal[1]), int.Parse(argVal[2]));
        }

        private void frmMain_Load(object sender, System.EventArgs e)
        {
            frmsCnsl.MdiParent = this;
            frmsCnsl.Dock = DockStyle.Bottom;
            frmsCnsl.Show();
            frmsCnsl.Dock = DockStyle.Bottom;
            this.WindowState = FormWindowState.Maximized;
            this.statusBar1.Text = "Compel Paint";
        }

        private void mnuItemConsole_Click(object sender, System.EventArgs e)
        {
            if (frmsCnsl.Visible)
                frmsCnsl.Hide();
            else
            {
                frmsCnsl.Dock = DockStyle.Bottom;
                frmsCnsl.Show();
            }
        }

        private void menuItem9_Popup(object sender, System.EventArgs e)
        {
            if (frmsCnsl.Visible)
                this.mnuItemConsole.Checked = true;
            else
                this.mnuItemConsole.Checked = false;
        }

        private void menuItem2_Popup(object sender, System.EventArgs e)
        {
            if (frmsCnsl.Visible && frmsCnsl.txtScript.Text != "")
                mnuItemRun.Enabled = true;
            else
                mnuItemRun.Enabled = false;

        }

        private void mnuItemRun_Click(object sender, System.EventArgs e)
        {
            CompelScript script = MakeScript();

            //			script.LoadLines(frmsCnsl.txtScript.Text.Replace("\r\n",""), ";");
            script.LoadLines(frmsCnsl.txtScript.Text, "\r\n");

            script.Run();
            script.Dispose();
        }

        private void menuItem5_Click(object sender, System.EventArgs e)
        {

        }

        private void menuItem7_Click(object sender, System.EventArgs e)
        {
            frmAbout abt = new frmAbout();
            abt.ShowDialog(this);
            abt.Dispose();
        }

        private void menuItem8_Click(object sender, System.EventArgs e)
        {
            MessageBox.Show(
              "Create - creates a new canvas\n" +
              "CvTitle _title_ - sets a new title\n" +
              "CvClear - clears the canvas\n" +
              "CvSelect _titles_ - Switchs to a canvas by title\n" +
              "CvLine _x1_ _y1_ _x2_ _y2_ - Draws a line\n" +
              "CvRect _x1_ _y1_ _x2_ _y2_ - Draws a rectangle\n" +
              "CvEllipse _x1_ _y1_ _x2_ _y2_ - Draws an ellipse\n" +
              "CvColor R G B - Selects the color\n" +
              "CvClose - Closes a canvas\n" +
              "CvSave _filename_ - Saves canvas to an image file\n",
              "Command Reference");
        }
    }
}
