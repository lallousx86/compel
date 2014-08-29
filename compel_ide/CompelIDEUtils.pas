unit CompelIDEUtils;

interface

uses CompelWorkspace, Forms, Windows;

procedure FormToWksDimension(
  Form: TForm;
  var ADim: TCompelWorkspaceDimensions;
  const DimToForm: Boolean = False);

procedure DisableWindowsAnimation(Disable: Boolean);

implementation

procedure DisableWindowsAnimation(Disable: Boolean);
var
  ani: ANIMATIONINFO;
begin
  ani.cbSize := sizeof(ANIMATIONINFO);
  ani.iMinAnimate := 0; // non-zero to enable animations
  SystemParametersInfo(SPI_SETANIMATION,  0, @ani, SPIF_SENDWININICHANGE);
end;

procedure FormToWksDimension(Form: TForm;var ADim: TCompelWorkspaceDimensions;const DimToForm: Boolean);
begin
  if DimToForm then
  begin
    if TWindowState(ADim.WindowState) = wsNormal then
    begin
      Form.WindowState := wsNormal;
      Form.Width := ADim.Width;
      Form.Height := ADim.Height;
      Form.Top := ADim.Top;
      Form.Left := ADim.Left;
    end else
    begin
      Form.WindowState := TWindowState(ADim.WindowState);
    end;
  end
  else begin
    ADim.Width := Form.Width;
    ADim.Height := Form.Height;
    ADim.Top := Form.Top;
    ADim.Left := Form.Left;
    ADim.WindowState := Integer(Form.WindowState);
  end;
end;

end.
