unit uCanvasExtShapes;

interface

uses Classes, Graphics, Windows, Messages, Math;

type

  TCanvasExtShape = class
  private
    FAttributes: Cardinal;
    FBrushColor: TColor;
    FBrushStyle: TBrushStyle;
    FPenColor: TColor;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Draw(ACanvas: TCanvas);virtual;abstract;
    procedure Serialize(AMs: TStream);virtual;
    procedure DeSerialize(AMs: TStream);virtual;

    procedure SetCanvasBasicParams(ACanvas: TCanvas);
  published
    property Attributes: Cardinal read FAttributes write FAttributes;
    property BrushColor: TColor read FBrushColor write FBrushColor;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property PenColor: TColor read FPenColor write FPenColor;
  end;

  TCanvasExtShape2D = class(TCanvasExtShape)
  private
  public
  end;

  TCanvasExt2DText = class(TCanvasExtShape2D)
  private
    FFont: TFont;
    FMsg: string;
    X, Y: Integer;

    procedure SetFont(Font: TFont);
  public
    constructor Create(const AX, AY: Integer;const AText: string);overload;
    constructor Create;overload;
    procedure Draw(ACanvas: TCanvas);override;
    destructor Destroy;override;
    procedure Serialize(AMs: TStream);override;
    procedure DeSerialize(AMs: TStream);override;
  published
    property Font: TFont read FFont write SetFont;
  end;

  TCanvasExt2DImageLoader = class(TCanvasExtShape2D)
  private
    FRectSrc: TRect;
    FRectDest: TRect;
    FBitmap: Graphics.TBitmap;
    FFileName: string;

    X1, Y1, X2, Y2: Integer;

    function DoConstruct: Boolean;
  public
    Width, Height: Integer;
    constructor Create(const AX1, AY1, AX2, AY2: Integer; const AFileName: string);overload;
    constructor Create;overload;
    procedure Draw(ACanvas: TCanvas);override;
    procedure Serialize(AMs: TStream);override;
    procedure DeSerialize(AMs: TStream);override;
    destructor Destroy;override;
  end;

  TCanvasExtShape4PointsType = (fptRect, fptEllipse, fptLine);
  TCanvasExtShape4Points = class(TCanvasExtShape2D)
  private
    X1, Y1, X2, Y2: Integer;
    FType: TCanvasExtShape4PointsType;
    FFilled: Boolean;
  public
    procedure Draw(ACanvas: TCanvas);override;
    constructor Create(
       const AX1, AY1, AX2, AY2: Integer;
       AType: TCanvasExtShape4PointsType = fptRect);overload;
//    constructor Create(AType: TCanvasExtShape4PointsType = fptRect);overload;
    constructor Create;overload;   
    procedure Serialize(AMs: TStream);override;
    procedure DeSerialize(AMs: TStream);override;
  published
    property Filled: Boolean read FFilled write FFilled;
  end;

  TCanvasShapeTypes = (st4Pt, st2DImg, st2DText);
  TCanvasExtShapeList = class
  private
    FList: TList;
    function GetItems(Index: Integer): TCanvasExtShape;
  public
    constructor Create;
    destructor Destroy;override;
    property Items[Index: Integer]: TCanvasExtShape read GetItems;default;
    function Count: Integer;
    procedure Add(AShape: TCanvasExtShape);
    procedure Delete(Index: Integer);
    procedure Clear(AForce: Boolean = True);
  end;

const
  WM_CANVAS_BASE                = WM_APP;
  WM_CANVAS_CTL                 = WM_CANVAS_BASE + 0;
  WM_CANVAS_CLEAR               = WM_CANVAS_BASE + 1;
  WM_CANVAS_INSERTSHAPE         = WM_CANVAS_BASE + 2;

  SHAPE_ATTR_RESISTSCLEAR       = $00000001;

implementation

uses SysUtils, uCanvasExtUtil;

{ Utility functions }


procedure SerializeFont(MS: TStream;AFont: TFont);
var
  FontSize: Integer;
  FontColor: TColor;
  t: Cardinal;
begin
  if Assigned(AFont) then
    t := 1
  else
    t := 0;
  Ms.Write(t, SizeOf(t));

  if (t = 0) then
    Exit;

  FontSize := AFont.Size;
  Ms.Write(FontSize, SizeOf(FontSize));

  FontColor := AFont.Color;
  Ms.Write(FontColor, SizeOf(FontColor));

  SerializeString(Ms, AFont.Name);
end;

procedure DeSerializeFont(MS: TStream;var AFont: TFont);
var
  FontSize: Integer;
  FontColor: TColor;
  t: Cardinal;
  FontName: string;
begin
  Ms.Read(t, SizeOf(t));

  if (t = 0) then
    Exit;

  if not Assigned(AFont) then
    AFont := TFont.Create;

  Ms.Read(FontSize, SizeOf(FontSize));
  AFont.Size := FontSize;

  Ms.Read(FontColor, SizeOf(FontColor));
  AFont.Color := FontColor;

  DeSerializeString(Ms, FontName);
  AFont.Name := FontName;
end;

function HasAttr(const AValue, AAttr: Cardinal): Boolean;
begin
  Result := (AValue and AAttr) = AAttr;
end;

{ TCanvasExtShapeList }

procedure TCanvasExtShapeList.Add(AShape: TCanvasExtShape);
begin
  FList.Add(AShape);
end;

procedure TCanvasExtShapeList.Clear(AForce: Boolean = True);
var
  i: Integer;
  s: TCanvasExtShape;
begin
  for i := 0 to Count - 1 do
  begin
    s := Items[i];

    if AForce = False and HasAttr(s.Attributes, SHAPE_ATTR_RESISTSCLEAR) then
      Continue;
    s.Free;
  end;
  FList.Clear;
end;

function TCanvasExtShapeList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCanvasExtShapeList.Create;
begin
  FList := TList.Create;
end;

destructor TCanvasExtShapeList.Destroy;
begin
  Clear;
  FList.Free;
end;

procedure TCanvasExtShapeList.Delete(Index: Integer);
begin
  // Check bounds
  if (Index < 0) or (Index >= Count) then
    Exit;
    
  // Free shape
  Items[Index].Free;
  // Delete pointer from list
  FList.Delete(Index);
end;

function TCanvasExtShapeList.GetItems(Index: Integer): TCanvasExtShape;
begin
  Result := TCanvasExtShape(FList[Index]);
end;

{ TCanvasExtShape }

constructor TCanvasExtShape.Create;
begin
  BrushStyle := bsClear;
  Attributes := 0;
end;

procedure TCanvasExtShape.DeSerialize(AMs: TStream);
begin
  AMs.Read(FAttributes, sizeof(FAttributes));
  AMs.Read(FBrushColor, sizeof(FBrushColor));
  AMs.Read(FBrushStyle, sizeof(FBrushStyle));
  AMs.Read(FPenColor, sizeof(FPenColor));
end;

destructor TCanvasExtShape.Destroy;
begin
  inherited;
end;

procedure TCanvasExtShape.Serialize(AMs: TStream);
begin
  AMs.Write(FAttributes, sizeof(FAttributes));
  AMs.Write(FBrushColor, sizeof(FBrushColor));
  AMs.Write(FBrushStyle, sizeof(FBrushStyle));
  AMs.Write(FPenColor, sizeof(FPenColor));
end;

procedure TCanvasExtShape.SetCanvasBasicParams(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := BrushColor;
  ACanvas.Brush.Style := BrushStyle;
  ACanvas.Pen.Color   := PenColor;
end;

{ TCanvasExtShape4Points }

constructor TCanvasExtShape4Points.Create(
  const AX1, AY1, AX2, AY2: Integer;
  AType: TCanvasExtShape4PointsType);
begin
  inherited Create;
  X1 := AX1;
  Y1 := AY1;
  X2 := AX2;
  Y2 := AY2;
  FType := AType;
end;

procedure TCanvasExtShape4Points.DeSerialize(AMs: TStream);
begin
  inherited;
  AMs.Read(X1, SizeOf(X1));
  AMs.Read(Y1, SizeOf(Y1));
  AMs.Read(X2, SizeOf(X2));
  AMs.Read(Y2, SizeOf(Y2));
  AMs.Read(FType, SizeOf(FType));
  AMs.Read(FFilled, SizeOf(FFilled));
end;

procedure TCanvasExtShape4Points.Serialize(AMs: TStream);
begin
  inherited;
  AMs.Write(X1, SizeOf(X1));
  AMs.Write(Y1, SizeOf(Y1));
  AMs.Write(X2, SizeOf(X2));
  AMs.Write(Y2, SizeOf(Y2));
  AMs.Write(FType, SizeOf(FType));
  AMs.Write(FFilled, SizeOf(FFilled));
end;

procedure TCanvasExtShape4Points.Draw(ACanvas: TCanvas);
var
  r: TRect;
begin
  inherited;
  SetCanvasBasicParams(ACanvas);
  case FType of
    fptRect:
    begin
      if Filled then
      begin
        R.Left := X1;
        R.Top := Y1;
        R.Right := X2;
        R.Bottom := Y2;
        ACanvas.FillRect(R);
      end
      else
        ACanvas.Rectangle(X1, Y1, X2, Y2);
    end;
    fptEllipse: ACanvas.Ellipse(X1, Y1, X2, Y2);
    fptLine:
    begin
      ACanvas.MoveTo(X1, Y1);
      ACanvas.LineTo(X2, Y2);
    end;
  end;
end;

{
constructor TCanvasExtShape4Points.Create(AType: TCanvasExtShape4PointsType);
begin
  FType := AType;
  inherited Create;
end;
}
constructor TCanvasExtShape4Points.Create;
begin
  inherited;
end;

{ TCanvasExt2DText }

constructor TCanvasExt2DText.Create(const AX, AY: Integer; const AText: string);
begin
  inherited Create;
  X := AX;
  Y := AY;
  FFont := nil;
  FMsg := AText;
end;

constructor TCanvasExt2DText.Create;
begin
  inherited;
end;

procedure TCanvasExt2DText.DeSerialize(AMs: TStream);
begin
  inherited;

  AMs.Read(X, SizeOf(X));
  AMs.Read(Y, SizeOf(Y));

  DeSerializeString(AMs, FMsg);

  DeSerializeFont(AMs, FFont);
end;

destructor TCanvasExt2DText.Destroy;
begin
  if Assigned(FFont) then
    FFont.Free;
  inherited;
end;

procedure TCanvasExt2DText.Draw(ACanvas: TCanvas);
begin
  if Assigned(FFont) then
  begin
    ACanvas.Font.Name := FFont.Name;
    ACanvas.Font.Size := FFont.Size;
    ACanvas.Font.Color := FFont.Color;
    ACanvas.Font.Style := FFont.Style;
  end;
  SetCanvasBasicParams(ACanvas);
  ACanvas.TextOut(X, Y, FMsg);
end;

procedure TCanvasExt2DText.Serialize(AMs: TStream);
begin
  inherited;

  AMs.Write(X, SizeOf(X));
  AMs.Write(Y, SizeOf(Y));

  SerializeString(AMs, FMsg);

  SerializeFont(AMs, FFont);
end;

procedure TCanvasExt2DText.SetFont(Font: TFont);
begin
  if not Assigned(FFont) then
    FFont := TFont.Create;

  FFont.Name := Font.Name;
  FFont.Size := Font.Size;
  FFont.Color := Font.Color;
end;

{ TCanvasExt2DImageLoader }

constructor TCanvasExt2DImageLoader.Create(const AX1, AY1, AX2, AY2: Integer;
  const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  X1 := AX1;
  Y1 := AY1;
  X2 := AX2;
  Y2 := AY2;
  DoConstruct;
end;

procedure TCanvasExt2DImageLoader.DeSerialize(AMs: TStream);
begin
  inherited;
  AMs.Read(X1, SizeOf(X1));
  AMs.Read(Y1, SizeOf(Y1));
  AMs.Read(X2, SizeOf(X2));
  AMs.Read(Y2, SizeOf(X2));
  DeSerializeString(AMs, FFileName);

  DoConstruct;
end;

procedure TCanvasExt2DImageLoader.Serialize(AMs: TStream);
begin
  inherited;
  AMs.Write(X1, SizeOf(X1));
  AMs.Write(Y1, SizeOf(Y1));
  AMs.Write(X2, SizeOf(X2));
  AMs.Write(Y2, SizeOf(X2));
  SerializeString(AMs, FFileName);
end;

destructor TCanvasExt2DImageLoader.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TCanvasExt2DImageLoader.DoConstruct: Boolean;
begin
  if not Assigned(FBitmap) then
    FBitmap := Graphics.TBitmap.Create;

  try
    FBitmap.LoadFromFile(FFileName);

    Width := FBitmap.Width;
    Height := FBitmap.Height;

    with FRectSrc do
    begin
      Left   := 0;
      Top    := 0;
      Right  := Width;
      Bottom := Height;
    end;

    with FRectDest do
    begin
      Left   := X1;
      Top    := Y1;
      if X2 = -1 then
        Right := Left + Width
      else
        Right  := X2;

      if Y2 = -1 then
        Bottom := Top + Height
      else
        Bottom := Y2;
    end;
    Result := True;
  except
    Width := 0;
    Height := 0;
    Result := False;
  end;
end;

procedure TCanvasExt2DImageLoader.Draw(ACanvas: TCanvas);
begin
  inherited;
  SetCanvasBasicParams(ACanvas);
  ACanvas.BrushCopy(FRectDest, FBitmap, FRectSrc, BrushColor);
end;

constructor TCanvasExt2DImageLoader.Create;
begin
  inherited;
end;

end.
