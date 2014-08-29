unit uCanvasExtShapes;

interface

uses Classes, Graphics, Windows, Messages;

type

  TCanvasExtShape = class
  private
    FAttributes: Cardinal;
    FBrushColor: TColor;
    FBrushStyle: TBrushStyle;
    FPenColor: TColor;
  public
    constructor Create;
    procedure Draw(ACanvas: TCanvas);virtual;abstract;
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
    FFontAssigned: Boolean;
    X, Y: Integer;
    procedure SetFont(Font: TFont);
  public
    constructor Create(const AX, AY: Integer;const AText: string);
    procedure Draw(ACanvas: TCanvas);override;
    destructor Destroy;override;
  published
    property Font: TFont read FFont write SetFont;
  end;

  TCanvasExt2DImageLoader = class(TCanvasExtShape2D)
  private
    FRectSrc: TRect;
    FRectDest: TRect;
    FBitmap: Graphics.TBitmap;
  public
    Width, Height: Integer;
    constructor Create(const AX1, AY1, AX2, AY2: Integer; const AFileName: string);
    procedure Draw(ACanvas: TCanvas);override;
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
       AType: TCanvasExtShape4PointsType = fptRect);
  published
    property Filled: Boolean read FFilled write FFilled;
  end;


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

{ Utility functions }

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

{ TCanvasExt2DText }

constructor TCanvasExt2DText.Create(const AX, AY: Integer; const AText: string);
begin
  inherited Create;
  X := AX;
  Y := AY;
  FFont := nil;
  FFontAssigned := False;
  FMsg := AText; 
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
  FBitmap := Graphics.TBitmap.Create;

  try
    FBitmap.LoadFromFile(AFileName);
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
      Left   := AX1;
      Top    := AY1;
      if AX2 = -1 then
        Right := Left + Width
      else
        Right  := AX2;

      if AY2 = -1 then
        Bottom := Top + Height
      else
        Bottom := AY2;
    end;

  except
    Width := 0;
    Height := 0;
  end;
end;

destructor TCanvasExt2DImageLoader.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TCanvasExt2DImageLoader.Draw(ACanvas: TCanvas);
begin
  inherited;
  SetCanvasBasicParams(ACanvas);
  ACanvas.BrushCopy(FRectDest, FBitmap, FRectSrc, BrushColor);
end;

end.
