unit figures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLIntf, LCLType, transform, parameters;

type

  PPoint = array[0..3] of TPoint;

  AnchorPos = (TopLeft, TopRight, BottomLeft, BottomRight, PointPos);

  TFigureBase = class(TObject)
    Points: TDPointsArray;
    Region: HRGN;
    Selected: Boolean;
    PenStyle: TPenStyle;
    BrushStyle: TBrushStyle;
    PenColor, BrushColor: TColor;
    PenWidth, Rounding: integer;
    constructor Create(FPoint: TDPoint); virtual;
    procedure SetRegion; virtual; abstract;
    procedure Draw(ACanvas: TCanvas); virtual;
    function FindTopLeft: TDPoint;
    function FindBottomRight: TDPoint;
  end;

  TAnchor = class(TFigureBase)
    Figure: TFigureBase;
    Pos: AnchorPos;
    constructor Create(FPoint: TDPoint; APos: AnchorPos);
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
	end;

  TAnchorsArray = array of TAnchor;

  TAnchorsFigure = class(TFigureBase)
    function GetAnchors: TAnchorsArray; virtual;
	end;

  TAnchorsOnPointsFigure = class(TAnchorsFigure)
    function GetAnchors: TAnchorsArray; override;
	end;

  TRectangle = class(TAnchorsFigure)
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TRoundRectangle = class(TAnchorsFigure)
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TEllipse = class(TAnchorsFigure)
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TLine = class(TAnchorsOnPointsFigure)
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TPolyLine = class(TFigureBase)
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TFigureClass = class of TFigureBase;
  TFiguresArray = array of TFigureBase;

  function RectAroundLine(P1, P2: TPoint; Width: Integer): PPoint;

const
  SELECTION_PADDING = 5;
  ANCHOR_PADDING = 4;
  LINERECT_PADDING = 10;

var
  CanvasFigures: TFiguresArray;
  AnchorsFigures: TAnchorsArray;

implementation

function RectAroundLine(P1, P2: TPoint; Width: Integer): PPoint;
var TempPoint: TPoint;
begin
  if (P1.x > P2.x) then
  begin
    TempPoint := P1;
    P1 := P2; P2 := TempPoint;
  end;

  if (P1.y > P2.y) then
  begin
    Result[0] := Point(P1.x - Width - LINERECT_PADDING, P1.y);
    Result[1] := Point(P1.x, P1.y + Width + LINERECT_PADDING);
    Result[2] := Point(P2.x + Width + LINERECT_PADDING, P2.y);
    Result[3] := Point(P2.x, P2.y - Width - LINERECT_PADDING);
  end
  else begin
    Result[0] := Point(P1.x - Width - LINERECT_PADDING, P1.y);
    Result[1] := Point(P1.x, P1.y - Width - LINERECT_PADDING);
    Result[2] := Point(P2.x + Width + LINERECT_PADDING, P2.y);
    Result[3] := Point(P2.x, P2.y + Width + LINERECT_PADDING);
  end;
end;

constructor TFigureBase.Create(FPoint: TDPoint);
begin
  SetLength(Points, Length(Points) + 1);
  Points[High(Points)] := FPoint;

  PenStyle := INIT_PEN_STYLE;
  BrushStyle := INIT_BRUSH_STYLE;
  PenColor := INIT_PEN_COLOR;
  BrushColor := INIT_BRUSH_COLOR;
  PenWidth := INIT_PEN_WIDTH;
  Rounding := INIT_ROUNDING;
end;

procedure TFigureBase.Draw(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Width := PenWidth;
    Pen.Style := PenStyle;
    Pen.Color := PenColor;
    Brush.Color := BrushColor;
    Brush.Style := BrushStyle;
  end;
end;

function TFigureBase.FindTopLeft: TDPoint;
var i: TDPoint;
begin
  Result := Points[Low(Points)];
  for i in Points do
  begin
    if (i.x < Result.x) then
      Result.x := i.x;
    if (i.y < Result.y) then
      Result.y := i.y;
  end;
end;

function TFigureBase.FindBottomRight: TDPoint;
var i: TDPoint;
begin
  Result := Points[Low(Points)];
  for i in Points do
  begin
    if (i.x > Result.x) then
      Result.x := i.x;
    if (i.y > Result.y) then
      Result.y := i.y;
  end;
end;

constructor TAnchor.Create(FPoint: TDPoint; APos: AnchorPos);
begin
  SetLength(Points, Length(Points) + 1);
  Points[High(Points)] := FPoint;
  PenWidth := 1;
  PenColor := clBlack;
  PenStyle := psSolid;
  BrushColor := clRed;
  BrushStyle := bsSolid;
  Pos := APos;
end;

procedure TAnchor.SetRegion;
begin
  with WorldToScreen(Points[Low(Points)]) do
  Region := CreateRectRgn(
    x - ANCHOR_PADDING, y - ANCHOR_PADDING,
    x + ANCHOR_PADDING, y + ANCHOR_PADDING
  );
end;

procedure TAnchor.Draw(ACanvas: TCanvas);
var tx, ty: integer;
begin
  inherited;
  with WorldToScreen(Points[Low(Points)]) do
    case Pos of
      TopLeft: begin
        tx := x - SELECTION_PADDING;
        ty := y - SELECTION_PADDING;
			end;
      TopRight: begin
        tx := x + SELECTION_PADDING;
        ty := y - SELECTION_PADDING;
			end;
      BottomLeft: begin
        tx := x - SELECTION_PADDING;
        ty := y + SELECTION_PADDING;
			end;
      BottomRight: begin
        tx := x + SELECTION_PADDING;
        ty := y + SELECTION_PADDING;
			end;
      PointPos: begin
        tx := x; ty := y;
			end;
		end;
	ACanvas.Rectangle(
    tx - ANCHOR_PADDING, ty - ANCHOR_PADDING,
    tx + ANCHOR_PADDING, ty + ANCHOR_PADDING
  );
end;

function TAnchorsFigure.GetAnchors: TAnchorsArray;
var p1, p2: TDPoint;
begin
  SetLength(Result, 4);
	p1 := FindTopLeft; p2 := FindBottomRight;
  Result[0] := TAnchor.Create(p1, TopLeft);
  Result[1] := TAnchor.Create(DPoint(p2.x, p1.y), TopRight);
  Result[2] := TAnchor.Create(DPoint(p1.x, p2.y), BottomLeft);
  Result[3] := TAnchor.Create(p2, BottomRight);
end;

function TAnchorsOnPointsFigure.GetAnchors: TAnchorsArray;
var i: integer;
begin
  SetLength(Result, Length(Points));
  for i := Low(Points) to High(Points) do
    Result[i] := TAnchor.Create(Points[i], PointPos);
end;

procedure TRectangle.SetRegion;
begin
  Region := CreateRectRgn(
    WorldToScreen(Points[Low(Points)]).x - PenWidth div 2,
    WorldToScreen(Points[Low(Points)]).y - PenWidth div 2,
    WorldToScreen(Points[High(Points)]).x + PenWidth div 2,
    WorldToScreen(Points[High(Points)]).y + PenWidth div 2
  );
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Rectangle(
    WorldToScreen(Points[Low(Points)]).x,
    WorldToScreen(Points[Low(Points)]).y,
    WorldToScreen(Points[High(Points)]).x,
    WorldToScreen(Points[High(Points)]).y
  );
end;

procedure TRoundRectangle.SetRegion;
begin
  Region := CreateRoundRectRgn(
    WorldToScreen(Points[Low(Points)]).x - PenWidth div 2,
    WorldToScreen(Points[Low(Points)]).y - PenWidth div 2,
    WorldToScreen(Points[High(Points)]).x + PenWidth div 2,
    WorldToScreen(Points[High(Points)]).y + PenWidth div 2,
    Rounding, Rounding
  );
end;

procedure TRoundRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.RoundRect(
    WorldToScreen(Points[Low(Points)]).x,
    WorldToScreen(Points[Low(Points)]).y,
    WorldToScreen(Points[High(Points)]).x,
    WorldToScreen(Points[High(Points)]).y,
    Rounding, Rounding
  );
end;

procedure TEllipse.SetRegion;
begin
  Region := CreateEllipticRgn(
    WorldToScreen(Points[Low(Points)]).x - PenWidth div 2,
    WorldToScreen(Points[Low(Points)]).y - PenWidth div 2,
    WorldToScreen(Points[High(Points)]).x + PenWidth div 2,
    WorldToScreen(Points[High(Points)]).y + PenWidth div 2
  );
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Ellipse(
    WorldToScreen(Points[Low(Points)]).x,
    WorldToScreen(Points[Low(Points)]).y,
    WorldToScreen(Points[High(Points)]).x,
    WorldToScreen(Points[High(Points)]).y
  );
end;

procedure TLine.SetRegion;
var TempPoints: PPoint;
begin
  TempPoints := RectAroundLine(WorldToScreen(Points[Low(Points)]),
    WorldToScreen(Points[High(Points)]), PenWidth);
  Region := CreatePolygonRgn(TempPoints, Length(TempPoints), WINDING);
end;

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(
    WorldToScreen(Points[Low(Points)]),
    WorldToScreen(Points[High(Points)])
  );
end;

procedure TPolyLine.SetRegion;
var
  TempRegion: HRGN;
  TempPoints: PPoint;
  i : integer;
begin
  for i := Low(Points) to High(Points) - 1 do
  begin
    TempPoints := RectAroundLine(WorldToScreen(Points[i]),
      WorldToScreen(Points[i+1]), PenWidth);
    TempRegion := CreatePolygonRgn(TempPoints, Length(TempPoints), WINDING);
    if (i = Low(Points)) then
      Region := TempRegion
    else begin
      CombineRgn(Region, Region, TempRegion, RGN_OR);
      DeleteObject(TempRegion);
    end;
  end;
end;

procedure TPolyLine.Draw(ACanvas: TCanvas);
var ScreenPoints: TPointsArray; i: integer;
begin
  inherited;
  SetLength(ScreenPoints, Length(Points));
  for i := Low(ScreenPoints) to High(ScreenPoints) do
    ScreenPoints[i] := WorldToScreen(Points[i]);
  ACanvas.Polyline(ScreenPoints);
end;

end.
