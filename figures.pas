unit figures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLIntf, LCLType, transform, parameters;

type

  PPoint = array[0..3] of TPoint;

  TFigureBase = class
    DPoints: TDPointsArray;
    Points: TPointsArray;
    Region: HRGN;
    PenStyle: TPenStyle;
    BrushStyle: TBrushStyle;
    PenColor, BrushColor: TColor;
    PenWidth, Rounding: integer;
    constructor Create(FPoint: TDPoint);
    procedure SetRegion; virtual; abstract;
    procedure Draw(ACanvas: TCanvas); virtual;
    function FindTopLeft: TDPoint;
    function FindBottomRight: TDPoint;
  end;

  TRectangle = class(TFigureBase)
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TRoundRectangle = class(TFigureBase)
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TEllipse = class(TFigureBase)
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TLine = class(TFigureBase)
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TPolyLine = class(TFigureBase)
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TFigureClass = class of TFigureBase;

  function RectAroundLine(P1, P2: TPoint; Width: Integer): PPoint;

var
  CanvasFigures: array of TFigureBase;
  SelectedFigures: array of boolean;

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
    Result[0] := Point(P1.x - Width - 5, P1.y);
    Result[1] := Point(P1.x, P1.y + Width + 5);
    Result[2] := Point(P2.x + Width + 5, P2.y);
    Result[3] := Point(P2.x, P2.y - Width - 5);
	end
  else begin
    Result[0] := Point(P1.x - Width - 5, P1.y);
    Result[1] := Point(P1.x, P1.y - Width - 5);
    Result[2] := Point(P2.x + Width + 5, P2.y);
    Result[3] := Point(P2.x, P2.y + Width + 5);
	end;
end;

constructor TFigureBase.Create(FPoint: TDPoint);
begin
  SetLength(DPoints, Length(DPoints) + 1);
  DPoints[High(DPoints)] := FPoint;
end;

procedure TFigureBase.Draw(ACanvas: TCanvas);
var i: integer;
begin
  with ACanvas do
  begin
    Pen.Width := PenWidth;
    Pen.Style := PenStyle;
    Pen.Color := PenColor;
    Brush.Color := BrushColor;
    Brush.Style := BrushStyle;
  end;
  SetLength(Points, Length(DPoints));
  for i := Low(DPoints) to High(DPoints) do
    Points[i] := WorldToScreen(DPoints[i]);
end;

function TFigureBase.FindTopLeft: TDPoint;
var i: TDPoint;
begin
  Result := DPoints[Low(DPoints)];
  for i in DPoints do
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
  Result := DPoints[Low(Points)];
  for i in DPoints do
  begin
    if (i.x > Result.x) then
      Result.x := i.x;
    if (i.y > Result.y) then
      Result.y := i.y;
  end;
end;

procedure TRectangle.SetRegion;
begin
  Region := CreateRectRgn(
    WorldToScreen(DPoints[Low(DPoints)]).x,
    WorldToScreen(DPoints[Low(DPoints)]).y,
    WorldToScreen(DPoints[High(DPoints)]).x,
    WorldToScreen(DPoints[High(DPoints)]).y
  );
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Rectangle(
    Points[Low(Points)].x,
    Points[Low(Points)].y,
    Points[High(Points)].x,
    Points[High(Points)].y
  );
end;

procedure TRoundRectangle.SetRegion;
begin
  Region := CreateRoundRectRgn(
    WorldToScreen(DPoints[Low(DPoints)]).x,
    WorldToScreen(DPoints[Low(DPoints)]).y,
    WorldToScreen(DPoints[High(DPoints)]).x,
    WorldToScreen(DPoints[High(DPoints)]).y,
    Rounding, Rounding
  );
end;

procedure TRoundRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.RoundRect(
    Points[Low(Points)].x,
    Points[Low(Points)].y,
    Points[High(Points)].x,
    Points[High(Points)].y,
    Rounding, Rounding
  );
end;

procedure TEllipse.SetRegion;
begin
  Region := CreateEllipticRgn(
    WorldToScreen(DPoints[Low(DPoints)]).x,
    WorldToScreen(DPoints[Low(DPoints)]).y,
    WorldToScreen(DPoints[High(DPoints)]).x,
    WorldToScreen(DPoints[High(DPoints)]).y
  );
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Ellipse(
    Points[Low(Points)].x,
    Points[Low(Points)].y,
    Points[High(Points)].x,
    Points[High(Points)].y
  );
end;

procedure TLine.SetRegion;
var TempPoints: PPoint;
begin
  TempPoints := RectAroundLine(WorldToScreen(DPoints[Low(DPoints)]),
    WorldToScreen(DPoints[High(DPoints)]), PenWidth);
  Region := CreatePolygonRgn(TempPoints, Length(TempPoints), WINDING);
end;

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(
    Points[High(Points)],
    Points[Low(Points)]
  );
end;

procedure TPolyLine.SetRegion;
var
  TempRegion: HRGN;
  TempPoints: PPoint;
  i : integer;
begin
  for i := Low(DPoints) to High(DPoints) - 1 do
  begin
    TempPoints := RectAroundLine(WorldToScreen(DPoints[i]),
      WorldToScreen(DPoints[i+1]), PenWidth);
    TempRegion := CreatePolygonRgn(TempPoints, Length(TempPoints), WINDING);
    if (i = Low(DPoints)) then
      Region := TempRegion
    else begin
      CombineRgn(Region, Region, TempRegion, RGN_OR);
      DeleteObject(TempRegion);
		end;
	end;
end;

procedure TPolyLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Polyline(Points);
end;

end.
