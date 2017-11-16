unit figures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, transform, parameters;

type

  TFigureBase = class
    DPoints: TDPointsArray;
    Points: TPointsArray;
    PenStyle: TPenStyle;
    BrushStyle: TBrushStyle;
    PenColor, BrushColor: TColor;
    PenWidth, Rounding: integer;
    constructor Create(FPoint: TDPoint);
    procedure Draw(ACanvas: TCanvas); virtual;
    function FindTopLeft: TDPoint;
    function FindBottomRight: TDPoint;
  end;

  TRectangle = class(TFigureBase)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TRoundRectangle = class(TFigureBase)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TEllipse = class(TFigureBase)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TLine = class(TFigureBase)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TPolyLine = class(TFigureBase)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TFigureClass = class of TFigureBase;

var
  CanvasFigures: array of TFigureBase;

implementation

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

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(
    Points[High(Points)],
    Points[Low(Points)]
  );
end;

procedure TPolyLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Polyline(Points);
end;

end.
