unit figures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  TFigureClass = class of TFigureBase;


  TFigureBase = class
    Points: array of TPoint;
    PenColor: TColor;
    PenWidth: Integer;
    BrushColor: TColor;
    FigureClass: TFigureClass;
    constructor Create(FPoint: TPoint; PColor: TColor; BColor: TColor; PWidth: Integer);
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure AddPoint(APoint: TPoint); virtual;
  end;

  TTwoPointFigure = class(TFigureBase)
    procedure AddPoint(APoint: TPoint); virtual;
  end;

  TPen = class(TFigureBase)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TRectangle = class(TTwoPointFigure)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TEllipse = class(TTwoPointFigure)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TLine = class(TTwoPointFigure)
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TPolyLine = class(TFigureBase)
    procedure Draw(ACanvas: TCanvas); override;
  end;

var
  FiguresRegister: array of TFigureClass;
  FiguresBitmaps: array of TBitMap;

implementation

constructor TFigureBase.Create(FPoint: TPoint; PColor: TColor; BColor: TColor; PWidth: Integer);
begin
  SetLength(Points, Length(Points) + 1);
  Points[High(Points)] := FPoint;
  PenColor := PColor;
  PenWidth := PWidth;
  BrushColor := BColor;
end;

procedure TFigureBase.Draw(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Width := PenWidth;
    Pen.Color := PenColor;
    Brush.Color := BrushColor;
  end;
end;

procedure TFigureBase.AddPoint(APoint: TPoint);
begin
  SetLength(Points, Length(Points) + 1);
  Points[High(Points)] := APoint;
end;

procedure TTwoPointFigure.AddPoint(APoint: TPoint);
begin
  SetLength(Points, 2);
  Points[High(Points)] := APoint;
end;

procedure TPen.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.PolyLine(Points);
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

procedure RegisterFigure(AFigureClass: TFigureClass; AFigureBitmap: String);
var bmp: TBitmap;
begin
  SetLength(FiguresRegister, Length(FiguresRegister) + 1);
  FiguresRegister[High(FiguresRegister)] := AFigureClass;

  bmp := TBitmap.Create;
  bmp.LoadFromFile(AFigureBitmap);
  SetLength(FiguresBitmaps, Length(FiguresBitmaps) + 1);
  FiguresBitmaps[High(FiguresBitmaps)] := bmp;
end;

initialization
RegisterFigure(TPen, 'Icons/TPen.bmp');
RegisterFigure(TLine, 'Icons/TLine.bmp');
RegisterFigure(TPolyLine, 'Icons/TPolyLine.bmp');
RegisterFigure(TRectangle, 'Icons/TRectangle.bmp');
RegisterFigure(TEllipse, 'Icons/TEllipse.bmp');

end.

