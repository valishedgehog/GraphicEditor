unit figures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLIntf, LCLType, transform, constants, fpjson;

type

  TFigureBase = class(TObject)
    Points: TDPointsArray;
    Region: HRGN;
    Selected: Boolean;
    PenStyle: TPenStyle;
    BrushStyle: TBrushStyle;
    PenColor, BrushColor: TColor;
    PenWidth, Rounding: integer;
    constructor Create(FPoint: TDPoint);
    procedure SetRegion; virtual; abstract;
    procedure Draw(ACanvas: TCanvas); virtual;
    function GetPenStyleNumber: Integer;
    function GetBrushStyleNumber: Integer;
    function GetParametersList: TStringArray; virtual;
    function FindTopLeft: TDPoint;
    function FindBottomRight: TDPoint;
    function Save: String;
  end;

  TAnchor = class(TFigureBase)
    Figure: TFigureBase;
    Position: AnchorPos;
    PointIndex: integer;
    constructor Create(FPoint: TDPoint; APos: AnchorPos; APointIndex: Integer);
    procedure SetRegion; override;
    function GetTempPos: TPoint;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TAnchorsArray = array of TAnchor;

  TAnchorsFigure = class(TFigureBase)
    function GetParametersList: TStringArray; override;
    function GetAnchors: TAnchorsArray; virtual;
  end;

  TAnchorsOnPointsFigure = class(TAnchorsFigure)
    function GetParametersList: TStringArray; override;
    function GetAnchors: TAnchorsArray; override;
  end;

  TRectangle = class(TAnchorsFigure)
    procedure SetRegion; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TRoundRectangle = class(TAnchorsFigure)
    function GetParametersList: TStringArray; override;
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

  TPolyLine = class(TAnchorsOnPointsFigure)
    Tool: PolyLineTool;
    procedure SetRegion; override;
    function GetAnchors: TAnchorsArray; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  TFigureClass = class of TFigureBase;
  TFiguresArray = array of TFigureBase;

  function RectAroundLine(P1, P2: TPoint; Width: Integer): PPoint;

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

function TFigureBase.GetParametersList: TStringArray;
begin
  SetLength(Result, 0);
end;

function TFigureBase.GetPenStyleNumber: Integer;
var i: integer;
begin
  for i := Low(PEN_STYLES) to High(PEN_STYLES) do
    if (PenStyle = PEN_STYLES[i].PenStyle) then begin
      Result := i; break;
    end;
end;

function TFigureBase.GetBrushStyleNumber: Integer;
var i: integer;
begin
  for i := Low(BRUSH_STYLES) to High(BRUSH_STYLES) do
    if (BrushStyle = BRUSH_STYLES[i].BrushStyle) then begin
      Result := i; break;
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

function TFigureBase.Save: String;
var
  obj: TJSONObject;
  pointsArr: TJSONArray;
  t: TDPoint;
begin
  obj := TJSONObject.Create;
  obj.Add('Class', Self.ClassName);

  obj.Add('PenStyle', TJSONIntegerNumber.Create(GetPenStyleNumber));
  obj.Add('BrushStyle', TJSONIntegerNumber.Create(GetBrushStyleNumber));
  obj.Add('PenColor', TJSONIntegerNumber.Create(PenColor));
  obj.Add('BrushBolor', TJSONIntegerNumber.Create(BrushColor));
  obj.Add('PenWidth', TJSONIntegerNumber.Create(PenWidth));
  obj.Add('Rouding', TJSONIntegerNumber.Create(Rounding));

  pointsArr := TJSONArray.Create;
  for t in Points do
    pointsArr.Add(TJSONObject.Create(['x', t.x, 'y', t.y]));
  obj.Add('Points', pointsArr);

  Result := obj.FormatJSON;
  obj.Free;
end;

constructor TAnchor.Create(FPoint: TDPoint; APos: AnchorPos; APointIndex: Integer);
begin
  SetLength(Points, Length(Points) + 1);
  Points[High(Points)] := FPoint;
  PenWidth := 1;
  PenColor := clBlack;
  PenStyle := psSolid;
  BrushColor := clRed;
  BrushStyle := bsSolid;
  Position := APos;
  PointIndex := APointIndex;
end;

procedure TAnchor.SetRegion;
var t: TPoint;
begin
  t := GetTempPos;
  Region := CreateRectRgn(
    t.x - ANCHOR_PADDING, t.y - ANCHOR_PADDING,
    t.x + ANCHOR_PADDING, t.y + ANCHOR_PADDING
  );
end;

function TAnchor.GetTempPos: TPoint;
var w: integer;
begin
  w := Figure.PenWidth;
  with WorldToScreen(Points[Low(Points)]) do
    case Position of
      TopLeft: begin
        Result.x := x - SELECTION_PADDING - w div 2;
        Result.y := y - SELECTION_PADDING - w div 2;
      end;
      TopRight: begin
        Result.x := x + SELECTION_PADDING + w div 2;
        Result.y := y - SELECTION_PADDING - w div 2;
      end;
      BottomLeft: begin
        Result.x := x - SELECTION_PADDING - w div 2;
        Result.y := y + SELECTION_PADDING + w div 2;
      end;
      BottomRight: begin
        Result.x := x + SELECTION_PADDING + w div 2;
        Result.y := y + SELECTION_PADDING + w div 2;
      end;
      PointPos: begin
        Result.x := x; Result.y := y;
      end;
    end;
end;

procedure TAnchor.Draw(ACanvas: TCanvas);
var t: TPoint;
begin
  inherited;
  t := GetTempPos;
  ACanvas.Rectangle(
    t.x - ANCHOR_PADDING, t.y - ANCHOR_PADDING,
    t.x + ANCHOR_PADDING, t.y + ANCHOR_PADDING
  );
end;

function TAnchorsFigure.GetParametersList: TStringArray;
begin
  SetLength(Result, 5);
  Result[4] := PEN_WIDTH_LABEL;
  Result[3] := PEN_STYLE_LABEL;
  Result[2] := BRUSH_STYLE_LABEL;
  Result[1] := PEN_COLOR_LABEL;
  Result[0] := BRUSH_COLOR_LABEL;
end;

function TAnchorsFigure.GetAnchors: TAnchorsArray;
var p1, p2: TDPoint;
begin
  SetLength(Result, 4);
  p1 := FindTopLeft; p2 := FindBottomRight;
  Result[0] := TAnchor.Create(p1, TopLeft, 0);
  Result[1] := TAnchor.Create(DPoint(p2.x, p1.y), TopRight, 0);
  Result[2] := TAnchor.Create(DPoint(p1.x, p2.y), BottomLeft, 0);
  Result[3] := TAnchor.Create(p2, BottomRight, 0);
end;

function TAnchorsOnPointsFigure.GetParametersList: TStringArray;
begin
  SetLength(Result, 3);
  Result[0] := PEN_WIDTH_LABEL;
  Result[1] := PEN_STYLE_LABEL;
  Result[2] := PEN_COLOR_LABEL;
end;

function TAnchorsOnPointsFigure.GetAnchors: TAnchorsArray;
var i: integer;
begin
  SetLength(Result, Length(Points));
  for i := Low(Points) to High(Points) do
    Result[i] := TAnchor.Create(Points[i], PointPos, i);
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

function TRoundRectangle.GetParametersList: TStringArray;
begin
  Result := inherited;
  SetLength(Result, Length(Result) + 1);
  Result[High(Result)] := ROUNDING_LABEL;
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

function TPolyLine.GetAnchors: TAnchorsArray;
begin
  if (Tool = pline) then Result := inherited;
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
