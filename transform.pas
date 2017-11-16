unit transform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TDPoint = record
    x, y: double;
  end;

  TDPointsArray = array of TDPoint;
  TPointsArray = array of TPoint;

function DPoint(AX, AY: double): TDPoint;
function ScreenToWorld(APoint: TPoint): TDPoint;
function WorldToScreen(ADPoint: TDPoint): TPoint;
procedure SetScale(AScale: double);
procedure ZoomPoint(APoint: TPoint; AScale: double);

const
  MIN_SCALE = 0.01;
  MAX_SCALE = 50;

var
  Scale: double;
  Offset: TDPoint;
  PBWidth, PBHeight: integer;

implementation

function DPoint(AX, AY: double): TDPoint;
begin
  DPoint.x := AX;
  DPoint.y := AY;
end;

function ScreenToWorld(APoint: TPoint): TDPoint;
begin
  ScreenToWorld.x := APoint.x / Scale + Offset.x;
  ScreenToWorld.y := APoint.y / Scale + Offset.y;
end;

function WorldToScreen(ADPoint: TDPoint): TPoint;
begin
  WorldToScreen.x := Round((ADPoint.x - Offset.x) * Scale);
  WorldToScreen.y := Round((ADPoint.y - Offset.y) * Scale);
end;

procedure SetScale(AScale: double);
begin
  if AScale > MAX_SCALE then
    Scale := MAX_SCALE
  else if AScale < MIN_SCALE then
    Scale := MIN_SCALE
  else
    Scale := AScale;
end;

procedure ZoomPoint(APoint: TPoint; AScale: double);
var Point1, Point2: TDPoint;
begin
  Point1 := ScreenToWorld(APoint);
  SetScale(AScale);
  Point2 := ScreenToWorld(APoint);
  Offset.x := Offset.x - (Point2.x - Point1.x);
  Offset.y := Offset.y - (Point2.y - Point1.y);;
end;

initialization
Offset := DPoint(0,0);
Scale := 1.0;

end.
