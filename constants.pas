unit constants;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TBrushStyleRecord = record
    Name: String;
    BrushStyle: TBrushStyle;
  end;

  TPenStyleRecord = record
    Name: String;
    PenStyle: TPenStyle;
  end;

  SelectionMode = (figureMode, anchorMode);
  PPoint = array[0..3] of TPoint;
  TStringArray = array of String;
  AnchorPos = (TopLeft, TopRight, BottomLeft, BottomRight, PointPos);
  PolyLineTool = (pen, pline);

const
  PEN_WIDTH_LABEL = 'Pen Width';
  PEN_STYLE_LABEL = 'Pen Style';
  BRUSH_STYLE_LABEL = 'Brush Style';
  PEN_COLOR_LABEL = 'Pen Color';
  BRUSH_COLOR_LABEL = 'Brush Color';
  ROUNDING_LABEL = 'Rounding';

  INIT_PEN_WIDTH = 1;
  INIT_PEN_COLOR = clBlack;
  INIT_BRUSH_COLOR = clWhite;
  INIT_PEN_STYLE = psSolid;
  INIT_BRUSH_STYLE = bsClear;
  INIT_ROUNDING = 100;

  BRUSH_STYLES: array[0..7] of TBrushStyleRecord = (
    (Name: 'Solid'; BrushStyle: bsSolid),
    (Name: 'Clear'; BrushStyle: bsClear),
    (Name: 'Hstripes'; BrushStyle: bsHorizontal),
    (Name: 'Vstripes'; BrushStyle: bsVertical),
    (Name: 'Left diagonal'; BrushStyle: bsFDiagonal),
    (Name: 'Right diagonal'; BrushStyle: bsBDiagonal),
    (Name: 'Cross'; BrushStyle: bsCross),
    (Name: 'Diagonal cross'; BrushStyle: bsDiagCross) );

  PEN_STYLES: array[0..5] of TPenStyleRecord = (
    (Name: 'Solid'; PenStyle: psSolid),
    (Name: 'No line'; PenStyle: psClear),
    (Name: 'Dots'; PenStyle: psDot),
    (Name: 'Dashes'; PenStyle: psDash),
    (Name: 'Dash dots'; PenStyle: psDashDot),
    (Name: 'Dash dot dots'; PenStyle: psDashDotDot) );

  SELECTION_PADDING = 5;
  ANCHOR_PADDING = 4;
  LINERECT_PADDING = 10;

implementation

end.

