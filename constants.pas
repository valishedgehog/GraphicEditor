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
  JSON_FIGURES = 'Figures';
  JSON_CLASS_NAME = 'Class';
  JSON_PEN_STYLE = 'PenStyle';
  JSON_BRUSH_STYLE = 'BrushStyle';
  JSON_PEN_COLOR = 'PenColor';
  JSON_BRUSH_COLOR = 'BrushColor';
  JSON_PEN_WIDTH = 'PenWidth';
  JSON_ROUNDING = 'Rouding';
  JSON_POINTS = 'Points';

  PEN_WIDTH_LABEL = 'Pen width';
  PEN_STYLE_LABEL = 'Pen style';
  BRUSH_STYLE_LABEL = 'Brush style';
  PEN_COLOR_LABEL = 'Pen color';
  BRUSH_COLOR_LABEL = 'Brush color';
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

