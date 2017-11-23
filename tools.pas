unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, Controls, LCLIntf, LCLType, math, figures, parameters, transform, Dialogs;

type

  TTool = class
    Bitmap: TBitmap;
    Panel: TPanel;
    Params: TParamsArray;
    FigureClass: TFigureClass;
    isDrawing: Boolean;
    procedure CreateFigure(FPoint: TPoint);
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); virtual;
    procedure MouseMove(APoint: TPoint); virtual;
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); virtual;
    procedure AddPoint(APoint: TPoint); virtual;
    procedure CreateParameters(APanel: TPanel); virtual;
    procedure ShowParameters;
    procedure FinishWork; virtual;
    procedure DestroyFigure;
    procedure AddPenParameters(APanel: TPanel);
    procedure AddBrushParameters(APanel: TPanel);
  end;

  TTwoPointTool = class(TTool)
    procedure AddPoint(APoint: TPoint); override;
  end;

  TActionTool = class(TTwoPointTool)
    initShift, initCtrl: Boolean;
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure CreateParameters(APanel: TPanel); override;
  end;

  TInvisibleActionTool = class(TActionTool)
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
  end;

  THandTool = class(TInvisibleActionTool)
    procedure MouseMove(APoint: TPoint); override;
  end;

  TMagnifierTool = class(TActionTool)
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
  end;

  TSelectionTool = class(TInvisibleActionTool)
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
    procedure ChangeSelection(i: integer; Button: TMouseButton);
    procedure AddSelection(figureIndex: integer);
    procedure RemoveSelection(figureIndex: integer);
    procedure FinishWork; override;
  end;

  TPenTool = class(TTool)
    procedure CreateParameters(APanel: TPanel); override;
  end;

  TRectangleTool = class(TTwoPointTool)
    procedure CreateParameters(APanel: TPanel); override;
  end;

  TRoundRectangleTool = class(TTwoPointTool)
    procedure CreateParameters(APanel: TPanel); override;
  end;

  TEllipseTool = class(TTwoPointTool)
    procedure CreateParameters(APanel: TPanel); override;
  end;

  TLineTool = class(TTwoPointTool)
    procedure CreateParameters(APanel: TPanel); override;
  end;

  TPolyLineTool = class(TTool)
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
    procedure CreateParameters(APanel: TPanel); override;
  end;

  TToolClass = class of TTool;

procedure RegisterTool(ATool: TTool; AFigureClass: TFigureClass; BMPSorce: String);

var ToolsRegister: array of TTool;

implementation

procedure TTool.CreateFigure(FPoint: TPoint);
var p: TParameter;
begin
  SetLength(CanvasFigures, Length(CanvasFigures) + 1);
  CanvasFigures[High(CanvasFigures)] := FigureClass.Create(ScreenToWorld(FPoint));
  with CanvasFigures[High(CanvasFigures)] do begin
    PenWidth := INIT_PEN_WIDTH;
    PenColor := INIT_PEN_COLOR;
    BrushColor := INIT_BRUSH_COLOR;
    PenStyle := INIT_PEN_STYLE;
    BrushStyle := INIT_BRUSH_STYLE;
    Rounding := INIT_ROUNDING;

    Selected := False;

    for p in Params do
    case p.ParamLabel.Caption of
      PEN_WIDTH_LABEL: PenWidth := (p as TIntegerSpinParameter).Param;
      PEN_COLOR_LABEL: PenColor := (p as TColorParameter).Param;
      BRUSH_COLOR_LABEL: BrushColor := (p as TColorParameter).Param;
      PEN_STYLE_LABEL: PenStyle := (p as TPenStyleParameter).Param;
      BRUSH_STYLE_LABEL: BrushStyle := (p as TBrushStyleParameter).Param;
      ROUNDING_LABEL: Rounding := (p as TIntegerSpinParameter).Param;
    end;
  end;
end;

procedure TTool.MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState);
begin
  if (Button = mbLeft) then begin
    CreateFigure(FPoint);
    isDrawing := True;
  end;
end;

procedure TTool.MouseMove(APoint: TPoint);
begin
  AddPoint(APoint);
end;

procedure TTool.MouseUp(APoint: TPoint; Button: TMouseButton);
begin
  isDrawing := False;
end;

procedure TTool.AddPoint(APoint: TPoint);
begin
  with CanvasFigures[High(CanvasFigures)] do begin
    SetLength(DPoints, Length(DPoints) + 1);
    DPoints[High(DPoints)] := ScreenToWorld(APoint);
  end;
end;

procedure TTool.CreateParameters(APanel: TPanel);
begin
  Panel := TPanel.Create(APanel);
  with Panel do begin
    Parent := APanel;
    Visible := False;
    Width := Parent.Width;
    Height := Parent.Height;
  end;
end;

procedure TTool.ShowParameters;
begin
  Panel.Visible := True;
end;

procedure TTool.FinishWork;
var i: TParameter;
begin
  isDrawing := False;
  Panel.Visible := False;
  for i in Params do
    i.SetParamToInit;
end;

procedure TTool.DestroyFigure;
begin
  CanvasFigures[High(CanvasFigures)].Free;
  SetLength(CanvasFigures, Length(CanvasFigures) - 1);
end;

procedure TTool.AddPenParameters(APanel: TPanel);
var i: integer;
begin
  i := High(Params); SetLength(Params, Length(Params) + 3);
  Params[i + 1] := TPenStyleParameter.Create(APanel, PEN_STYLE_LABEL, INIT_PEN_STYLE);
  Params[i + 2] := TIntegerSpinParameter.Create(APanel, PEN_WIDTH_LABEL, INIT_PEN_WIDTH);
  Params[i + 3] := TColorParameter.Create(APanel, PEN_COLOR_LABEL, INIT_PEN_COLOR);
end;

procedure TTool.AddBrushParameters(APanel: TPanel);
var i: integer;
begin
  i := High(Params); SetLength(Params, Length(Params) + 2);
  Params[i + 1] := TBrushStyleParameter.Create(APanel, BRUSH_STYLE_LABEL, INIT_BRUSH_STYLE);
  Params[i + 2] := TColorParameter.Create(APanel, BRUSH_COLOR_LABEL, INIT_BRUSH_COLOR);
end;

procedure TTwoPointTool.AddPoint(APoint: TPoint);
begin
  with CanvasFigures[High(CanvasFigures)] do begin
    SetLength(DPoints, 2);
    DPoints[High(DPoints)] := ScreenToWorld(APoint);
  end;
end;

procedure TActionTool.MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState);
begin
  CreateFigure(FPoint);
  isDrawing := True;
  with CanvasFigures[High(CanvasFigures)] do begin
    PenStyle := psDash;
    BrushStyle := bsClear;
  end;
  if ssShift in Shift then initShift := True
  else initShift := False;
  if ssCtrl in Shift then initCtrl := True
  else initCtrl := False;
end;

procedure TActionTool.CreateParameters(APanel: TPanel);
begin
  inherited;
end;

procedure TInvisibleActionTool.MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState);
begin
  inherited;
  with CanvasFigures[High(CanvasFigures)] do
    PenStyle := psClear;
end;

procedure TInvisibleActionTool.MouseUp(APoint: TPoint; Button: TMouseButton);
begin
  inherited;
  DestroyFigure;
end;

procedure THandTool.MouseMove(APoint: TPoint);
begin
  inherited;
  with CanvasFigures[High(CanvasFigures)] do begin
    Offset.x := Offset.x + DPoints[Low(DPoints)].x - DPoints[High(DPoints)].x;
    Offset.y := Offset.y + DPoints[Low(DPoints)].y - DPoints[High(DPoints)].y;
  end;
end;

procedure TMagnifierTool.MouseUp(APoint: TPoint; Button: TMouseButton);
var
  TopLeft, BottomRight: TDPoint;
  NewScale: double;
begin
  case Button of
    mbRight: ZoomPoint(APoint, Scale - 0.5);
    mbLeft: begin
      with CanvasFigures[High(CanvasFigures)] do begin
        TopLeft := FindTopLeft;
        BottomRight := FindBottomRight;
      end;
      if (sqr(TopLeft.x - BottomRight.x) + sqr(TopLeft.y - BottomRight.y) < 16*16) then
        ZoomPoint(APoint, Scale + 0.5)
      else begin
        NewScale := Scale * Min(PBWidth / Scale / (BottomRight.x - TopLeft.x),
          PBHeight / Scale / (BottomRight.y - TopLeft.y));
        ZoomPoint(WorldToScreen(DPoint((TopLeft.x + BottomRight.x) / 2,
          (TopLeft.y + BottomRight.y) / 2)), NewScale);
        inherited;
      end;
    DestroyFigure;
    end;
  end;
end;

procedure TSelectionTool.MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState);
begin
  inherited;
  if initShift then
    CanvasFigures[High(CanvasFigures)].PenStyle := psDash;
end;

procedure TSelectionTool.MouseUp(APoint: TPoint; Button: TMouseButton);
var i: integer; tempReg, reg: HRGN;
begin
  if initCtrl then begin
    for i := Low(CanvasFigures) to High(CanvasFigures) - 1 do
      ChangeSelection(i, Button);
  end

  else if initShift then begin
    with CanvasFigures[High(CanvasFigures)] do begin
      DeleteObject(Region); SetRegion;
      reg := Region;
    end;
    if Length(CanvasFigures) > 0 then
      for i := Low(CanvasFigures) to High(CanvasFigures) - 1 do
        with CanvasFigures[i] do begin
          DeleteObject(Region); SetRegion;
          tempReg := CreateRectRgn(0,0,1,1);
          if CombineRgn(tempReg, reg, Region, RGN_AND) <> NullRegion then
            ChangeSelection(i, Button);
          DeleteObject(tempReg);
        end;
  end

  else begin
    if Length(CanvasFigures) > 0 then
    for i := High(CanvasFigures) - 1 downto Low(CanvasFigures) do
      with CanvasFigures[i] do begin
        DeleteObject(Region); SetRegion;
        if PtInRegion(Region, APoint.x, APoint.y) then begin
          ChangeSelection(i, Button);
          Break;
        end;
      end;
  end;

  inherited;
end;

procedure TSelectionTool.ChangeSelection(i: integer; Button: TMouseButton);
begin
  case Button of
    mbLeft: if not CanvasFigures[i].Selected then AddSelection(i);
    mbRight: if CanvasFigures[i].Selected then RemoveSelection(i);
  end;
end;

procedure TSelectionTool.AddSelection(figureIndex: integer);
var p1, p2: TDPoint; width: integer;
begin
  SetLength(SelectionFigures, Length(SelectionFigures) + 1);
  SelectionFigures[High(SelectionFigures)] := TSelection.Create;
  CanvasFigures[figureIndex].Selected := True;
  with CanvasFigures[figureIndex] do begin
      p1 := FindTopLeft;
      p2 := FindBottomRight;
      width := PenWidth;
  end;
  with SelectionFigures[High(SelectionFigures)] do begin
    SelectedFigure := figureIndex;
    SetLength(DPoints, 2);
    DPoints[Low(DPoints)] := DPoint(p1.x - 7 - width, p1.y - 7 - width);
    DPoints[High(DPoints)] := DPoint(p2.x + 7 + width, p2.y + 7 + width);
  end;
end;

procedure TSelectionTool.RemoveSelection(figureIndex: integer);
var i, j: integer;
begin
  CanvasFigures[figureIndex].Selected := False;
  j := 0;
  for i := Low(SelectionFigures) to High(SelectionFigures) do
    if (SelectionFigures[i].SelectedFigure = figureIndex) then
      SelectionFigures[i].Free
    else begin
      SelectionFigures[j] := SelectionFigures[i];
      j := j + 1;
    end;
  SetLength(SelectionFigures, j);
end;

procedure TSelectionTool.FinishWork;
var i: TFigureBase;
begin
  inherited;
  for i in CanvasFigures do
    i.Selected := False;
  for i in SelectionFigures do
    i.Free;
  SetLength(SelectionFigures, 0);
end;

procedure TPenTool.CreateParameters(APanel: TPanel);
begin
  inherited;
  AddPenParameters(Panel);
end;

procedure TRectangleTool.CreateParameters(APanel: TPanel);
begin
  inherited;
  AddPenParameters(Panel);
  AddBrushParameters(Panel);
end;

procedure TRoundRectangleTool.CreateParameters(APanel: TPanel);
var i: integer;
begin
  inherited;
  AddPenParameters(Panel);
  AddBrushParameters(Panel);
  i := High(Params); SetLength(Params, Length(Params) + 1);
  Params[i+1] := TIntegerSpinParameter.Create(Panel, ROUNDING_LABEL, INIT_ROUNDING);;
end;

procedure TEllipseTool.CreateParameters(APanel: TPanel);
begin
  inherited;
  AddPenParameters(Panel);
  AddBrushParameters(Panel);
end;

procedure TLineTool.CreateParameters(APanel: TPanel);
begin
  inherited;
  AddPenParameters(Panel);
end;

procedure TPolyLineTool.MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState);
begin
  if Button = mbLeft then begin
    if not isDrawing then inherited;
    AddPoint(FPoint);
  end
  else begin
    with CanvasFigures[High(CanvasFigures)] do
      SetLength(DPoints, Length(DPoints) - 1);
    isDrawing := False;
  end;
end;

procedure TPolyLineTool.MouseMove(APoint: TPoint);
begin
  with CanvasFigures[High(CanvasFigures)] do
    DPoints[High(DPoints)] := ScreenToWorld(APoint);
end;

procedure TPolyLineTool.MouseUp(APoint: TPoint; Button: TMouseButton);
begin

end;

procedure TPolyLineTool.CreateParameters(APanel: TPanel);
begin
  inherited;
  AddPenParameters(Panel);
end;

procedure RegisterTool(ATool: TTool; AFigureClass: TFigureClass; BMPSorce: String);
begin
  SetLength(ToolsRegister, Length(ToolsRegister) + 1);
  ToolsRegister[High(ToolsRegister)] := ATool;
  with ToolsRegister[High(ToolsRegister)] do begin
    FigureClass := AFigureClass;
    Bitmap := TBitmap.Create;
    Bitmap.LoadFromFile(BMPSorce);
  end;
end;

Initialization
RegisterTool(TSelectionTool.Create, TRectangle, 'Icons/TSelectionTool.bmp');
RegisterTool(THandTool.Create, TRectangle, 'Icons/THandTool.bmp');
RegisterTool(TMagnifierTool.Create, TRectangle, 'Icons/TMagnifierTool.bmp');
RegisterTool(TPenTool.Create, TPolyLine, 'Icons/TPenTool.bmp');
RegisterTool(TLineTool.Create, TLine, 'Icons/TLineTool.bmp');
RegisterTool(TPolyLineTool.Create, TPolyLine, 'Icons/TPolyLineTool.bmp');
RegisterTool(TRectangleTool.Create, TRectangle, 'Icons/TRectangleTool.bmp');
RegisterTool(TRoundRectangleTool.Create, TRoundRectangle, 'Icons/TRoundRectangleTool.bmp');
RegisterTool(TEllipseTool.Create, TEllipse, 'Icons/TEllipseTool.bmp');

end.

