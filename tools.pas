unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, Controls, LCLIntf, LCLType, math, figures, parameters, transform;

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
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
    procedure CreateParameters(APanel: TPanel); override;
  end;

  TInvisibleActionTool = class(TActionTool)
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
  end;

  THandTool = class(TInvisibleActionTool)
    procedure MouseMove(APoint: TPoint); override;
  end;

  TMagnifierTool = class(TActionTool)
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
  end;

  TSelectionTool = class(TActionTool)
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
    procedure ChangeSelection(i: integer; Button: TMouseButton);
    procedure AddAnchors(figure: TFigureBase);
    procedure RemoveAnchors(figure: TFigureBase);
  end;

  TMoveTool = class(TInvisibleActionTool)
    prevPoint, newPoint: TDPoint;
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
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
    SetLength(Points, Length(Points) + 1);
    Points[High(Points)] := ScreenToWorld(APoint);
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
    SetLength(Points, 2);
    Points[High(Points)] := ScreenToWorld(APoint);
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
  initShift := ssShift in Shift;
  initCtrl := ssCtrl in Shift;
end;

procedure TActionTool.MouseUp(APoint: TPoint; Button: TMouseButton);
begin
  inherited;
  DestroyFigure;
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

procedure THandTool.MouseMove(APoint: TPoint);
begin
  inherited;
  with CanvasFigures[High(CanvasFigures)] do begin
    Offset.x := Offset.x + Points[Low(Points)].x - Points[High(Points)].x;
    Offset.y := Offset.y + Points[Low(Points)].y - Points[High(Points)].y;
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
      end;
    end;
  end;
  inherited;
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
    mbLeft: if not CanvasFigures[i].Selected then begin
      CanvasFigures[i].Selected := True;
      if CanvasFigures[i] is TAnchorsFigure then
        AddAnchors(CanvasFigures[i]);
		end;
		mbRight: if CanvasFigures[i].Selected then begin
      CanvasFigures[i].Selected := False;
      if CanvasFigures[i] is TAnchorsFigure then
        RemoveAnchors(CanvasFigures[i]);
    end;
  end;
end;

procedure TSelectionTool.AddAnchors(figure: TFigureBase);
var TempAnchors: TAnchorsArray; i: integer;
begin
  with figure as TAnchorsFigure do begin
    TempAnchors := GetAnchors;
    for i := Low(TempAnchors) to High(TempAnchors) do begin
      SetLength(AnchorsFigures, Length(AnchorsFigures) + 1);
      AnchorsFigures[High(AnchorsFigures)] := TempAnchors[i];
      AnchorsFigures[High(AnchorsFigures)].Figure := figure;
    end;
	end;
end;

procedure TSelectionTool.RemoveAnchors(figure: TFigureBase);
var i, j: integer;
begin
  j := 0;
  for i := Low(AnchorsFigures) to High(AnchorsFigures) do
  if AnchorsFigures[i].Figure = figure then
    AnchorsFigures[i].Free
  else begin
    AnchorsFigures[j] := AnchorsFigures[i];
    j := j + 1;
  end;
  SetLength(AnchorsFigures, j);
end;

procedure TMoveTool.MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState);
var i: TFigureBase;
begin
  for i in CanvasFigures do
    with i do begin
      DeleteObject(Region); SetRegion;
      if Selected and PtInRegion(Region, FPoint.x, FPoint.y) then begin
        isDrawing := True;
        break;
			end;
		end;
	newPoint := ScreenToWorld(FPoint);
end;

procedure TMoveTool.MouseMove(APoint: TPoint);
var i: TFigureBase; p: integer; dx, dy: double;
begin
  prevPoint := newPoint;
  newPoint := ScreenToWorld(APoint);
  dx := newPoint.x - prevPoint.x;
  dy := newPoint.y - prevPoint.y;

  for i in CanvasFigures do
    if i.Selected then
      for p := Low(i.Points) to High(i.Points) do begin
        i.Points[p].x := i.Points[p].x + dx;
        i.Points[p].y := i.Points[p].y + dy;
			end;

  for i in AnchorsFigures do begin
    i.Points[0].x := i.Points[0].x + dx;
    i.Points[0].y := i.Points[0].y + dy;
	end;
end;

procedure TMoveTool.MouseUp(APoint: TPoint; Button: TMouseButton);
begin
	isDrawing := False;
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
      SetLength(Points, Length(Points) - 1);
    isDrawing := False;
  end;
end;

procedure TPolyLineTool.MouseMove(APoint: TPoint);
begin
  with CanvasFigures[High(CanvasFigures)] do
    Points[High(Points)] := ScreenToWorld(APoint);
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
RegisterTool(TMoveTool.Create, TRectangle, 'Icons/TMoveTool.bmp');
RegisterTool(THandTool.Create, TRectangle, 'Icons/THandTool.bmp');
RegisterTool(TMagnifierTool.Create, TRectangle, 'Icons/TMagnifierTool.bmp');
RegisterTool(TPenTool.Create, TPolyLine, 'Icons/TPenTool.bmp');
RegisterTool(TLineTool.Create, TLine, 'Icons/TLineTool.bmp');
RegisterTool(TPolyLineTool.Create, TPolyLine, 'Icons/TPolyLineTool.bmp');
RegisterTool(TRectangleTool.Create, TRectangle, 'Icons/TRectangleTool.bmp');
RegisterTool(TRoundRectangleTool.Create, TRoundRectangle, 'Icons/TRoundRectangleTool.bmp');
RegisterTool(TEllipseTool.Create, TEllipse, 'Icons/TEllipseTool.bmp');

end.

