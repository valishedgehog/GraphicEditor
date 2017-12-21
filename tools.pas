unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, Controls, LCLIntf, LCLType, math, figures, parameters, transform, constants;

type

  TTool = class
    Bitmap: TBitmap;
    Panel: TPanel;
    Params: TParamsArray;
    FigureClass: TFigureClass;
    isDrawing: Boolean;
    procedure CreateFigure(FPoint: TPoint); virtual;
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); virtual;
    procedure MouseMove(APoint: TPoint); virtual;
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); virtual;
    procedure AddPoint(APoint: TPoint); virtual;
    procedure CreateParameters(APanel: TPanel); virtual;
    procedure ShowParameters; virtual;
    procedure DestroyFigure;
    procedure AddPenParameters;
    procedure AddBrushParameters;
  end;

  TTwoPointTool = class(TTool)
    procedure AddPoint(APoint: TPoint); override;
  end;

  TActionTool = class(TTwoPointTool)
    initShift, initCtrl: Boolean;
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
    procedure CreateParameters(APanel: TPanel); override;
    procedure UpdateParameters;
    procedure ShowParameters; override;
    procedure AddAnchors(figure: TFigureBase);
    procedure RemoveAnchors(figure: TFigureBase);
  end;

  TInvisibleActionTool = class(TActionTool)
    prevPoint, newPoint: TDPoint;
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
  end;

  THandTool = class(TInvisibleActionTool)
    procedure MouseMove(APoint: TPoint); override;
  end;

  TMagnifierTool = class(TActionTool)
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
  end;

  TSelectionTool = class(TInvisibleActionTool)
    Mode: SelectionMode;
    AnchorIndex: Integer;
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
    procedure MovePoint(dx, dy: double);
    procedure ResizeFigure(dx, dy: double);
    procedure MoveFigurePoint(tPoint: TDPoint; minX, minY, maxX, maxY: integer; var dx, dy: double);
    procedure FigureModeMouseUp(APoint: TPoint; Button: TMouseButton);
    procedure ChangeSelection(i: integer; Button: TMouseButton);
  end;

  TMoveTool = class(TInvisibleActionTool)
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
  end;

  TPenTool = class(TTool)
    procedure CreateFigure(FPoint: TPoint); override;
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
    procedure CreateFigure(FPoint: TPoint); override;
    procedure MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint; Button: TMouseButton); override;
    procedure CreateParameters(APanel: TPanel); override;
  end;

  TToolClass = class of TTool;
  TToolArray = array of TTool;

procedure RegisterTool(ATool: TTool; AFigureClass: TFigureClass; BMPSorce: String);

var ToolsRegister: TToolArray;

implementation

procedure TTool.CreateFigure(FPoint: TPoint);
var p: TParameter;
begin
  SetLength(CanvasFigures, Length(CanvasFigures) + 1);
  CanvasFigures[High(CanvasFigures)] := FigureClass.Create(ScreenToWorld(FPoint));
    CanvasFigures[High(CanvasFigures)].Selected := False;

    if CanvasFigures[High(CanvasFigures)] is TAnchorsOnPointsFigure then
      with CanvasFigures[High(CanvasFigures)] as TAnchorsOnPointsFigure do begin
        for p in Params do
        case p.ParamLabel.Caption of
          PEN_WIDTH_LABEL: PenWidth := GPenWidth;
          PEN_COLOR_LABEL: PenColor := GPenColor;
          PEN_STYLE_LABEL: PenStyle := GPenStyle;
        end;
      end;

    if CanvasFigures[High(CanvasFigures)] is TAnchorsFigure then
      with CanvasFigures[High(CanvasFigures)] as TAnchorsFigure do begin
        for p in Params do
        case p.ParamLabel.Caption of
          BRUSH_COLOR_LABEL: BrushColor := GBrushColor;
          BRUSH_STYLE_LABEL: BrushStyle := GBrushStyle;
        end;
      end;
    if CanvasFigures[High(CanvasFigures)] is TRoundRectangle then
      with CanvasFigures[High(CanvasFigures)] as TRoundRectangle do begin
        for p in Params do
        case p.ParamLabel.Caption of
          ROUNDING_LABEL: Rounding := GRounding;
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
var i: TParameter;
begin
  Panel.Visible := True;
  for i in Params do i.SetParamToInit;
end;

procedure TTool.DestroyFigure;
begin
  CanvasFigures[High(CanvasFigures)].Free;
  SetLength(CanvasFigures, Length(CanvasFigures) - 1);
end;

procedure TTool.AddPenParameters;
var i: integer;
begin
  i := High(Params); SetLength(Params, Length(Params) + 3);
  Params[i + 1] := TPenStyleParameter.Create(Panel, PEN_STYLE_LABEL);
  Params[i + 2] := TIntegerSpinParameter.Create(Panel, PEN_WIDTH_LABEL);
  Params[i + 3] := TColorParameter.Create(Panel, PEN_COLOR_LABEL);
end;

procedure TTool.AddBrushParameters;
var i: integer;
begin
  i := High(Params); SetLength(Params, Length(Params) + 2);
  Params[i + 1] := TBrushStyleParameter.Create(Panel, BRUSH_STYLE_LABEL);
  Params[i + 2] := TColorParameter.Create(Panel, BRUSH_COLOR_LABEL);
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
  with (CanvasFigures[High(CanvasFigures)] as TAnchorsFigure) do begin
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
  CreateParametersFromList(GetParametersList, Panel, Params);
  UpdateParameters;
end;

procedure TActionTool.UpdateParameters;
var
  ParamsList, FigureList: TStringArray;
  figure: TFigureBase;
  param: TParameter;
  contains: boolean;
  sParam: String;
  i, j, k: integer;
begin
  for param in Params do
    param.ParamPanel.Visible := False;
  k := 0;

  ParamsList := GetParametersList;
  for figure in CanvasFigures do
    if figure.Selected then begin
      k := k + 1; j := 0;
      FigureList := figure.GetParametersList;

      for i := Low(ParamsList) to High(ParamsList) do begin
        contains := False;
        for sParam in FigureList do
          if ParamsList[i] = sParam
            then contains := True;
        if contains then begin
          ParamsList[j] := ParamsList[i];
          j := j + 1;
        end;
      end;
      SetLength(ParamsList, j);
    end;

  if (k > 0) then
    for param in Params do
      for sParam in ParamsList do
        if (sParam = param.ParamLabel.Caption) then begin
          param.ParamPanel.Visible := True; break;
        end;
end;

procedure TActionTool.ShowParameters;
begin
  inherited;
  UpdateParameters;
end;

procedure TActionTool.AddAnchors(figure: TFigureBase);
var TempAnchors: TAnchorsArray; i: integer;
begin
  with figure as TAnchorsOnPointsFigure do begin
    TempAnchors := GetAnchors;
    for i := Low(TempAnchors) to High(TempAnchors) do begin
      SetLength(AnchorsFigures, Length(AnchorsFigures) + 1);
      AnchorsFigures[High(AnchorsFigures)] := TempAnchors[i];
      AnchorsFigures[High(AnchorsFigures)].Figure := figure;
    end;
  end;
end;

procedure TActionTool.RemoveAnchors(figure: TFigureBase);
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

procedure TInvisibleActionTool.MouseDown(FPoint: TPoint; Button: TMouseButton; Shift: TShiftState);
begin
  inherited;
  with (CanvasFigures[High(CanvasFigures)] as TAnchorsOnPointsFigure) do
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
var i: integer;
begin
  inherited;

  Mode := figureMode;
  for i := Low(AnchorsFigures) to High(AnchorsFigures) do begin
    DeleteObject(AnchorsFigures[i].Region); AnchorsFigures[i].SetRegion;
    if PtInRegion(AnchorsFigures[i].Region, FPoint.x, FPoint.y) then begin
      Mode := anchorMode;
      AnchorIndex := i;
      newPoint := ScreenToWorld(FPoint);
      break;
    end;
  end;

  if initShift and (Mode = figureMode) then
    (CanvasFigures[High(CanvasFigures)] as TAnchorsOnPointsFigure).PenStyle := psDash;
end;

procedure TSelectionTool.MouseMove(APoint: TPoint);
var
  dx, dy: double;
begin
  inherited;
  if (Mode = anchorMode) then
    with AnchorsFigures[AnchorIndex] do begin
      prevPoint := newPoint;
      newPoint := ScreenToWorld(APoint);
      dx := newPoint.x - prevPoint.x;
      dy := newPoint.y - prevPoint.y;

      if (Position = PointPos) then MovePoint(dx, dy)
      else ResizeFigure(dx, dy);
    end;
end;

procedure TSelectionTool.MouseUp(APoint: TPoint; Button: TMouseButton);
begin
  if (mode = figureMode) then FigureModeMouseUp(APoint, Button);
  inherited;
end;

procedure TSelectionTool.MovePoint(dx, dy: double);
begin
  with AnchorsFigures[AnchorIndex] do begin
    with Figure do begin
      Points[PointIndex].x := Points[PointIndex].x + dx;
      Points[PointIndex].y := Points[PointIndex].y + dy;
    end;
    Points[0].x := Points[0].x + dx;
    Points[0].y := Points[0].y + dy;
  end;
end;

procedure TSelectionTool.ResizeFigure(dx, dy: double);
var
  minX, minY, maxX, maxY: integer;
  i: TAnchor;
  tPoint, p: TDPoint;
begin
  with AnchorsFigures[AnchorIndex] do begin
    p := Points[0];
    with Figure do begin
      if (Points[Low(Points)].x <= Points[High(Points)].x) then begin
        minX := Low(Points); maxX := High(Points);
      end else begin
        minX := High(Points); maxX := Low(Points);
      end;
      if (Points[Low(Points)].y <= Points[High(Points)].y) then begin
        minY := Low(Points); maxY := High(Points);
      end else begin
        minY := High(Points); maxY := Low(Points);
      end;

      case Position of
        TopLeft: begin
          tPoint.x := Points[minX].x + dx;
          tPoint.y := Points[minY].y + dy;
          MoveFigurePoint(tPoint, minX, minY, maxX, maxY, dx, dy);
          Points[minX].x := Points[minX].x + dx;
          Points[minY].y := Points[minY].y + dy;
  	end;
        TopRight: begin
          tPoint.x := Points[maxX].x + dx;
          tPoint.y := Points[minY].y + dy;
          MoveFigurePoint(tPoint, minX, minY, maxX, maxY, dx, dy);
          Points[maxX].x := Points[maxX].x + dx;
          Points[minY].y := Points[minY].y + dy;
  	end;
        BottomLeft: begin
          tPoint.x := Points[minX].x + dx;
          tPoint.y := Points[maxY].y + dy;
          MoveFigurePoint(tPoint, minX, minY, maxX, maxY, dx, dy);
          Points[minX].x := Points[minX].x + dx;
          Points[maxY].y := Points[maxY].y + dy;
  	end;
        BottomRight: begin
          tPoint.x := Points[maxX].x + dx;
          tPoint.y := Points[maxY].y + dy;
          MoveFigurePoint(tPoint, minX, minY, maxX, maxY, dx, dy);
          Points[maxX].x := Points[maxX].x + dx;
          Points[maxY].y := Points[maxY].y + dy;
  	end;
      end;

      for i in AnchorsFigures do
        if (i.Figure = Figure) then begin
          if (i.Points[0].x = p.x) then i.Points[0].x := i.Points[0].x + dx;
          if (i.Points[0].y = p.y) then i.Points[0].y := i.Points[0].y + dy;
        end;
    end;
  end;
end;

procedure TSelectionTool.MoveFigurePoint(tPoint: TDPoint; minX, minY, maxX, maxY: integer; var dx, dy: double);
begin
  with AnchorsFigures[AnchorIndex] do begin
    if ((tPoint.x >= Figure.Points[maxX].x) and ((Position = TopLeft) or (Position = BottomLeft)))
      or ((tPoint.x <= Figure.Points[minX].x) and ((Position = TopRight) or (Position = BottomRight)))
        then dx := 0;
    if ((tPoint.y <= Figure.Points[minY].y) and ((Position = BottomLeft) or (Position = BottomRight)))
      or ((tPoint.y >= Figure.Points[maxY].y) and ((Position = TopLeft) or (Position = TopRight)))
        then dy := 0;
  end;
end;

procedure TSelectionTool.FigureModeMouseUp(APoint: TPoint; Button: TMouseButton);
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
end;

procedure TSelectionTool.ChangeSelection(i: integer; Button: TMouseButton);
begin
  case Button of
    mbLeft: if not CanvasFigures[i].Selected then begin
      CanvasFigures[i].Selected := True;
      if CanvasFigures[i] is TAnchorsOnPointsFigure then
        AddAnchors(CanvasFigures[i]);
      UpdateParameters;
    end;
    mbRight: if CanvasFigures[i].Selected then begin
      CanvasFigures[i].Selected := False;
      if CanvasFigures[i] is TAnchorsOnPointsFigure then
        RemoveAnchors(CanvasFigures[i]);
      UpdateParameters;
    end;
  end;
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

procedure TPenTool.CreateFigure(FPoint: TPoint);
begin
  inherited;
  (CanvasFigures[High(CanvasFigures)] as TPolyLine).Tool := pen;
end;

procedure TPenTool.CreateParameters(APanel: TPanel);
begin
  inherited;
  AddPenParameters;
end;

procedure TRectangleTool.CreateParameters(APanel: TPanel);
begin
  inherited;
  AddPenParameters;
  AddBrushParameters;
end;

procedure TRoundRectangleTool.CreateParameters(APanel: TPanel);
var i: integer;
begin
  inherited;
  AddPenParameters;
  AddBrushParameters;
  i := High(Params); SetLength(Params, Length(Params) + 1);
  Params[i+1] := TIntegerSpinParameter.Create(Panel, ROUNDING_LABEL);
end;

procedure TEllipseTool.CreateParameters(APanel: TPanel);
begin
  inherited;
  AddPenParameters;
  AddBrushParameters;
end;

procedure TLineTool.CreateParameters(APanel: TPanel);
begin
  inherited;
  AddPenParameters;
end;

procedure TPolyLineTool.CreateFigure(FPoint: TPoint);
begin
  inherited;
  (CanvasFigures[High(CanvasFigures)] as TPolyLine).Tool := pline;
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
  AddPenParameters;
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

