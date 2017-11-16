unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, StdCtrls, Spin, Math, about, tools, figures, transform, Types;

type

	{ TMainForm }

  TMainForm = class(TForm)
    ScaleSpin: TFloatSpinEdit;
    HorScrollBar: TScrollBar;
    ScaleLabel: TLabel;
    MainMenu: TMainMenu;
    MFile: TMenuItem;
    MFileExit: TMenuItem;
    MHelp: TMenuItem;
    MHelpAbout: TMenuItem;
    ScalePanel: TPanel;
    ParametersPanel: TPanel;
    VerScrollBar: TScrollBar;
    ImageList: TImageList;
    PaintBox: TPaintBox;
    ToolsButtons: TPanel;
    ToolsPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure MHelpAboutClick(Sender: TObject);
    procedure MFileExitClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxPaint(Sender: TObject);
    procedure OnClickTool(Sender: TObject);
    procedure PaintBoxResize(Sender: TObject);
    procedure ScaleSpinChange(Sender: TObject);
    procedure SetScrollBars;
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
  end;

var
  MainForm: TMainForm;
  currentTool: TTool;
  isDrawing: boolean;
  isDrawingPolyLine: boolean;

implementation

{$R *.lfm}

procedure TMainForm.MFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MHelpAboutClick(Sender: TObject);
begin
  AboutForm.Show;
end;

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    if not isDrawingPolyLine then
    begin
      currentTool.CreateFigure(Point(X, Y));
      isDrawing := True;
      if currentTool is TPolyLineTool then
        isDrawingPolyLine := True;
    end;
    if isDrawingPolyLine then
      currentTool.AddPoint(Point(X, Y));
  end
  else if (Button = mbRight) and (isDrawingPolyLine) then
  begin
    with CanvasFigures[High(CanvasFigures)] do
      SetLength(DPoints, Length(DPoints) - 1);
    isDrawingPolyLine := False;
    isDrawing := False;
  end;
  PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if isDrawing then
    currentTool.MouseMove(Point(X, Y));
  SetScrollBars;
  PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  currentTool.MouseUp(Point(X, Y), Button);
  if (not isDrawingPolyLine) then
    isDrawing := False;
  ScaleSpin.Value := Scale * 100;
  SetScrollBars;
  PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then ZoomPoint(MousePos, Scale + 0.5)
  else ZoomPoint(MousePos, Scale - 0.5);
  ScaleSpin.Value := Scale * 100;
  SetScrollBars;
  PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  element: TFigureBase;
begin
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.FillRect(0, 0, Width, Height);
  for element in CanvasFigures do
    element.Draw(PaintBox.Canvas);
end;

procedure TMainForm.OnClickTool(Sender: TObject);
begin
  currentTool.HideParameters;
  currentTool := ToolsRegister[(Sender as TBitBtn).Tag];
  currentTool.ShowParameters;
end;

procedure TMainForm.PaintBoxResize(Sender: TObject);
begin
  PBWidth := PaintBox.Width;
  PBHeight := PaintBox.Height;
  SetScrollBars;
end;

procedure TMainForm.ScaleSpinChange(Sender: TObject);
begin
  with ScaleSpin do
  begin
    try
      if StrToFloat(Caption) > MaxValue then
        Caption := FloatToStr(MaxValue)
      else if StrToFloat(Caption) < MinValue then
        Caption := FloatToStr(MinValue);
    except on EConvertError do
      Caption := FloatToStr(MinValue);
    end;
    ZoomPoint(Point(PaintBox.Width div 2, PaintBox.Height div 2), Value / 100);
    SetScrollBars;
    PaintBox.Invalidate;
  end;
end;

procedure TMainForm.SetScrollBars;
var
  HMin, HMax, VMin, VMax: integer;
  i: TFigureBase;
  Corner: TDPoint;
begin
  HMin := Round(Min(Offset.x - 10, -10));
  HMax := Round(Max(Offset.x + 10,  10));
  VMin := Round(Min(Offset.y - 10, -10));
  VMax := Round(Max(Offset.y + 10,  10));

  Corner := ScreenToWorld(Point(PBWidth, PBHeight));
  for i in CanvasFigures do
  begin
    HMin := Min(HMin, Round(i.FindTopLeft.x - 10));
    HMax := Max(HMax, Round(Offset.x + i.FindBottomRight.x - Corner.x + 10));
    VMin := Min(VMin, Round(i.FindTopLeft.y - 10));
    VMax := Max(VMax, Round(Offset.y + i.FindBottomRight.y - Corner.y + 10));
  end;

  HorScrollBar.Min := HMin;
  HorScrollBar.Max := HMax;
  VerScrollBar.Min := VMin;
  VerScrollBar.Max := VMax;
  HorScrollBar.Position := Round(Offset.x);
  VerScrollBar.Position := Round(Offset.y);
  HorScrollBar.PageSize := Round((Corner.x - Offset.x) / (HMax - HMin));
  VerScrollBar.PageSize := Round((Corner.y - Offset.y) / (VMax - VMin));
end;

procedure TMainForm.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  Offset.x := HorScrollBar.Position;
  Offset.y := VerScrollBar.Position;
  PaintBox.Invalidate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i, k: integer;
  Button: TBitBtn;
begin
  ScaleSpin.Value := Scale * 100;
  ScaleSpin.MinValue := MIN_SCALE * 100;
  ScaleSpin.MaxValue := MAX_SCALE * 100;
  PBHeight := PaintBox.Height;
  PBWidth  := PaintBox.Width;

  for i := Low(ToolsRegister) to High(ToolsRegister) do
  begin
    ToolsRegister[i].CreateParameters(ParametersPanel);
    Button := TBitBtn.Create(ToolsButtons);
    Button.Parent := ToolsButtons;
    Button.Tag := i;
    Button.Caption := '';
    Button.Top := 32 * (i div 3);
    if (i mod 3 = 0) then k := 2;
    Button.Left := 32 * (i mod 3) + k;
    Button.Width := 30;
    Button.Height := 30;
    Button.Glyph := ToolsRegister[i].Bitmap;
    Button.OnClick := @OnClickTool;
  end;
  if Length(ToolsRegister) mod 3 > 0 then k := 1
  else k := 0;
  ToolsButtons.Height := 32 * ((Length(ToolsRegister)) div 3 + k);
  if Length(ToolsRegister) > 0 then
  begin
    currentTool := ToolsRegister[0];
    currentTool.ShowParameters;
  end;
end;

initialization
isDrawing := False;
isDrawingPolyLine := False;

end.