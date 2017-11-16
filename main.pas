unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, Spin, StdCtrls, about, figures;

type

  TMainForm = class(TForm)
    BrushColorLabel: TLabel;
    PenSizeLabel: TLabel;
    PenColor: TColorButton;
    BrushColor: TColorButton;
    SelectColorDialog: TColorDialog;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MHelp: TMenuItem;
    MFile: TMenuItem;
    MHelpAbout: TMenuItem;
    MFileExit: TMenuItem;
    PaintBox: TPaintBox;
    PenSizeEdit: TSpinEdit;
    PenColorLabel: TLabel;
    ToolsButtons: TPanel;
    ToolsPanel: TPanel;
    procedure BrushColorChanged(Sender: TObject);
    procedure PenColorChanged(Sender: TObject);
    procedure PenSizeEditEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MHelpAboutClick(Sender: TObject);
    procedure MFileExitClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure FigureOnClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;
  CanvasFigures: Array of TFigureBase;
  currentFigureClass: TFigureClass;
  currentBrushColor: TColor;
  currentPenColor: TColor;
  currentPenWidth: Integer;
  isDrawing: Boolean;
  isDrawingBLine: Boolean;

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
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if not isDrawingBLine then
    begin
      SetLength(CanvasFigures, Length(CanvasFigures) + 1);
      CanvasFigures[High(CanvasFigures)] := currentFigureClass.Create(
        Point(X, Y),
        currentPenColor,
        currentBrushColor,
        currentPenWidth
      );
      isDrawing := True;
      if (currentFigureClass.ClassName = 'TPolyLine') then
        isDrawingBLine := True;
    end;

    if isDrawingBLine then
      with CanvasFigures[High(CanvasFigures)] do
      begin
        SetLength(Points, Length(Points) + 1);
        Points[High(Points)] := Point(X, Y);
      end;
  end

  else if (Button = mbRight) and (isDrawingBLine) then
  begin
    with CanvasFigures[High(CanvasFigures)] do
      SetLength(Points, Length(Points) - 1);
    isDrawingBLine := False;
  end;

  PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if isDrawingBLine then
    with CanvasFigures[High(CanvasFigures)] do
      Points[High(Points)] := Point(X, Y)
  else if isDrawing then
    CanvasFigures[High(CanvasFigures)].AddPoint(Point(X, Y));
  PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    isDrawing := false;
  PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var element: TFigureBase;
begin
  with PaintBox.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(0, 0, Width, Height);
  end;

  for element in CanvasFigures do
    element.Draw(PaintBox.Canvas);
end;

procedure TMainForm.PenSizeEditEditingDone(Sender: TObject);
begin
  try
    if StrToInt(PenSizeEdit.Caption) > PenSizeEdit.MaxValue then
      PenSizeEdit.Caption := IntToStr(PenSizeEdit.MaxValue)
    else if StrToInt(PenSizeEdit.Caption) < PenSizeEdit.MinValue then
      PenSizeEdit.Caption := IntToStr(PenSizeEdit.MinValue);
  except on EConvertError do
    PenSizeEdit.Caption := IntToStr(PenSizeEdit.MinValue);
  end;
  currentPenWidth := PenSizeEdit.Value;
end;

procedure TMainForm.BrushColorChanged(Sender: TObject);
begin
  currentBrushColor := BrushColor.ButtonColor
end;

procedure TMainForm.PenColorChanged(Sender: TObject);
begin
  currentPenColor := PenColor.ButtonColor;
end;

procedure TMainForm.FigureOnClick(Sender: TObject);
begin
  currentFigureClass := FiguresRegister[(Sender as TBitBtn).Tag];
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  Button: TBitBtn;
begin
  PaintBox.Parent.DoubleBuffered := true;

  for i := Low(FiguresRegister) to High(FiguresRegister) do
  begin
    Button := TBitBtn.Create(ToolsPanel);
    Button.Parent := ToolsButtons;
    Button.Tag := i;
    Button.Caption := '';
    Button.Top := 32 * (i div 3);
    Button.Left := 32 * (i mod 3);
    Button.Width := 30;
    Button.Height := 30;
    Button.Glyph := FiguresBitmaps[i];
    Button.OnClick := @FigureOnClick;
  end;
  if High(FiguresRegister) mod 3 > 0 then i := 1;
  ToolsButtons.Height := 32 * ((High(FiguresRegister)+1) div 3 + i);
end;

initialization
currentPenColor := clBlack;
currentBrushColor := clWhite;
currentPenWidth := 1;
isDrawing := False;
isDrawingBLine := False;
currentFigureClass := FiguresRegister[0];

end.

