unit parameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, ExtCtrls, Forms, Controls, Buttons,
  Dialogs, Spin;

type

  TParameter = class
    ParamPanel: TPanel;
    ParamLabel: TLabel;
    constructor Create(APanel: TPanel; ACaption: String);
    procedure ChangeParameter(Sender: TObject); virtual; abstract;
    procedure SetParamToInit; virtual; abstract;
  end;

  TColorParameter = class(TParameter)
    Param, InitValue: TColor;
    ParamControl: TColorButton;
    constructor Create(APanel: TPanel; ACaption: String; Init: TColor);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TIntegerSpinParameter = class(TParameter)
    Param, InitValue: Integer;
    ParamControl: TSpinEdit;
    constructor Create(APanel: TPanel; ACaption: String; Init: Integer);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TComboBoxParameter = class(TParameter)
    ParamControl: TComboBox;
    constructor Create(APanel: TPanel; ACaption: String);
  end;

  TPenStyleParameter = class(TComboBoxParameter)
    Param, InitValue: TPenStyle;
    constructor Create(APanel: TPanel; ACaption: String; Init: TPenStyle);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TBrushStyleParameter = class(TComboBoxParameter)
    Param, InitValue: TBrushStyle;
    constructor Create(APanel: TPanel; ACaption: String; Init: TBrushStyle);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TParamsArray = array of TParameter;

  TBrushStyleRecord = record
    Name: String;
    BrushStyle: TBrushStyle;
  end;

  TPenStyleRecord = record
    Name: String;
    PenStyle: TPenStyle;
  end;

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


implementation

constructor TParameter.Create(APanel: TPanel; ACaption: String);
begin
  ParamPanel := TPanel.Create(APanel);
  with ParamPanel do begin
    Align := alTop;
    Parent := APanel;
    Width := Parent.Width - 4;
    Height := 52;
    Left := 2;
    Caption := '';
  end;

  ParamLabel := TLabel.Create(ParamPanel);
  with ParamLabel do begin
    Parent := ParamPanel;
    AutoSize := False;
    Caption := ACaption;
    Width := Parent.Width;
    Height := 20;
    Top := 0;
    Alignment := taCenter;
    Font.Height := 20;
  end;
end;

constructor TColorParameter.Create(APanel: TPanel;
  ACaption: String; Init: TColor);
begin
  inherited Create(APanel, ACaption);
  ParamControl := TColorButton.Create(ParamPanel);
  with ParamControl do begin
    Parent := ParamPanel;
    AutoSize := False;
    ColorDialog := TColorDialog.Create(ParamPanel);
    Height := 30;
    Top := 22;
    Caption := '';
    Width := Parent.Width;
    OnColorChanged := @ChangeParameter;
  end;
  InitValue := Init;
  SetParamToInit;
end;

procedure TColorParameter.ChangeParameter(Sender: TObject);
begin
  Param := ParamControl.ButtonColor;
  ParamControl.Color := Param;
end;

procedure TColorParameter.SetParamToInit;
begin
  Param := InitValue;
  ParamControl.Color := InitValue;
  ParamControl.ButtonColor := InitValue;
end;

constructor TIntegerSpinParameter.Create(APanel: TPanel;
  ACaption: String; Init: Integer);
begin
  inherited Create(APanel, ACaption);
  ParamControl := TSpinEdit.Create(ParamPanel);
  with ParamControl do begin
    Parent := ParamPanel;
    AutoSize := False;
    Height := 30;
    Top := 22;
    Width := Parent.Width;
    Font.Size := 12;
    MinValue := 1;
    MaxValue := 200;
    OnEditingDone := @ChangeParameter;
  end;
  InitValue := Init;
  SetParamToInit;
end;

procedure TIntegerSpinParameter.ChangeParameter(Sender: TObject);
begin
  with ParamControl do begin
    try
      if StrToInt(Caption) > MaxValue then
        Caption := IntToStr(MaxValue)
      else if StrToInt(Caption) < MinValue then
        Caption := IntToStr(MinValue);
    except on EConvertError do
      Caption := IntToStr(MinValue);
    end;
    Param := ParamControl.Value;
  end;
end;

procedure TIntegerSpinParameter.SetParamToInit;
begin
  Param := InitValue;
  ParamControl.Value := InitValue;
  ParamControl.Caption := IntToStr(InitValue);
end;

constructor TComboBoxParameter.Create(APanel: TPanel; ACaption: String);
begin
  inherited Create(APanel, ACaption);
  ParamControl := TComboBox.Create(ParamPanel);
  with ParamControl do begin
    Parent := ParamPanel;
    ReadOnly := True;
    AutoSize := False;
    Height := 30;
    Width := Parent.Width;
    Top := 22;
    Font.Size := 10;
    OnChange := @ChangeParameter;
  end;
end;

constructor TPenStyleParameter.Create(APanel: TPanel;
  ACaption: String; Init: TPenStyle);
var i: TPenStyleRecord;
begin
  inherited Create(APanel, ACaption);
  for i in PEN_STYLES do
    ParamControl.Items.Add(i.Name);
  InitValue := Init;
  SetParamToInit;
end;

procedure TPenStyleParameter.ChangeParameter(Sender: TObject);
begin
  Param := PEN_STYLES[ParamControl.ItemIndex].PenStyle;
end;

procedure TPenStyleParameter.SetParamToInit;
var i: integer;
begin
  for i := Low(PEN_STYLES) to High(PEN_STYLES) do
    if PEN_STYLES[i].PenStyle = InitValue then
      ParamControl.ItemIndex := i;
  Param := InitValue;
end;

constructor TBrushStyleParameter.Create(APanel: TPanel;
  ACaption: String; Init: TBrushStyle);
var i: TBrushStyleRecord;
begin
  inherited Create(APanel, ACaption);
  with ParamControl do
  begin
    for i in BRUSH_STYLES do
      Items.Add(i.Name);
    InitValue := Init;
    SetParamToInit;
  end;
end;

procedure TBrushStyleParameter.ChangeParameter(Sender: TObject);
begin
  Param := BRUSH_STYLES[ParamControl.ItemIndex].BrushStyle;
end;

procedure TBrushStyleParameter.SetParamToInit;
var i: integer;
begin
  for i := Low(BRUSH_STYLES) to High(BRUSH_STYLES) do
    if BRUSH_STYLES[i].BrushStyle = InitValue then
      ParamControl.ItemIndex := i;
  Param := InitValue;
end;

end.

