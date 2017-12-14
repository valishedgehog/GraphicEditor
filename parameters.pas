unit parameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, ExtCtrls, Forms, Controls, Buttons,
  Dialogs, Spin, figures, constants;

type

  TParameter = class
    ParamPanel: TPanel;
    ParamLabel: TLabel;
    constructor Create(APanel: TPanel; ACaption: String); virtual;
    procedure ChangeParameter(Sender: TObject); virtual; abstract;
    procedure SetParamToInit; virtual; abstract;
  end;

  TColorParameter = class(TParameter)
    ParamControl: TColorButton;
    constructor Create(APanel: TPanel; ACaption: String); override;
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TIntegerSpinParameter = class(TParameter)
    ParamControl: TSpinEdit;
    constructor Create(APanel: TPanel; ACaption: String); override;
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TComboBoxParameter = class(TParameter)
    ParamControl: TComboBox;
    constructor Create(APanel: TPanel; ACaption: String); override;
  end;

  TPenStyleParameter = class(TComboBoxParameter)
    constructor Create(APanel: TPanel; ACaption: String); override;
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TBrushStyleParameter = class(TComboBoxParameter)
    constructor Create(APanel: TPanel; ACaption: String); override;
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TParamClass = class of TParameter;

  TParamsArray = array of TParameter;

  TParamRecord = record
    Name: String;
    ParamClass: TParamClass;
  end;

  TParamRecordArray = array of TParamRecord;

procedure CreateParametersFromList(List: TStringArray; APanel: TPanel; var Params: TParamsArray);
procedure RegisterParam(AName: String; AParamClass: TParamClass);
function GetParametersList: TStringArray;

var
  ParamsRegister: TParamRecordArray;
  GPenWidth: Integer;
  GPenColor: TColor;
  GBrushColor: TColor;
  GPenStyle: TPenStyle;
  GBrushStyle: TBrushStyle;
  GRounding: Integer;

implementation

procedure CreateParametersFromList(List: TStringArray; APanel: TPanel; var Params: TParamsArray);
var p: TParamRecord; s: String;
begin
  for s in List do
    for p in ParamsRegister do
      if (s = p.Name) then begin
        SetLength(Params, Length(Params) + 1);
        Params[High(Params)] := p.ParamClass.Create(APanel, s);
        break;
      end;
end;

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
    Font.Size := 12;
  end;
end;

constructor TColorParameter.Create(APanel: TPanel; ACaption: String);
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
end;

procedure TColorParameter.ChangeParameter(Sender: TObject);
var i: TFigureBase; Param: TColor;
begin
  Param := ParamControl.ButtonColor;
  case ParamLabel.Caption of
    PEN_COLOR_LABEL: GPenColor := Param;
    BRUSH_COLOR_LABEL: GBrushColor := Param;
  end;
  ParamControl.Color := Param;
  for i in CanvasFigures do
    if i.Selected then
      case ParamLabel.Caption of
        PEN_COLOR_LABEL: i.PenColor := Param;
        BRUSH_COLOR_LABEL: i.BrushColor := Param;
      end;
  ParamPanel.Parent.Parent.Parent.Parent.Invalidate;
end;

procedure TColorParameter.SetParamToInit;
var InitValue: TColor;
begin
  case ParamLabel.Caption of
    PEN_COLOR_LABEL: InitValue := GPenColor;
    BRUSH_COLOR_LABEL: InitValue := GBrushColor;
  end;
  ParamControl.Color := InitValue;
  ParamControl.ButtonColor := InitValue;
end;

constructor TIntegerSpinParameter.Create(APanel: TPanel; ACaption: String);
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
    OnChange := @ChangeParameter;
  end;
end;

procedure TIntegerSpinParameter.ChangeParameter(Sender: TObject);
var i: TFigureBase; Param: Integer;
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
  end;
  Param := ParamControl.Value;
  case ParamLabel.Caption of
    PEN_WIDTH_LABEL: GPenWidth := Param;
    ROUNDING_LABEL: GRounding := Param;
  end;
  for i in CanvasFigures do
    if i.Selected then begin
      case ParamLabel.Caption of
        PEN_WIDTH_LABEL: i.PenWidth := Param;
        ROUNDING_LABEL: i.Rounding := Param;
      end;
    end;
  ParamPanel.Parent.Parent.Parent.Parent.Invalidate;
end;

procedure TIntegerSpinParameter.SetParamToInit;
var InitValue: Integer;
begin
  case ParamLabel.Caption of
    PEN_WIDTH_LABEL: InitValue := GPenWidth;
    ROUNDING_LABEL: InitValue := GRounding;
  end;
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
    Font.Size := 8;
    OnChange := @ChangeParameter;
  end;
end;

constructor TPenStyleParameter.Create(APanel: TPanel; ACaption: String);
var i: TPenStyleRecord;
begin
  inherited Create(APanel, ACaption);
  for i in PEN_STYLES do
    ParamControl.Items.Add(i.Name);
end;

procedure TPenStyleParameter.ChangeParameter(Sender: TObject);
var i: TFigureBase;
begin
  GPenStyle := PEN_STYLES[ParamControl.ItemIndex].PenStyle;
  for i in CanvasFigures do
    if i.Selected then i.PenStyle := GPenStyle;
  ParamPanel.Parent.Parent.Parent.Parent.Invalidate;
end;

procedure TPenStyleParameter.SetParamToInit;
var i: integer;
begin
  for i := Low(PEN_STYLES) to High(PEN_STYLES) do
    if PEN_STYLES[i].PenStyle = GPenStyle then
      ParamControl.ItemIndex := i;
end;

constructor TBrushStyleParameter.Create(APanel: TPanel; ACaption: String);
var i: TBrushStyleRecord;
begin
  inherited Create(APanel, ACaption);
  with ParamControl do begin
    for i in BRUSH_STYLES do
      Items.Add(i.Name);
  end;
end;

procedure TBrushStyleParameter.ChangeParameter(Sender: TObject);
var i: TFigureBase;
begin
  GBrushStyle := BRUSH_STYLES[ParamControl.ItemIndex].BrushStyle;
  for i in CanvasFigures do
    if i.Selected then i.BrushStyle := GBrushStyle;
  ParamPanel.Parent.Parent.Parent.Parent.Invalidate;
end;

procedure TBrushStyleParameter.SetParamToInit;
var i: integer;
begin
  for i := Low(BRUSH_STYLES) to High(BRUSH_STYLES) do
    if BRUSH_STYLES[i].BrushStyle = GBrushStyle then
      ParamControl.ItemIndex := i;
end;

procedure RegisterParam(AName: String; AParamClass: TParamClass);
begin
  SetLength(ParamsRegister, Length(ParamsRegister) + 1);
  with ParamsRegister[High(ParamsRegister)] do begin
    Name := AName;
    ParamClass := AParamClass;
  end;
end;

function GetParametersList: TStringArray;
var p: TParamRecord;
begin
  for p in ParamsRegister do begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := p.Name;
  end;
end;

initialization
RegisterParam(ROUNDING_LABEL, TIntegerSpinParameter); GRounding := INIT_ROUNDING;
RegisterParam(PEN_STYLE_LABEL, TPenStyleParameter); GPenStyle := INIT_PEN_STYLE;
RegisterParam(PEN_WIDTH_LABEL, TIntegerSpinParameter); GPenWidth := INIT_PEN_WIDTH;
RegisterParam(PEN_COLOR_LABEL, TColorParameter); GPenColor := INIT_PEN_COLOR;
RegisterParam(BRUSH_STYLE_LABEL, TBrushStyleParameter); GBrushStyle := INIT_BRUSH_STYLE;
RegisterParam(BRUSH_COLOR_LABEL, TColorParameter); GBrushColor := INIT_BRUSH_COLOR;

end.

