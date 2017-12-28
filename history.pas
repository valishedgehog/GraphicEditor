unit history;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, fpjson, jsonparser,
  figures, constants, transform;

function GetAppState: String;
procedure LoadAppState(State: String);
procedure AddFigures(JFigures: TJSONArray; mode: AddJFigureMode);
procedure UpdateCurrentState(PaintBox: TPaintBox);
procedure AddStateToHistory(State: String);
procedure ClearAppState;
procedure ClearHistory;
procedure ClearRedoHistory;
procedure Undo(PaintBox: TPaintBox);
procedure Redo(PaintBox: TPaintBox);
procedure CopySelected(PaintBox: TPaintBox);
procedure PasteSelected(PaintBox: TPaintBox);

var
  StatesHistory: array [1..3000] of String;
  currentState: Integer;
  redoAllowded, prevPaste: Boolean;

implementation

function GetAppState: String;
var
  i: TFigureBase;
  data: TJSONObject;
  figures: TJSONArray;
begin
  data := TJSONObject.Create;
  figures := TJSONArray.Create;
  for i in CanvasFigures do
    figures.Add(i.Save);
  data.Add(JSON_APPNAME, figures);
  Result := data.AsJSON;
  data.Free;
end;

procedure LoadAppState(State: String);
var JData: TJSONData;
begin
  with TJSONParser.Create(State) do
  try JData := Parse; finally Free; end;
  try AddFigures(TJSONArray(JData.GetPath(JSON_APPNAME)), add);
  finally JData.Free; end;
end;

procedure AddFigures(JFigures: TJSONArray; mode: AddJFigureMode);
var
  i, j, padding: integer;
  fClass: TFigureClass;
  JPoints: TJSONArray;
begin
  if mode = paste then padding := PASTE_FIGURE_PADDING
  else padding := 0;

  for i := 0 to JFigures.Count - 1 do
    with TJSONObject(JFigures.Items[i]) do begin
      fClass := GetFigureClassByName(Get(JSON_CLASS_NAME));
      SetLength(CanvasFigures, Length(CanvasFigures) + 1);
      CanvasFigures[High(CanvasFigures)] := fClass.Create(DPoint(0, 0));
      with CanvasFigures[High(CanvasFigures)] do begin
        Load(TJSONObject(JFigures.Items[i]));
        SetLength(Points, 0);
        JPoints := TJSONArray(GetPath(JSON_POINTS));
        for j := 0 to JPoints.Count - 1 do
          with TJSONObject(JPoints.Items[j]) do begin
            SetLength(Points, Length(Points) + 1);
            Points[High(Points)] := DPoint(Get('x') + padding, Get('y') + padding);
          end;
      end;
    end;
end;

procedure UpdateCurrentState(PaintBox: TPaintBox);
begin
  ClearAppState;
  LoadAppState(StatesHistory[currentState]);
  PaintBox.Invalidate;
end;

procedure AddStateToHistory(State: String);
var i: Integer;
begin
  if (State <> StatesHistory[currentState]) then begin
    if currentState = High(StatesHistory) then begin
      for i := Low(StatesHistory) to High(StatesHistory)-1 do
        StatesHistory[i] := StatesHistory[i+1];
    end else Inc(currentState);
    StatesHistory[currentState] := State;
  end;
end;

procedure ClearAppState;
var i: TFigureBase;
begin
  for i in CanvasFigures do
    i.Destroy;
  SetLength(CanvasFigures, 0);
  for i in AnchorsFigures do
    i.Destroy;
  SetLength(AnchorsFigures, 0);
end;

procedure ClearHistory;
var i: Integer;
begin
  for i := Low(StatesHistory) to High(StatesHistory) do
    StatesHistory[i] := '';
  currentState := Low(StatesHistory);
  StatesHistory[currentState] := GetAppState();
end;

procedure ClearRedoHistory;
var i: Integer;
begin
  redoAllowded := False;
  for i := currentState + 1 to High(StatesHistory) do
    StatesHistory[i] := '';
end;

procedure Undo(PaintBox: TPaintBox);
begin
  if (currentState <> Low(StatesHistory)) then begin
    Dec(currentState);
    UpdateCurrentState(PaintBox);
    redoAllowded := True;
  end;
end;

procedure Redo(PaintBox: TPaintBox);
begin
  if redoAllowded and (StatesHistory[currentState +1 ] <> '') then begin
    Inc(currentState);
    UpdateCurrentState(PaintBox);
  end;
end;

procedure CopySelected(PaintBox: TPaintBox);
var
  i: TFigureBase;
  JFigures: TJSONArray;
  JCopies: TJSONObject;
begin
  JCopies := TJSONObject.Create;
  JFigures := TJSONArray.Create;
  for i in CanvasFigures do
    if i.Selected then JFigures.Add(i.Save);
  JCopies.Add(JSON_COPIED_FIGURES, JFigures);
  ClipBoard.AsText := JCopies.FormatJSON;
  JCopies.Clear;
  PaintBox.Invalidate;
end;

procedure PasteSelected(PaintBox: TPaintBox);
var JData: TJSONData;
begin
  try
    with TJSONParser.Create(ClipBoard.AsText) do
    try JData := Parse; finally Free; end;
    try AddFigures(TJSONArray(JData.GetPath(JSON_COPIED_FIGURES)), paste);
    finally JData.Free; end;
    AddStateToHistory(GetAppState);
  except end;
  PaintBox.Invalidate;
end;

initialization
ClearHistory;

end.

