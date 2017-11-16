unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf;

type

  { TAboutForm }
  TAboutForm = class(TForm)
    AppName: TLabel;
    Author: TLabel;
    CloseButton: TButton;
    Email: TLabel;
    GitHub: TLabel;
    procedure CloseButtonClick(Sender: TObject);
    procedure GitHubClick(Sender: TObject);
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.GitHubClick(Sender: TObject);
begin
  OpenURL('https:\\' + GitHub.Caption);
end;

procedure TAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

end.

