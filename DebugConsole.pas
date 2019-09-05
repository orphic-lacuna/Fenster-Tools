unit DebugConsole;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DPIAwareForm{$IfDef Logging}, Logging{$EndIf};

type

  { Tfrm_DebugConsole }

  Tfrm_DebugConsole = class(TDPIAwareForm)
	  Memo1: TMemo;
		procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
		procedure FormResize(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public
    procedure LogNotification(aLogEntry: TLogParameterObject);
  end;

var
  frm_DebugConsole: Tfrm_DebugConsole;

implementation

{$R *.lfm}

{ Tfrm_DebugConsole }

procedure Tfrm_DebugConsole.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Visible then
  begin
    CanClose := false;
    Hide;
    Memo1.Lines.Clear;
  end;
end;

procedure Tfrm_DebugConsole.FormResize(Sender: TObject);
begin
  if WindowState = wsMinimized then Hide;
end;

procedure Tfrm_DebugConsole.FormShow(Sender: TObject);
begin
  Left := 0;
  Top := Screen.Height - Height - 80;
end;

procedure Tfrm_DebugConsole.Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 65) and (Shift = [ssCtrl]) then Memo1.SelectAll;
end;

procedure Tfrm_DebugConsole.LogNotification(aLogEntry: TLogParameterObject);
begin
  Memo1.Lines.Text := Memo1.Lines.Text + aLogEntry.ToString;
  Memo1.SelStart := Length(Memo1.Lines.Text);
  Memo1.SelLength := 0;
end;

end.

