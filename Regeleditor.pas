unit Regeleditor;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
			Menus, Buttons, Windows, LCLType, ShellApi, PCRE, ReplaceList, DPIAwareForm;

type
  { Tfrm_Regeleditor }

  Tfrm_Regeleditor = class(TDPIAwareForm)
	  mi_phWeekday: TMenuItem;
		mi_phTime: TMenuItem;
		btnSave: TBitBtn;
		btnCancel: TBitBtn;
		btnPlaceHolders: TBitBtn;
		btnHelp: TBitBtn;
		cbxIsRegEx: TCheckBox;
		cbxCaseSensitive: TCheckBox;
    cbxOnlyExtract: TCheckBox;
		mi_phAfterTomorrow: TMenuItem;
		mi_phYesterday: TMenuItem;
		mi_phTomorrow: TMenuItem;
		mi_phBeforeYesterday: TMenuItem;
		Edit_name: TEdit;
		mi_phDate: TMenuItem;
		Label_find: TLabel;
		label_name: TLabel;
		label_replace: TLabel;
		Memo_find: TMemo;
		Memo_replace: TMemo;
		pmPlaceHolders: TPopupMenu;
  procedure btnCancelClick(Sender: TObject);
		procedure btnHelpClick(Sender: TObject);
    procedure btnPlaceHoldersClick(Sender: TObject);
		procedure btnSaveClick(Sender: TObject);
    procedure cbxIsRegExClick(Sender: TObject);
    procedure cbxOnlyExtractClick(Sender: TObject);
		//procedure cbxOnlyExtractClick(Sender: TObject);
		procedure Edit_nameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure mi_phAfterTomorrowClick(Sender: TObject);
    procedure mi_phBeforeYesterdayClick(Sender: TObject);
    procedure mi_phDateClick(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
		procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure FormResize(Sender: TObject);
		procedure FormShow(Sender: TObject);
    procedure mi_phTimeClick(Sender: TObject);
    procedure mi_phTomorrowClick(Sender: TObject);
    procedure mi_phWeekdayClick(Sender: TObject);
    procedure mi_phYesterdayClick(Sender: TObject);
    procedure SetChanged(Sender: TObject);
  private
    fSave: boolean;
    fChanged: boolean;
    fReplaceListItem: TReplaceListItem;
    procedure SetReplaceListItem(Value: TReplaceListItem);
    procedure InsertPlaceHolder(const PlaceHolder: String);
  public
    property ReplaceListItem: TReplaceListItem read fReplaceListItem write SetReplaceListItem;
  end;

var
  frm_Regeleditor: Tfrm_Regeleditor;

implementation

{$R *.lfm}

{ Tfrm_Regeleditor }

procedure Tfrm_Regeleditor.btnPlaceHoldersClick(Sender: TObject);
begin
  pmPlaceHolders.PopUp;
end;

procedure Tfrm_Regeleditor.btnSaveClick(Sender: TObject);
begin
  fSave := true; fChanged := false;
  Close;
end;

procedure Tfrm_Regeleditor.cbxIsRegExClick(Sender: TObject);
begin
  if not cbxIsRegEx.Checked then cbxOnlyExtract.Checked := false;
  cbxOnlyExtract.Visible := cbxIsRegEx.Checked;
  SetChanged(Sender);
end;

procedure Tfrm_Regeleditor.cbxOnlyExtractClick(Sender: TObject);
begin
  Memo_replace.Visible := not cbxOnlyExtract.Checked;
  label_replace.Visible := not cbxOnlyExtract.Checked;
  FormResize(Sender);
  SetChanged(Sender);
end;

procedure Tfrm_Regeleditor.btnCancelClick(Sender: TObject);
begin
  fSave := false; fChanged := false;
  Close;
end;

procedure Tfrm_Regeleditor.btnHelpClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.adrian-zinke.de/FensterTools/Platzhalter.html', nil, nil, SW_SHOW);
end;

procedure Tfrm_Regeleditor.SetChanged(Sender: TObject);
begin
  fChanged := true;
end;

procedure Tfrm_Regeleditor.Edit_nameKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
begin
  if Key = VK_RETURN then Memo_find.SetFocus;
end;

procedure Tfrm_Regeleditor.FormCreate(Sender: TObject);
begin
  DesignTimeDPI := 120;
end;

procedure Tfrm_Regeleditor.mi_phAfterTomorrowClick(Sender: TObject);
begin
  InsertPlaceHolder('ÜBERMORGEN');
end;

procedure Tfrm_Regeleditor.mi_phBeforeYesterdayClick(Sender: TObject);
begin
  InsertPlaceHolder('VORGESTERN');
end;

procedure Tfrm_Regeleditor.InsertPlaceHolder(const PlaceHolder: String);
begin
  //if not Memo_replace.Focused then exit;
  Memo_replace.Lines.Text := Copy(Memo_replace.Lines.Text, 1, Memo_replace.SelStart) + PlaceHolder + Copy(Memo_replace.Lines.Text, Memo_replace.SelStart+Memo_replace.SelLength+1, Length(Memo_replace.Lines.Text));
end;

procedure Tfrm_Regeleditor.mi_phDateClick(Sender: TObject);
begin
  InsertPlaceHolder('DATUM');
end;

procedure Tfrm_Regeleditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Result: Integer;
  errMsg: UnicodeString;
  errOffset: NativeUInt;
begin
  ModalResult := mrCancel;
  CanClose := true;

  if fChanged then
  begin
		Result := MessageBoxW(Handle, PWideChar(UTF8Decode('Aktuelle Regel speichern?')), 'Frage', MB_ICONQUESTION or MB_YESNOCANCEL);
    if Result = ID_CANCEL then
    begin
      CanClose := false;
      exit;
    end else if Result = ID_YES then fSave := true
    else if Result = ID_NO then fSave := false;
  end;

  if cbxIsRegEx.Checked and fSave then
  begin
    if not TPerlRegEx.ValidatePattern(UTF8Decode(Memo_find.Lines.Text), errMsg, errOffset{%H-}) then
    begin
      CanClose := false;
      Memo_find.SetFocus; Memo_find.SelLength := 0; Memo_find.SelStart := errOffset;
      MessageBoxW(Handle, PWideChar(errMsg), 'Fehler', 48);
    end;
  end;

  if CanClose and fSave then
  begin
    if fReplaceListItem <> nil then
    begin
      // Werte nach Settings zurückschreiben
      fReplaceListItem.BeginUpdate;
      fReplaceListItem.Name := Edit_name.Text;
      fReplaceListItem.LookFor := Memo_find.Lines.Text;
      fReplaceListItem.ReplaceWith := Memo_replace.Lines.Text;
      fReplaceListItem.CaseSensitive := cbxCaseSensitive.Checked;
      fReplaceListItem.IsRegEx := cbxIsRegEx.Checked;
      fReplaceListItem.OnlyExtract := cbxOnlyExtract.Checked;
      fReplaceListItem.EndUpdate;
      ModalResult := mrOK;
		end;
	end;

  {$Message Warn 'Handle windowsShutdown here.'}
  //if WindowsShutdown then CanClose := true;
end;

procedure Tfrm_Regeleditor.FormKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure Tfrm_Regeleditor.FormResize(Sender: TObject);
const
  ControlSpacingY = 8;
begin
  if not cbxOnlyExtract.Checked then
  begin
    Memo_find.Height := (cbxCaseSensitive.Top - memo_find.Top) div 2 - ControlSpacingY;
    Memo_replace.height := cbxCaseSensitive.Top - Memo_find.BoundsRect.Bottom - 2 * ControlSpacingY;
    Memo_replace.Top := Memo_find.BoundsRect.Bottom + ControlSpacingY;
  end else
  begin
    Memo_find.Height := cbxCaseSensitive.Top - memo_find.Top - ControlSpacingY;
  end;
end;

procedure Tfrm_Regeleditor.FormShow(Sender: TObject);
begin
  Edit_name.SetFocus;
end;

procedure Tfrm_Regeleditor.mi_phTimeClick(Sender: TObject);
begin
  InsertPlaceHolder('ZEIT');
end;

procedure Tfrm_Regeleditor.mi_phTomorrowClick(Sender: TObject);
begin
  InsertPlaceHolder('MORGEN');
end;

procedure Tfrm_Regeleditor.mi_phWeekdayClick(Sender: TObject);
begin
  InsertPlaceHolder('TAG');
end;

procedure Tfrm_Regeleditor.mi_phYesterdayClick(Sender: TObject);
begin
  InsertPlaceHolder('GESTERN');
end;

procedure Tfrm_Regeleditor.SetReplaceListItem(Value: TReplaceListItem);
begin
  fReplaceListItem := Value;
  if fReplaceListItem <> nil then
  begin
    fReplaceListItem.Collection.BeginUpdate; // lock the replace list !
    Edit_name.Text := fReplaceListItem.Name;
    Memo_find.Lines.Text := fReplaceListItem.LookFor;
    Memo_replace.Lines.Text := fReplaceListItem.ReplaceWith;
    cbxCaseSensitive.Checked := fReplaceListItem.CaseSensitive;
    cbxIsRegEx.Checked := fReplaceListItem.IsRegEx;
    cbxOnlyExtract.Checked := fReplaceListItem.OnlyExtract;
    fReplaceListItem.Collection.EndUpdate;
    fChanged := false; fSave := false;
	end;
end;

end.

