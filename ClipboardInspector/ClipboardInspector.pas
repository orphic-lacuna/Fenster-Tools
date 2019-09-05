unit ClipboardInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type

  { Tfrm_ClipboardInspector }

  Tfrm_ClipboardInspector = class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    lblProductName: TLabel;
    ListView1: TListView;
    PaintBox1: TPaintBox;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frm_ClipboardInspector: Tfrm_ClipboardInspector;

implementation

uses Clipboard.Manager, Clipboard.Formats, Clipboard.List, typinfo;

{$R *.lfm}

{ Tfrm_ClipboardInspector }

procedure Tfrm_ClipboardInspector.Button1Click(Sender: TObject);
var
  item: TClipboardItem;
  lvi: TListItem;
begin
  ListView1.Items.Clear;
  StatusBar1.SimpleText := IntToStr(ClipbrdManager.History.Count) + ' Einträge';
  item := ClipbrdManager.History.LastItem;
  while item <> nil do
  begin

    lvi := Listview1.Items.Add;
    lvi.Caption := IntToStr(item.Index);
    lvi.SubItems.Add(FormatDateTime('dd.mm.YYYY - hh:mm:ss:zzz', item.Header.CopyTimestamp));
    lvi.SubItems.Add(item.Body.CopiedFrom);
    lvi.SubItems.Add(IntToStr(item.Formats.Count));
    lvi.SubItems.Add(IntToStr(item.Header.PastedCount));
    lvi.SubItems.Add(SetToString(PTypeInfo(TypeInfo(item.Header.Flags)), LongInt(item.Header.Flags), true));
    lvi.SubItems.Add(IntToStr(item.Index - item.Header.BelongsToIndexRelative));
    lvi.SubItems.Add(item.Formats.AsString);

    item := item.PreviousItem;
  end;
end;

procedure Tfrm_ClipboardInspector.ListView1Click(Sender: TObject);
var
  i: Integer;
  index: LongInt;
  item: TClipboardItem;
begin
  for i := 0 to Listview1.Items.Count-1 do
  begin
    if Listview1.Items.Item[i].Selected then
    begin
      index := StrToInt(Listview1.Items.Item[i].Caption);
      item := ClipbrdManager.History.Items[index];
      lblProductName.Caption := item.CopiedFromAppInfo.ProductName;
      Paintbox1.Refresh;
      break;
    end;
  end;

end;

procedure Tfrm_ClipboardInspector.PaintBox1Paint(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Listview1.Items.Count-1 do
  begin
    if Listview1.Items.Item[i].Selected then
    begin
      lblProductName.Caption := ClipbrdManager.History.Items[StrToInt(Listview1.Items.Item[i].Caption)].CopiedFromAppInfo.ProductName;
      Paintbox1.Canvas.StretchDraw(Paintbox1.ClientRect, ClipbrdManager.History.Items[StrToInt(Listview1.Items.Item[i].Caption)].CopiedFromAppInfo.Icon);
      break;
    end;
  end;

end;

end.

