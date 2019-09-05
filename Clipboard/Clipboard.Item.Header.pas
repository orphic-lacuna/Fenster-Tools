unit Clipboard.Item.Header;

{$mode objfpc}{$H+}
{$ModeSwitch ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils;

type

  { TClipbrdItemHeader }

  TClipboardItemFlags = set of (cbifDeleted, cbifActive, cbifInCopyBlock);

  TClipbrdItemHeader = class
  private
    procedure SetBelongsToIndexRelative(aValue: Integer);
    procedure SetCategoryCount(aValue: Cardinal);
    procedure SetDataAddress(aValue: Int64);
    procedure SetFlags(aValue: TClipboardItemFlags);
    procedure SetFormatCount(aValue: Word);
  public
    type
      { THeaderInfo }

      THeaderInfo = record
        CopyTimestamp: TDateTime;
        FormatCount: Word; // is needed, because fFormats may contain zero items if content of TClipboardItem is not yet loaded
        DataAddress: Int64;
        Flags: TClipboardItemFlags;
        PastedCount: DWord;
        TimestampOfLastInsertion: TDateTime;
        BelongsToIndexRelative: Integer;
        CategoryCount: Cardinal;
        ParentDirectoryID: Integer;
        procedure Reset;
      end;
  protected
    fHeaderInfo: THeaderInfo;
    fIsSaved: Boolean;
  public
    property Flags: TClipboardItemFlags read fHeaderInfo.Flags write SetFlags;
    property FormatCount: Word read fHeaderInfo.FormatCount write SetFormatCount;
    property CategoryCount: Cardinal read fHeaderInfo.CategoryCount write SetCategoryCount;
    property BelongsToIndexRelative: Integer read fHeaderInfo.BelongsToIndexRelative write SetBelongsToIndexRelative;
    property DataAddress: Int64 read fHeaderInfo.DataAddress write SetDataAddress;
    property IsSaved: Boolean read fIsSaved;
    property CopyTimestamp: TDateTime read fHeaderInfo.CopyTimestamp;
    property PastedCount: DWord read fHeaderInfo.PastedCount;

    constructor Create;
    procedure IncPastedCount;
    { loads header from stream }
    procedure LoadFromStream(aStream: TStream; Version: Integer);
    procedure SaveToStream(aStream: TStream);
  end;

implementation

uses Clipboard.List;

{ TClipbrdItemHeader.THeaderInfo }

procedure TClipbrdItemHeader.THeaderInfo.Reset;
begin
  CopyTimestamp := Now;
  FormatCount := 0; CategoryCount := 0;
  ParentDirectoryID := 0;
  PastedCount := 0; TimestampOfLastInsertion := 0;
  Flags := [cbifActive];
  DataAddress := -1; // indicates that no memory block was used before in the data filestream
end;

{ TClipbrdItemHeader }

constructor TClipbrdItemHeader.Create;
begin
  fHeaderInfo.Reset;
  fIsSaved := false;
end;

procedure TClipbrdItemHeader.IncPastedCount;
begin
  fHeaderInfo.PastedCount := fHeaderInfo.PastedCount + 1;
  fIsSaved := false;
end;

procedure TClipbrdItemHeader.SetBelongsToIndexRelative(aValue: Integer);
begin
  if fHeaderInfo.BelongsToIndexRelative = aValue then exit;

  fHeaderInfo.BelongsToIndexRelative := aValue;
  fIsSaved := false;
end;

procedure TClipbrdItemHeader.SetCategoryCount(aValue: Cardinal);
begin
  if fHeaderInfo.CategoryCount <> aValue then
  begin
    fHeaderInfo.CategoryCount := aValue;
    fIsSaved := false;
  end;
end;

procedure TClipbrdItemHeader.SetDataAddress(aValue: Int64);
begin
  if fHeaderInfo.DataAddress <> aValue then
  begin
    fHeaderInfo.DataAddress := aValue;
    fIsSaved := false;
  end;
end;

procedure TClipbrdItemHeader.SetFlags(aValue: TClipboardItemFlags);
begin
  if fHeaderInfo.Flags <> aValue then
  begin
    fHeaderInfo.Flags := aValue;
    fIsSaved := false;
  end;
end;

procedure TClipbrdItemHeader.SetFormatCount(aValue: Word);
begin
  if fHeaderInfo.FormatCount <> aValue then
  begin
    fHeaderInfo.FormatCount := aValue;
    fIsSaved := false;
  end;
end;

procedure TClipbrdItemHeader.LoadFromStream(aStream: TStream; Version: Integer);
begin
  case Version of
    CURRENT_CLIPBOARDLIST_VERSION: begin
      aStream.Read(fHeaderInfo, SizeOf(fHeaderInfo));
    end;
  end;
  fIsSaved := true;
end;

procedure TClipbrdItemHeader.SaveToStream(aStream: TStream);
begin
  if fIsSaved then exit else fIsSaved := true;
  aStream.Write(fHeaderInfo, SizeOf(fHeaderInfo));
end;

end.

