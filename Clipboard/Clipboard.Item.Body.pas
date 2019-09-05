unit Clipboard.Item.Body;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, fgl, Clipboard.Formats, Clipboard.Item.Header, ExtendedWinAPI{$IfDef Logging}, Logging{$EndIf};

type

  { TClipbrdItemBody }

  TClipbrdItemBody = class
  private
    function GetSize: Integer;
    procedure SetCopiedFrom(aValue: UnicodeString);
  protected
    type TCategoryList = specialize TFPGList<Integer>;
  protected
    fCopiedFrom: UnicodeString;
    fCategories: TCategoryList;
    fFormats: TClipbrdItemFormatList;
    fIsSaved: Boolean;
    fIsLoaded: Boolean;
  public
    property CopiedFrom: UnicodeString read fCopiedFrom write SetCopiedFrom;
    { this property is not intended to be accessed outside }
    property Formats: TClipbrdItemFormatList read fFormats;
    property IsSaved: Boolean read fIsSaved;
    property IsLoaded: Boolean read fIsLoaded;

    function HasCategory(aCategory: Integer): Boolean;
    procedure AddCategory(aCategory: Integer);
    procedure RemoveCategory(aCategory: Integer);
    function CategoryCount: Integer;

    { returns the number of formats that have been acquired from clipboard }
    function AcquireFromClipboard: Integer;

    { saves the clipboard item data to stream. Updates the FormatCount and CategoryCount in aHeader. }
    procedure SaveToStream(aStream: TStream; aHeader: TClipbrdItemHeader);
    { loads the clipbrd item data from stream }
    procedure LoadFromStream(aStream: TStream; aHeader: TClipbrdItemHeader);
    property Size: Integer read GetSize;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses FensterToolsCommon;

{ TClipbrdItemBody }

constructor TClipbrdItemBody.Create;
begin
  fIsSaved := true; fIsLoaded := false;

  fFormats := TClipbrdItemFormatList.Create;
  //fFormats.OnLoadFormatList := @LoadClipboardItemData;
  fCategories := TCategoryList.Create;
end;

function TClipbrdItemBody.GetSize: Integer;
var
  f: TClipbrdItemFormat;
begin
  //        length of fCopiedFrom          fCopiedFrom                        all categories
  Result := SizeOf(Integer) + (SizeOf(UnicodeChar)*Length(fCopiedFrom)) + SizeOf(Integer)*fCategories.Count;
  // all formats
  for f in fFormats do
  begin
    Result := Result + f.Size;
  end;
end;

procedure TClipbrdItemBody.SetCopiedFrom(aValue: UnicodeString);
begin
  if fCopiedFrom <> aValue then
  begin
    fCopiedFrom := aValue;
    fIsSaved := false;
  end;
end;

function TClipbrdItemBody.HasCategory(aCategory: Integer): Boolean;
begin
  Result := (fCategories.IndexOf(aCategory) >= 0);
end;

procedure TClipbrdItemBody.AddCategory(aCategory: Integer);
begin
  if not HasCategory(aCategory) then
  begin
    fIsSaved := false;
    fCategories.Add(aCategory);
  end;
end;

procedure TClipbrdItemBody.RemoveCategory(aCategory: Integer);
var
  cat_index: LongInt;
begin
  cat_index := fCategories.IndexOf(aCategory);
  if cat_index >= 0 then
  begin
    fIsSaved := false;
    fCategories.Delete(cat_index);
  end;
end;

function TClipbrdItemBody.CategoryCount: Integer;
begin
  Result := fCategories.Count;
end;

function TClipbrdItemBody.AcquireFromClipboard: Integer;
var
  formatItem: TClipboardItemFormatString;
  cb_owner: HWND;
begin
  Result := 0;
  if TryOpenClipboard(0) then
  begin
    formatItem := TClipboardItemFormatString.Create(CF_UNICODETEXT);
    formatItem.AcquireFromClipboard;
    fFormats.Add(formatItem);

    Result := fFormats.Count;

    cb_owner := GetClipboardOwner;
    if cb_owner <> 0 then fCopiedFrom := GetFullExeNameFromWindowHandle(cb_owner);

    CloseClipboard;
  end {$IfDef Logging}else Logger.Add('TClipbrdItemBody.AcquireFromClipboard: Unable to open clipboard', LT_ERROR){$EndIf};
  fIsSaved := false; fIsLoaded := true;
end;

procedure TClipbrdItemBody.SaveToStream(aStream: TStream; aHeader: TClipbrdItemHeader);
var
  byteCount, category: Integer;
  format: TClipbrdItemFormat;
begin
  if not fIsLoaded then exit;
  if fIsSaved then exit else fIsSaved := true;

  byteCount := Length(fCopiedFrom)*SizeOf(UnicodeChar);
  aStream.Write(byteCount, SizeOf(byteCount));
  if byteCount > 0 then aStream.Write(fCopiedFrom[1], byteCount);

  for category in fCategories do
  begin
    aStream.Write(category, SizeOf(category));
  end;
  aHeader.CategoryCount := fCategories.Count;

  for format in fFormats do
  begin
    format.SaveToStream(aStream);
  end;
  aHeader.FormatCount := fFormats.Count;
end;

procedure TClipbrdItemBody.LoadFromStream(aStream: TStream; aHeader: TClipbrdItemHeader);
var
  i, byteCount, category: Integer;
begin
  aStream.Position := aHeader.DataAddress;
  aStream.Read(byteCount{%H-}, SizeOf(byteCount));
  SetLength(fCopiedFrom, byteCount div SizeOf(UnicodeChar));
  if byteCount > 0 then aStream.Read(fCopiedFrom[1], byteCount);

  for i := 0 to aHeader.CategoryCount-1 do
  begin
    aStream.Read(category{%H-}, SizeOf(category));
    fCategories.Add(category);
  end;

  for i := 0 to aHeader.FormatCount-1 do
  begin
    fFormats.Add(TClipbrdItemFormat.CreateFromStream(aStream));
  end;

  fIsSaved := true; fIsLoaded := true;
end;

destructor TClipbrdItemBody.Destroy;
begin
  fCategories.Free;
  fFormats.Free;

  inherited Destroy;
end;

end.

