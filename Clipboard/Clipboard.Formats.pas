unit Clipboard.Formats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Windows, Clipbrd, ExtendedWinAPI{$IfDef Logging}, Logging{$EndIf};

type
  TClipboardItemFormatString = class;

  { TClipbrdItemFormat }

  TClipbrdItemFormat = class
  protected
    type TClipbrdFormatType = Cardinal;
  protected
    fFormatType: TClipbrdFormatType;
    fIsSaved: Boolean;
    procedure LoadFromStream(aStream: TStream); virtual;
    function GetSize: Integer; virtual;
  public
    property Size: Integer read GetSize;

    constructor Create(aFormat: NativeUInt); virtual;
    procedure SaveToStream(aStream: TStream); virtual;
    procedure AcquireFromClipboard; virtual;
    procedure WriteToClipboard; virtual; abstract;
    class function CreateFromStream(aStream: TStream): TClipbrdItemFormat;
  end;
  TClipbrdItemFormatClass = class of TClipbrdItemFormat;

  { TClipbrdItemNamedFormat }

  TClipbrdItemNamedFormat = class(TClipbrdItemFormat)
  protected
    fFormatName: UnicodeString;
    procedure LoadFromStream(aStream: TStream); override;
    function GetSize: Integer; override;
  public
    procedure SaveToStream(aStream: TStream); override;
  end;

  { TClipboardItemFormatString }

  TClipboardItemFormatString = class(TClipbrdItemFormat)
  protected
    fContent, fOriginalContent: UnicodeString;
    procedure LoadFromStream(aStream: TStream); override;
    function GetSize: Integer; override;
  public
    property Content: UnicodeString read fContent;
    property OriginalContent: UnicodeString read fOriginalContent;

    procedure SaveToStream(aStream: TStream); override;
    procedure AcquireFromClipboard; override;
    procedure WriteToClipboard; override;
  end;

  { TClipbrdItemFormatList }

  TClipbrdItemFormatList = class(specialize TFPGList<TClipbrdItemFormat>)
  private
    //fOnLoadFormatList: TNotifyEvent;
    function GetAsString: UnicodeString;
    function GetIsSaved: Boolean;
  public
    //property OnLoadFormatList: TNotifyEvent read fOnLoadFormatList write fOnLoadFormatList;
    property AsString: UnicodeString read GetAsString;
    property IsSaved: Boolean read GetIsSaved;

    function FindFormat(aClass: TClipbrdItemFormatClass): TClipbrdItemFormat;
  end;

implementation

{ TClipbrdItemNamedFormat }

procedure TClipbrdItemNamedFormat.LoadFromStream(aStream: TStream);
var
  formatnamelen: LongInt;
  newFormatType: TClipbrdFormatType;
begin
  inherited LoadFromStream(aStream);
  // at this point the format type id has already been read from stream by class function TClipbrdItemFormat.CreateFromStream

  // we have an user registered clipboard format, so we must load the name
  aStream.Read(formatnamelen, SizeOf(formatnamelen));
  SetLength(fFormatName, formatnamelen div SizeOf(UnicodeChar));
  aStream.Read(fFormatName[1], formatnamelen);
  // and register the named clipboard format to obtain the correct clipboard format type id
  newFormatType := RegisterClipboardFormatW(PWideChar(fFormatName));
  if newFormatType <> 0 then fFormatType := newFormatType;
  // a change of the format type id does not imply the need of resaving this clipboard item, since all the needed information on clipboard
  // format type is stored in its name
end;

function TClipbrdItemNamedFormat.GetSize: Integer;
begin
  Result := inherited GetSize + SizeOf(LongInt) + (SizeOf(UnicodeChar)*Length(fFormatName));
end;

procedure TClipbrdItemNamedFormat.SaveToStream(aStream: TStream);
var
  formatnamelen: LongInt;
begin
  inherited SaveToStream(aStream);

  // we have an user registered clipboard format, so we must save the name, too
  SetLength(fFormatName, 256);
  formatnamelen := GetClipboardFormatNameW(fFormatType, @fFormatName[1], Length(fFormatName));
  SetLength(fFormatName, formatnamelen);
  formatnamelen := formatnamelen * SizeOf(UnicodeChar);
  aStream.Write(formatnamelen, SizeOf(formatnamelen));
  aStream.Write(fFormatName[1], formatnamelen);

  fIsSaved := true;
end;

{ TClipbrdItemFormatList }

function TClipbrdItemFormatList.GetAsString: UnicodeString;
var
  item: TClipbrdItemFormat;
begin
  Result := '';
  //if (Count = 0) and Assigned(fOnLoadFormatList) then fOnLoadFormatList(Self);
  for item in Self do
  begin
    if item is TClipboardItemFormatString then
    begin
      Result := TClipboardItemFormatString(item).Content;
      break;
    end;
  end;
end;

function TClipbrdItemFormatList.GetIsSaved: Boolean;
var
  item: TClipbrdItemFormat;
begin
  Result := true;
  for item in Self do
  begin
    if not item.fIsSaved then
    begin
      Result := false;
      break;
    end;
  end;
end;

function TClipbrdItemFormatList.FindFormat(aClass: TClipbrdItemFormatClass): TClipbrdItemFormat;
var
  item: TClipbrdItemFormat;
begin
  Result := nil;
  for item in Self do
  begin
    if item.InheritsFrom(aClass) then
    begin
      Result := item;
      exit;
    end;
  end;
end;

{ TClipbrdItemFormat }

function TClipbrdItemFormat.GetSize: Integer;
begin
  Result := SizeOf(TClipbrdFormatType);
end;

procedure TClipbrdItemFormat.LoadFromStream(aStream: TStream);
begin
  // at this point the format type id has already been read from stream by class function TClipbrdItemFormat.CreateFromStream
  fIsSaved := true;
end;

constructor TClipbrdItemFormat.Create(aFormat: NativeUInt);
begin
  fFormatType := aFormat;
end;

procedure TClipbrdItemFormat.SaveToStream(aStream: TStream);
begin
  aStream.Write(fFormatType, SizeOf(fFormatType));
  fIsSaved := true;
end;

procedure TClipbrdItemFormat.AcquireFromClipboard;
begin
  fIsSaved := false;
end;

class function TClipbrdItemFormat.CreateFromStream(aStream: TStream): TClipbrdItemFormat;
var
  format: TClipbrdFormatType;
begin
  aStream.Read(format{%H-}, SizeOf(format));

  case format of
    CF_UNICODETEXT: begin
      Result := TClipboardItemFormatString.Create(format);
    end;
    else begin
      if (format >= $C000) {and (format <= $FFFF)} then
      begin
        // we have an user registered clipboard format
        Result := TClipbrdItemNamedFormat.Create(format);
      end else Result := nil;
    end;
  end;
  if Result <> nil then Result.LoadFromStream(aStream);
end;

{ TClipboardItemFormatString }

procedure TClipboardItemFormatString.LoadFromStream(aStream: TStream);
var
  len: UInt64;
begin
  inherited LoadFromStream(aStream);
  // at this point the format type id has already been read from stream by class function TClipbrdItemFormat.CreateFromStream

  aStream.Read(len{%H-}, SizeOf(len));
  SetLength(fContent, len div 2);
  if len > 0 then aStream.Read(fContent[1], len);

  aStream.Read(len, SizeOf(len));
  SetLength(fOriginalContent, len div 2);
  if len > 0 then aStream.Read(fOriginalContent[1], len);
end;

function TClipboardItemFormatString.GetSize: Integer;
begin
  Result := inherited GetSize + 2*SizeOf(UInt64) + (SizeOf(UnicodeChar)*(Length(fContent)+Length(fOriginalContent)));
end;

procedure TClipboardItemFormatString.SaveToStream(aStream: TStream);
var
  len: UInt64;
begin
  inherited SaveToStream(aStream);

  len := Length(fContent)*SizeOf(UnicodeChar);
  aStream.Write(len, SizeOf(len));
  if len > 0 then aStream.Write(fContent[1], len);

  len := Length(fOriginalContent)*SizeOf(UnicodeChar);
  aStream.Write(len, SizeOf(len));
  if len > 0 then aStream.Write(fOriginalContent[1], len);
end;

procedure TClipboardItemFormatString.AcquireFromClipboard;
var
  MemoryHandle: HANDLE;
  cbstr: PWideChar;
begin
  inherited AcquireFromClipboard;
  // we assume that clipboard is already opened by a higher instance

  MemoryHandle := GetClipboardData(CF_UNICODETEXT);
  if MemoryHandle <> 0 then
  begin
    cbstr := PWideChar(GlobalLock(MemoryHandle));
    fContent := cbstr;
    GlobalUnlock(MemoryHandle);
  end else fContent := '';
end;

procedure TClipboardItemFormatString.WriteToClipboard;
var
  hMem: HGLOBAL;
  str: PWideChar;
begin
  // we assume that clipboard is already opened and cleared by a higher instance

  hMem := GlobalAlloc(GMEM_MOVEABLE, SizeOf(UnicodeChar)*(Length(fContent)+1));
  str := GlobalLock(hMem);
  if fContent = '' then
  begin
    {$IfDef Logging}Logger.Add('TClipboardItemFormatString.WriteToClipboard: fContent = ""', LT_ERROR);{$EndIf}
    str^ := WideChar(#0);
  end else CopyMemory(str, @fContent[1], SizeOf(UnicodeChar)*(Length(fContent)+1));

  GlobalUnlock(hMem);

  {$IfDef Logging}if {$EndIf}SetClipboardData(CF_UNICODETEXT, hMem){$IfDef Logging}= 0 then Logger.Add('TClipboardItemFormatString.WriteToClipboard: SetClipboardData failed with ' + IntToStr(GetLastError) + ' ' + SysErrorMessage(GetLastError), LT_ERROR);{$EndIf}
end;

end.

