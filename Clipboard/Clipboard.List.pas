unit Clipboard.List;

{$mode objfpc}{$H+}

{$IfDef Logging}
  {$Define EnableLog}
{$EndIf}

interface

uses
  Windows, Classes, SysUtils, LinkedLists, Clipbrd, Dialogs, math, ExtendedWinAPI, Toolbox,
  Clipboard.Formats, Clipboard.Notifications, Clipboard.Item.Header, Clipboard.Item.Body, AppFileInfo, DX12.DWrite, dateutils, DX12.WinCodec, DX12.D2D1, DX12.DXGI, DX12.DCommon{$IfDef EnableLog}, logging{$EndIf};

type

  TClipboardList = class;
  TClipboardItem = class;

  { TRenderingInfo }

  TRenderingInfo = class
  private
    //fCritSect: TRTLCriticalSection;

    // acquired from clipboard stack
    fParent: TClipboardItem;
    fText: UnicodeString;
    fCopyTimestamp: TDateTime;
    fIconHandle: HICON;

    // Direct2D data
    fTextLayout, fTextLayoutTimestamp: IDWriteTextLayout;
    fTimestampString: UnicodeString;
    fIconAsBitmap: ID2D1Bitmap;


    function GetTimestampString: UnicodeString; virtual;
    procedure SetText(aValue: UnicodeString);
  public
    // acquired from clipboard stack
    property Parent: TClipboardItem read fParent;
    property Text: UnicodeString read fText write SetText;
    property TimestampString: UnicodeString read GetTimestampString;

    // Direct2D data
    property TextLayout: IDWriteTextLayout read fTextLayout;
    property TextLayoutTimestamp: IDWriteTextLayout read fTextLayoutTimestamp;
    property IconAsBitmap: ID2D1Bitmap read fIconAsBitmap;

    procedure MakeLayout(aWriteFactory: IDWriteFactory; aTextFormat: IDWriteTextFormat; maxWidth, maxHeight: Single);
    procedure MakeLayoutTimestamp(aWriteFactory: IDWriteFactory; aTextFormat: IDWriteTextFormat; maxWidth, maxHeight: Single);
    procedure ConvertIcon(aDeviceContext: ID2D1RenderTarget); virtual;

    class procedure DiscardDeviceDependentResources;

    constructor Create(aParent: TClipboardItem);
    destructor Destroy; override;
  end;

  { TEmptyRenderingInfo }

  TEmptyRenderingInfo = class(TRenderingInfo)
  private
    function GetTimestampString: UnicodeString; override;
  public
    procedure ConvertIcon(aDeviceContext: ID2D1RenderTarget); override;
    constructor Create; reintroduce;
  end;

  { TClipboardItem }

  TClipboardItem = class(specialize TGLinkedListItem<TClipboardList, TClipboardItem>)
  private
    fHeader: TClipbrdItemHeader;
    fBody: TClipbrdItemBody;
    fRenderingInfo: TRenderingInfo;

    class function CreateFromHeaderStream(aStream: TStream; headerSize: Word; Version: Integer): TClipboardItem;
    class function CreateFromClipboard: TClipboardItem;
    function GetBelongsTo: TClipboardItem;
    procedure SetBelongsTo(aValue: TClipboardItem);
    function GetBody: TClipbrdItemBody;
    function GetCopiedFromAppInfo: TAppFileInfo;
    function GetFormats: TClipbrdItemFormatList;
    function GetIsSaved: Boolean;
    function GetRenderingInfo: TRenderingInfo;
  public
    property Formats: TClipbrdItemFormatList read GetFormats;
    property BelongsTo: TClipboardItem read GetBelongsTo write SetBelongsTo;

    property Header: TClipbrdItemHeader read fHeader;
    property Body: TClipbrdItemBody read GetBody;

    property CopiedFromAppInfo: TAppFileInfo read GetCopiedFromAppInfo;

    property IsSaved: Boolean read GetIsSaved;
    property RenderingInfo: TRenderingInfo read GetRenderingInfo;

    function FindLastItemOfBlock: TClipboardItem;

    function WriteToClipboard(aFormatList: Array of TClipbrdItemFormat): Boolean; overload;
    function WriteToClipboard(aFormatClassList: Array of TClipbrdItemFormatClass): Boolean; overload;

    procedure SaveToStreams(aHeader, aData: TStream);

    constructor Create; override;
    destructor Destroy; override;
  end;

  { TClipboardList }

  TClipboardList = class(specialize TGLinkedList<TClipboardItem>)
  private
    const StartOffset = SizeOf(Integer); // for Header Version info field
  private
    Version: Integer;
    HeaderSize: Word;
    header, body: TFileStream;
    fClipbrdManager: TClipboardBase;
    fAppFileInfoList: TAppFileInfoList;

    { LoadItems loads Count items backwards from StartIndex. }
    procedure LoadItems(aCount: NativeUInt; StartIndex: NativeUInt); overload;
    procedure LoadItems(aCount: NativeUInt); overload;
  protected
    procedure HandleAccessToItemNotYetLoaded(aIndex: NativeUInt); override;
  public
    function AddFromClipboard: TClipboardItem;
    procedure ClearActiveItems;
    {$IfDef Debug}function CheckHealth: Boolean;{$EndIf}
    constructor Create(filenameHeader, filenameBody: UnicodeString; aClipbrdManager: TClipboardBase); reintroduce;
    destructor Destroy; override;
  end;

{

TRenderingInfo has 2 phases:
Phase 1: Creation and data acquisition from clipboard stack
  -> Create
  -> SetText
Phase 2: Rendering and data processing (no access to clipboard stack)
  -> MakeLayout

therefore we don't need a critical section
}

const
  CURRENT_CLIPBOARDLIST_VERSION = 1;

implementation

uses comobj, ActiveX;

var
  DeviceDependentResources: TFPList; // may only be accessed from clipboard window thread !!!

const
  INITIAL_LOAD_ITEM_COUNT = 50;

{ TEmptyRenderingInfo }

function TEmptyRenderingInfo.GetTimestampString: UnicodeString;
begin
  Result := '';
end;

procedure TEmptyRenderingInfo.ConvertIcon(aDeviceContext: ID2D1RenderTarget);
var
  wicConverter: IWICFormatConverter;
  wicFactory: IWICImagingFactory;
  wicDecoder: IWICBitmapDecoder;
  wicFrame: IWICBitmapFrameDecode;
  bitmapProps: TD2D1_BITMAP_PROPERTIES;
begin
  if fIconAsBitmap = nil then DeviceDependentResources.Add(@fIconAsBitmap);

  CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, wicFactory);
  wicFactory.CreateDecoderFromFilename(PWideChar(UnicodeString(RelativeToAbsolutePath('Icons\Empty.png'))), nil, GENERIC_READ, WICDecodeMetadataCacheOnDemand, wicDecoder);
  wicDecoder.GetFrame(0, wicFrame);

  wicFactory.CreateFormatConverter(wicConverter);
  wicConverter.Initialize(wicFrame, GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeCustom);

  //bitmapProps.bitmapOptions := D2D1_BITMAP_OPTIONS_NONE;
  bitmapProps.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
  bitmapProps.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;
  bitmapProps.dpiX := 96;
  bitmapProps.dpiY := 96;
  //bitmapProps.colorContext := nil;

  aDeviceContext.CreateBitmapFromWicBitmap(wicConverter, @bitmapProps, fIconAsBitmap);
end;

constructor TEmptyRenderingInfo.Create;
begin
  Text := 'Zwischenablage ist leer';
end;

{ TRenderingInfo }

constructor TRenderingInfo.Create(aParent: TClipboardItem);
begin
  fParent := aParent;
  fCopyTimestamp := fParent.Header.CopyTimestamp;
  Text := fParent.Body.Formats.AsString;
  fIconHandle := DuplicateIcon(0, fParent.CopiedFromAppInfo.Icon.Handle);

  // critical section is not necessary because TRenderingInfo is only created
  //InitializeCriticalSection(fCritSect);
end;

destructor TRenderingInfo.Destroy;
begin
  //DeleteCriticalSection(fCritSect);
  if fIconHandle <> 0 then
  begin
    DestroyIcon(fIconHandle);
    fIconHandle := 0;
  end;
  inherited Destroy;
end;

procedure TRenderingInfo.SetText(aValue: UnicodeString);
begin
  //EnterCriticalSection(fCritSect);
  if fText = aValue then Exit;
  fText := aValue;
  fTextLayout := nil;
  //LeaveCriticalSection(fCritSect);
end;

function TRenderingInfo.GetTimestampString: UnicodeString;
var
  min: Int64;
begin
  min := MinutesBetween(fCopyTimestamp, Now);
  if min = 0 then
    Result := 'Gerade eben'
  else if min = 1 then
    Result := 'Vor 1 Minute'
  else if min < 60 then
    Result := 'Vor ' + IntToStr(min) + ' Minuten'
  else if min < 120 then
    Result := 'Vor 1 Stunde (' + FormatDateTime('hh:nn', fCopyTimestamp) + ' Uhr)'
  else if min < 12*60 then
    Result := 'Vor ' + IntToStr(min div 60) + ' Stunden (' + FormatDateTime('hh:nn', fCopyTimestamp) + ' Uhr)'
  else if min < 24*60 then
    Result := 'Heute, ' + FormatDateTime('hh:nn', fCopyTimestamp) + ' Uhr'
  else if min < 24*60*2 then
    Result := 'Gestern, ' + FormatDateTime('hh:nn', fCopyTimestamp) + ' Uhr'
  else if min < 24*60*3 then
    Result := 'Vorgestern, ' + FormatDateTime('hh:nn', fCopyTimestamp) + ' Uhr'
  else if min < 24*60*7 then
    Result := 'Vor ' + IntToStr(min div (60*24)) + ' Tagen, ' + FormatDateTime('hh:nn', fCopyTimestamp) + ' Uhr'
  else if min < 24*60*8 then
    Result := 'Vor einer Woche, ' + FormatDateTime('hh:nn', fCopyTimestamp) + ' Uhr'
  else
    Result := FormatDateTime('dd.mm.yyyy', fCopyTimestamp) + ', ' + FormatDateTime('hh:nn', fCopyTimestamp) + ' Uhr';

  if Result <> fTimestampString then
  begin
    fTimestampString := Result;
    fTextLayoutTimestamp := nil;
  end;
end;

procedure TRenderingInfo.MakeLayout(aWriteFactory: IDWriteFactory; aTextFormat: IDWriteTextFormat; maxWidth, maxHeight: Single);
begin
  //EnterCriticalSection(fCritSect);

  if fTextLayout = nil then DeviceDependentResources.Add(@fTextLayout);

  {$IfDef Debug}if{$EndIf}aWriteFactory.CreateTextLayout(PWideChar(fText), Length(fText), aTextFormat, maxWidth, maxHeight, fTextLayout){$IfDef Debug} <> S_OK then raise Exception.Create('aWriteFactory.CreateTextLayout failed.'){$EndIf};

  //LeaveCriticalSection(fCritSect);
end;

procedure TRenderingInfo.MakeLayoutTimestamp(aWriteFactory: IDWriteFactory; aTextFormat: IDWriteTextFormat; maxWidth, maxHeight: Single);
begin
  //EnterCriticalSection(fCritSect);

  if fTextLayoutTimestamp = nil then DeviceDependentResources.Add(@fTextLayoutTimestamp);

  {$IfDef Debug}if {$EndIf}aWriteFactory.CreateTextLayout(PWideChar(TimestampString), Length(TimestampString), aTextFormat, maxWidth, maxHeight, fTextLayoutTimestamp){$IfDef Debug} <> S_OK then raise Exception.Create('aWriteFactory.CreateTextLayout failed.'){$EndIf};

  //LeaveCriticalSection(fCritSect);
end;

procedure TRenderingInfo.ConvertIcon(aDeviceContext: ID2D1RenderTarget);
var
  wicBitmap: IWICBitmap;
  wicConverter: IWICFormatConverter;
  wicFactory: IWICImagingFactory;
  bitmapProps: TD2D1_BITMAP_PROPERTIES;
begin
  if fIconAsBitmap = nil then DeviceDependentResources.Add(@fIconAsBitmap);

  CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, wicFactory);
  wicFactory.CreateBitmapFromHICON(fIconHandle, wicBitmap);
  wicFactory.CreateFormatConverter(wicConverter);
  wicConverter.Initialize(wicBitmap, GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeCustom);

  //bitmapProps.bitmapOptions := D2D1_BITMAP_OPTIONS_NONE;
  bitmapProps.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
  bitmapProps.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;
  bitmapProps.dpiX := 96;
  bitmapProps.dpiY := 96;
  //bitmapProps.colorContext := nil;

  aDeviceContext.CreateBitmapFromWicBitmap(wicConverter, @bitmapProps, fIconAsBitmap);
end;

class procedure TRenderingInfo.DiscardDeviceDependentResources;
var
  i: Integer;
begin
  for i := 0 to DeviceDependentResources.Count-1 do IUnknown(DeviceDependentResources.Items[i]^) := nil;
  DeviceDependentResources.Clear;
end;

{ TClipboardItem }

constructor TClipboardItem.Create;
begin
  inherited Create;

  fHeader := TClipbrdItemHeader.Create;
  fBody := TClipbrdItemBody.Create;
end;

class function TClipboardItem.CreateFromHeaderStream(aStream: TStream; headerSize: Word; Version: Integer): TClipboardItem;
begin
  Result := TClipboardItem.Create;
  Result.Index := (aStream.Position - TClipboardList.StartOffset) div headerSize;
  Result.fHeader.LoadFromStream(aStream, Version);
end;

class function TClipboardItem.CreateFromClipboard: TClipboardItem;
begin
  Result := TClipboardItem.Create;
  Result.fHeader.FormatCount := Result.fBody.AcquireFromClipboard;

  // Item index will be set by TClipboardList.Add method
end;

function TClipboardItem.GetBelongsTo: TClipboardItem;
begin
  if cbifInCopyBlock in fHeader.Flags then
  begin
    Result := Parent.Items[Index-fHeader.BelongsToIndexRelative];
  end else Result := nil;
end;

function TClipboardItem.GetBody: TClipbrdItemBody;
begin
  {$Message Hint 'We have a problem here, if fHeader.FormatCount is 0'};
  if not fBody.IsLoaded then fBody.LoadFromStream(Parent.body, fHeader);
  Result := fBody;
end;

function TClipboardItem.GetCopiedFromAppInfo: TAppFileInfo;
var
  i: Integer;
begin
  // check if app info for the app, this entry was copied from, is already existing
  if Parent.fAppFileInfoList.Find(Body.CopiedFrom, i) then
    Result := Parent.fAppFileInfoList.Data[i] // then just give it back from the list
  else begin
    Result := TAppFileInfo.Create(Body.CopiedFrom); // load app info
    Parent.fAppFileInfoList.Add(Body.CopiedFrom, Result); // and store it in the list
  end;
end;

function TClipboardItem.GetFormats: TClipbrdItemFormatList;
begin
  Result := Body.Formats;
end;

function TClipboardItem.GetIsSaved: Boolean;
begin
  Result := fHeader.IsSaved and fBody.IsSaved;
end;

function TClipboardItem.GetRenderingInfo: TRenderingInfo;
begin
  if fRenderingInfo = nil then
  begin
    fRenderingInfo := TRenderingInfo.Create(Self);
  end;
  Result := fRenderingInfo;
end;

function TClipboardItem.FindLastItemOfBlock: TClipboardItem;
var
  blockMaster: TClipboardItem;
begin
  Result := nil;

  blockMaster := BelongsTo;
  if blockMaster = nil then exit;

  Result := Self;
  while (Result.NextItem <> nil) and (Result.NextItem.BelongsTo = blockMaster) do
  begin
    Result := Result.NextItem;
  end;

end;

procedure TClipboardItem.SetBelongsTo(aValue: TClipboardItem);
begin
  if aValue <> nil then
  begin
    fHeader.BelongsToIndexRelative := Index - aValue.Index;
    fHeader.Flags := fHeader.Flags + [cbifInCopyBlock];
  end else
  begin
    fHeader.BelongsToIndexRelative := 0;
    fHeader.Flags := fHeader.Flags - [cbifInCopyBlock];
  end;
end;

function TClipboardItem.WriteToClipboard(aFormatList: array of TClipbrdItemFormat): Boolean;
var
  item: TClipbrdItemFormat;
begin
  if TryOpenClipboard(Parent.fClipbrdManager.ThreadWindow) then
  begin
    EmptyClipboard;
    for item in aFormatList do
    begin
      if item = nil then continue;
      item.WriteToClipboard;
    end;
    CloseClipboard;
    Result := true;
  end else Result := false;
end;

function TClipboardItem.WriteToClipboard(aFormatClassList: array of TClipbrdItemFormatClass): Boolean;
var
  itemarray: Array of TClipbrdItemFormat;
  i: Integer;
begin
  SetLength(itemarray, Length(aFormatClassList));
  for i := Low(aFormatClassList) to High(aFormatClassList) do
    itemarray[i] := Formats.FindFormat(aFormatClassList[i]);
  Result := WriteToClipboard(itemarray);
  {$IfDef EnableLog}Logger.Add('TClipboardItem.WriteToClipboard: Result = %', [Result]);{$EndIf}
end;

procedure TClipboardItem.SaveToStreams(aHeader, aData: TStream);
var
  freeSpace: Integer;
begin
  if IsSaved then exit;

  aHeader.Position := TClipboardList.StartOffset + Int64(Index)*Int64(SizeOf(TClipbrdItemHeader.THeaderInfo));

  if (fHeader.DataAddress < 0) then
  begin
    // clipboard item has never been saved before, write it to the end of data file
    aData.Position := aData.Size;
    fHeader.DataAddress := aData.Position;
  end else begin
    // clipboard item has been saved before, check whether there is enough space at its previous location
    if NextItem <> nil then
    begin
      freeSpace := NextItem.Header.DataAddress - fHeader.DataAddress;
      if freeSpace >= fBody.Size then
      begin
        // there is enough free space
        aData.Position := fHeader.DataAddress;
      end else begin
        // there is not enough free space, so we write this at the end of data stream
        aData.Position := aData.Size;
        fHeader.DataAddress := aData.Position;
      end;
    end else begin
      // this is the last item, so we write this at the beginning of last item (and without size check because it is the last item)
      {$Message Warn 'das macht irgendwie keinen Sinn'}
      //aData.Position := aData.Size;
      //fHeader.DataAddress := aData.Position;
      aData.Position := fHeader.DataAddress;
    end;
  end;

  fBody.SaveToStream(aData, fHeader);
  fHeader.SaveToStream(aHeader);
end;

destructor TClipboardItem.Destroy;
begin
  fBody.Free;
  fHeader.Free;

  inherited Destroy;
end;

{ TClipboardList }

constructor TClipboardList.Create(filenameHeader, filenameBody: UnicodeString; aClipbrdManager: TClipboardBase);
begin
  inherited Create;

  fClipbrdManager := aClipbrdManager;
  fAppFileInfoList := TAppFileInfoList.Create;

  if not FileExists(filenameHeader) then
  begin
    header := TFileStream.Create(UTF8Encode(filenameHeader), fmOpenReadWrite or fmCreate);
    Version := CURRENT_CLIPBOARDLIST_VERSION;
    HeaderSize := SizeOf(TClipbrdItemHeader.THeaderInfo);
    if header.Write(Version, SizeOf(Version)) <> SizeOf(Version) then raise Exception.Create('Unable to write to clipbard header file');

    if FileExists(filenameBody) then
    begin
      // Try self repair for next start
      DeleteFile(filenameBody);
      header.Free;
      DeleteFile(filenameHeader);

      {$Message Warn 'Logging or Exception?'}
      raise Exception.Create('Missing clipboard header file');
    end else begin
      body := TFileStream.Create(UTF8Encode(filenameBody), fmOpenReadWrite or fmCreate);
    end;


  end else begin
    header := TFileStream.Create(UTF8Encode(filenameHeader), fmOpenReadWrite);
    header.Position := 0;
    if header.Read(Version, SizeOf(Version)) <> SizeOf(Version) then raise Exception.Create('Unable to read header of clipboard header file');
    case Version of
      CURRENT_CLIPBOARDLIST_VERSION: begin
        HeaderSize := SizeOf(TClipbrdItemHeader.THeaderInfo);
      end;
    end;

    if not FileExists(filenameBody) then
    begin
      header.Free;
      DeleteFile(filenameHeader);
      raise Exception.Create('Missing clipboard data file');
    end else begin
      body := TFileStream.Create(UTF8Encode(filenameBody), fmOpenReadWrite);
      LoadItems(INITIAL_LOAD_ITEM_COUNT);
    end;
  end;
end;

destructor TClipboardList.Destroy;
var
  item: TClipboardItem;
begin
  fAppFileInfoList.Free;
  item := LastItem;
  while (item <> nil) do
  begin
    if not item.IsSaved then item.SaveToStreams(header, body);
    item := item.PreviousItemIfLoaded;
  end;
  inherited Destroy;
end;

procedure TClipboardList.LoadItems(aCount: NativeUInt; StartIndex: NativeUInt);
var
  pos_stop: Int64;
  item: TClipboardItem;
begin
  header.Position := StartOffset + Int64(StartIndex)*Int64(HeaderSize);
  pos_stop := Int64(StartOffset) + Int64(StartIndex)*Int64(HeaderSize) - Int64(aCount)*Int64(HeaderSize);
  repeat
    item := TClipboardItem.CreateFromHeaderStream(header, HeaderSize, Version);
    Self.Insert(item); // Insert respects the given Index of the Item
    header.Position := Max(0, header.Position - Int64(2)*Int64(HeaderSize)); // to go one item backwards, we have to go back the current item first
  until (header.Position <= pos_stop) or (header.Position < StartOffset);
end;

procedure TClipboardList.LoadItems(aCount: NativeUInt);
begin
  if header.Size < HeaderSize then exit; // prevents RangeCheck-Error (StartIndex (NativeUInt) would be = -1)
  LoadItems(aCount, ((header.Size-StartOffset) div HeaderSize) - 1);
end;

procedure TClipboardList.HandleAccessToItemNotYetLoaded(aIndex: NativeUInt);
begin
  if FirstItem <> nil then
    LoadItems(FirstItem.Index - aIndex, FirstItem.Index - 1)
  else
    LoadItems(((header.Size-StartOffset) div HeaderSize) - aIndex);
end;

function TClipboardList.AddFromClipboard: TClipboardItem;
begin
  {$Message Warn 'Was passiert, wenn CB-Entry empty ist'}
  Result := TClipboardItem.CreateFromClipboard;
  Add(Result); // Add sets the item index = last item.index + 1

  // stream positions are set within .SaveToStreams
  //header.Position := header.Size;
  //body.Position := body.Size;
  Result.SaveToStreams(header, body);
end;

procedure TClipboardList.ClearActiveItems;
var
  item: TClipboardItem;
begin
  item := LastItem;
  while (item <> nil) and (cbifActive in item.Header.Flags) do
  begin
    item.Header.Flags := item.Header.Flags - [cbifActive];
    item := item.PreviousItem;
  end;
end;

{$IfDef Debug}
function TClipboardList.CheckHealth: Boolean;
var
  item: TClipboardItem;
begin
  Result := false;
  item := LastItem;
  if item = nil then
  begin
    Result := true;
    exit;
  end;
  while (item.PreviousItem <> nil) do
  begin
    if (item.Index - item.PreviousItem.Index) <> 1 then raise Exception.Create('List failure during backward scanning: invalid index found at ' + IntToStr(item.Index));
    item := item.PreviousItem;
  end;
  if item.Index <> 0 then raise Exception.Create('List failure during backward scanning: List seems to be broken at ' + IntToStr(item.Index));

  item := FirstItem;
  while (item.NextItem <> nil) do
  begin
    if (item.NextItem.Index - item.Index) <> 1 then raise Exception.Create('List failure during forward scanning: invalid index found at ' + IntToStr(item.Index));
    item := item.NextItem;
  end;
  if item.Index <> Count-1 then raise Exception.Create('List failure during forward scanning: List seems to be broken at ' + IntToStr(item.Index));
  Result := true;
end;
{$EndIf}

initialization
  DeviceDependentResources := TFPList.Create;

finalization
  DeviceDependentResources.Free;

end.

