unit AppFileInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, fgl, ToolBox, Graphics, shellapi, Dialogs;

type

  { TAppFileInfo }

  TAppFileInfo = class
  private
    fFilename: UnicodeString;
    fData: Pointer;
    fIcon: TIcon;
    function GetProductName: UnicodeString;
    procedure LoadIcon;
    procedure LoadVersionInfo;
  public
    property ProductName: UnicodeString read GetProductName;
    property Icon: TIcon read fIcon;

    constructor Create(aFilename: UnicodeString);
    destructor Destroy; override;
  end;

  { TAppFileInfoList }

  TAppFileInfoList = class(specialize TFPGMap<UnicodeString, TAppFileInfo>)
  public
    constructor Create;
  end;

implementation

function ImageList_GetIcon(himl: HIMAGELIST; i: Integer; flags: UINT): HICON; stdcall; external comctl32;

{ TAppFileInfoList }

constructor TAppFileInfoList.Create;
begin
  inherited Create;
  Sorted := true;
end;

{ TAppFileInfo }

constructor TAppFileInfo.Create(aFilename: UnicodeString);
begin
  fFilename := aFilename;
  LoadVersionInfo;
  LoadIcon;
end;

procedure TAppFileInfo.LoadVersionInfo;
var
  ver_size: DWORD;
begin
  ver_size := GetFileVersionInfoSizeW(PWideChar(fFilename), nil);
  if ver_size > 0 then
  begin
    fData := GetMem(ver_size+1);
    if not GetFileVersionInfoW(PWideChar(fFilename), 0, ver_size, fData) then
    begin
      Freemem(fData);
      fData := nil;
    end;
  end;
end;

procedure TAppFileInfo.LoadIcon;
var
  fileinfo: _SHFILEINFOW;
  il: Pointer;
const
  SHIL_JUMBO = $04;
  IID_IImageList: TGuid = '{46EB5926-582E-4017-9FDF-E8998DAA0950}';
  ILD_TRANSPARENT = $01;
begin
  SHGetFileInfoW(PWideChar(fFilename), 0, @fileinfo, SizeOf(fileinfo), SHGFI_SYSICONINDEX);
  SHGetImageList(SHIL_JUMBO, IID_IImageList, @il);
  TryFreeAndNil(fIcon);
  fIcon := TIcon.Create;
  fIcon.Handle := ImageList_GetIcon(HIMAGELIST(il), fileinfo.iIcon, ILD_TRANSPARENT);
end;

function TAppFileInfo.GetProductName: UnicodeString;
var
  str: UnicodeString;
  str2: Pointer;
  len: UINT;
begin
  if fData <> nil then
  begin
    str := '\StringFileInfo\000004B0\ProductName';
    if VerQueryValueW(fData, LPWSTR(str), str2, len) then
    begin
      SetLength(Result, len);
      CopyMemory(@Result[1], str2, len*SizeOf(UnicodeChar));
    end else Result := ExtractFileNameWithoutExt(ExtractFilename(fFilename));
  end else Result := ExtractFileNameWithoutExt(ExtractFilename(fFilename));
end;

destructor TAppFileInfo.Destroy;
begin
  if fData <> nil then
  begin
    Freemem(fData);
    fData := nil;
  end;
  TryFreeAndNil(fIcon);
  inherited Destroy;
end;

end.

