unit Hotkey.List;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SettingsBase, SettingsBaseTypes, Hotkey, Hotkey.Actions, fgl, fpjson, typinfo;

type

  { THotkeyListItem }

  THotkeyListItem = class(TSettingsBaseHotkey)
  private
    fActionID: THotkeyActionID;
    function GetAsJson: TJSONObject;
    procedure SetFromJSON(aValue: TJSONObject);
  public
    property AsJSON: TJSONObject read GetAsJson write SetFromJSON;
    //property IsPrimary: Boolean read fIsPrimary; // muss ermittelt werden, indem geprüft wird ob es das letezte mit dieser ActionID in der Liste ist
    property ActionID: THotkeyActionID read fActionID write fActionID;
  published
  end;

  { THotkeyList }

  THotkeyList = class(TSettingsBase)
  private type TFPHotkeyList = specialize TFPGList<THotkeyListItem>;
  private
    fList: TFPHotkeyList;
    function GetAsJson: TJSONData;
    procedure SetFromJSON(aValue: TJSONData);
  public
    property AsJSON: TJSONData read GetAsJson write SetFromJSON;
    property Items: TFPHotkeyList read fList;

    procedure UpdateAll; override;

    //function GetPrimaryHotkey(aActionID: THotkeyActionID): THotkeyListItem;
    function GetHotkey(aActionID: THotkeyActionID; Index: Integer = 0; CreateIfNotExists: Boolean = true): THotkeyListItem;
    constructor Create(aParent: TSettingsBase = nil); override;
    destructor Destroy; override;
  end;

implementation

{ THotkeyListItem }

function THotkeyListItem.GetAsJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('Action', GetEnumName(TypeInfo(fActionID), Ord(fActionID)));
  Result.Add('Shortcut', Value);
end;

procedure THotkeyListItem.SetFromJSON(aValue: TJSONObject);
begin
  fActionID := THotkeyActionID(GetEnumValue(TypeInfo(fActionID), aValue.Strings['Action']));
  Value := aValue.Objects['Shortcut'];
end;

{ THotkeyList }

function THotkeyList.GetAsJson: TJSONData;
var
  item: THotkeyListItem;
begin
  Result := TJSONArray.Create;
  for item in fList do
  begin
    TJSONArray(Result).Add(item.AsJSON);
  end;
end;

procedure THotkeyList.SetFromJSON(aValue: TJSONData);
var
  i: Integer;
  item: THotkeyListItem;
begin
  {$IfDef Debug}if not (aValue is TJSONArray) then raise Exception.Create('THotkeyList.SetFromJSON got invalid JSONArray:'#13#10+aValue.AsJSON);{$EndIf}
  for item in fList do item.Free;
  fList.Clear;

  for i := 0 to aValue.Count-1 do
  begin
    item := THotkeyListItem.Create(Self);
    item.AsJSON := TJSONArray(aValue).Objects[i];
    fList.Add(item);
  end;
end;

procedure THotkeyList.UpdateAll;
var
  item: THotkeyListItem;
begin
  inherited UpdateAll;
  for item in fList do item.UpdateAll;
end;

function THotkeyList.GetHotkey(aActionID: THotkeyActionID; Index: Integer; CreateIfNotExists: Boolean): THotkeyListItem;
var
  item: THotkeyListItem;
  i: Integer;
begin
  Result := nil;

  i := 0;
  for item in fList do
  begin
    if item.ActionID = aActionID then
    begin
      if i = Index then
      begin
        Result := item;
        exit;
      end else Inc(i);
    end;
  end;

  if (Result = nil) and CreateIfNotExists then
  begin
    Result := THotkeyListItem.Create(Self);
    Result.ActionID := aActionID;
    fList.Add(Result);
  end;
end;

constructor THotkeyList.Create(aParent: TSettingsBase);
begin
  inherited Create(aParent);
  fList := TFPHotkeyList.Create;
end;

destructor THotkeyList.Destroy;
var
  item: THotkeyListItem;
begin
  for item in fList do item.Free;
  fList.Free;
  inherited Destroy;
end;

end.

