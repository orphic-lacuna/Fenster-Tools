unit SettingsBaseTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ComCtrls, FensterToolsScrollGraphSettings, SettingsBase, fpjson, SettingsBoundaries, FensterToolsCommon, Hotkey;

type
  { TSettingsBaseTyped }

  generic TSettingsBaseTyped<T> = class(TSettingsBase)
  private
  protected
    fValue: T;
    procedure SetValue(val: T); reintroduce; virtual; // called by Code, calls TSettingsBase.SetValue which propagates the Change
  public
    property Value: T read fValue write SetValue;
  end;

  _SETTINGS_BASE_INTEGER = specialize TSettingsBaseTyped<Integer>;
  _SETTINGS_BASE_FLOAT = specialize TSettingsBaseTyped<Double>;
  _SETTINGS_BASE_BOOL = specialize TSettingsBaseTyped<Boolean>;
  _SETTINGS_BASE_JSONOBJECT = specialize TSettingsBaseTyped<TJSONObject>;
  _SETTINGS_BASE_HOTKEY = specialize TSettingsBaseTyped<Hotkey.TShortcut>;
  _SETTINGS_BASE_STRING = specialize TSettingsBaseTyped<String>;

  { TSettingsBaseInteger }

  TSettingsBaseInteger = class(_SETTINGS_BASE_INTEGER)
  protected
    procedure SetValue(val: Integer); override;
    procedure WriteValueToComponent(comp: TComponent); override; // called by TSettingsBase.SetValue and .OnBoundComponentChange
    function ReadValueFromComponent(comp: TComponent): Boolean; override; // called by Component via TSettingsBase.OnBoundComponentChange
  public
    function IsDefaultValue: Boolean; override;
    procedure ResetToDefault; override;
    function ToString: String; override;
  end;

  { TSettingsBaseFloat }

  TSettingsBaseFloat = class(_SETTINGS_BASE_FLOAT)
  protected
    procedure SetValue(val: Double); override;
    procedure WriteValueToComponent(comp: TComponent); override; // called by TSettingsBase.SetValue and .OnBoundComponentChange
    function ReadValueFromComponent(comp: TComponent): Boolean; override; // called by Component via TSettingsBase.OnBoundComponentChange
  public
    function IsDefaultValue: Boolean; override;
    procedure ResetToDefault; override;
    function ToString: String; override;
  end;

  { TSettingsBaseBool }

  TSettingsBaseBool = class(_SETTINGS_BASE_BOOL)
  protected
    procedure WriteValueToComponent(comp: TComponent); override; // called by TSettingsBase.SetValue and .OnBoundComponentChange
    function ReadValueFromComponent(comp: TComponent): Boolean; override; // called by Component via TSettingsBase.OnBoundComponentChange
  public
    function IsDefaultValue: Boolean; override;
    procedure ResetToDefault; override;
    function ToString: String; override;
  end;

  { TSettingsBaseScrollSettings }

  TSettingsBaseScrollSettings = class(_SETTINGS_BASE_JSONOBJECT)
  protected
    procedure SetValue(val: TJSONObject); override;

    procedure WriteValueToComponent(comp: TComponent); override; // called by TSettingsBase.SetValue and .OnBoundComponentChange
    function ReadValueFromComponent(comp: TComponent): Boolean; override; // called by Component via TSettingsBase.OnBoundComponentChange
    procedure BindSubClass(var c: TComponentAndNotifyEvent; comp: TComponent); override;
  public
    function IsDefaultValue: Boolean; override;
    procedure ResetToDefault; override;
    function ToString: String; override;
    destructor Destroy; override;
  end;

  { TSettingsBaseHotkey }

  TSettingsBaseHotkey = class(_SETTINGS_BASE_HOTKEY)
  protected
    procedure WriteValueToComponent(comp: TComponent); override; // called by TSettingsBase.SetValue and .OnBoundComponentChange
    function ReadValueFromComponent(comp: TComponent): Boolean; override; // called by Component via TSettingsBase.OnBoundComponentChange
    procedure BindSubClass(var c: TComponentAndNotifyEvent; comp: TComponent); override;
  public
    procedure SetForce(aValue: Boolean);
    procedure NotifyAlreadyInUse(HotkeyName: String; ClearHotkey: Boolean = true);
    function IsDefaultValue: Boolean; override;
    procedure ResetToDefault; override;
    function ToString: String; override;
  end;

  { TSettingsBaseString }

  TSettingsBaseString = class(_SETTINGS_BASE_STRING)
  protected
    procedure WriteValueToComponent(comp: TComponent); override; // called by TSettingsBase.SetValue and .OnBoundComponentChange
    function ReadValueFromComponent(comp: TComponent): Boolean; override; // called by Component via TSettingsBase.OnBoundComponentChange
  public
    function IsDefaultValue: Boolean; override;
    procedure ResetToDefault; override;
    function ToString: String; override;
  end;

implementation

{$Region 'TSettingsBaseTyped<T>'}

procedure TSettingsBaseTyped.SetValue(val: T);
begin
  fValue := val;
  inherited SetValue;
end;

{$EndRegion}
{$Region 'TSettingsBaseInteger'}

procedure TSettingsBaseInteger.SetValue(val: Integer);
begin
  {$IfDef Debug}
  if fBounds <> nil then
  begin
  {$EndIf}
    if TIntegerBounds(fBounds).Check(val) then inherited SetValue(val);
  {$IfDef Debug}
  end else begin
    raise Exception.Create('No bounds found in DefaultConfig.json for "' + Self.Name + '"');
    inherited SetValue(val);
  end;
  {$EndIf}
end;

procedure TSettingsBaseInteger.WriteValueToComponent(comp: TComponent); // called by TSettingsBase.
begin
  if comp is TEdit then
  begin
    (comp as TEdit).Text := IntToStr(fValue);
	end else if comp is TTrackBar then
  begin
    (comp as TTrackBar).Position := fValue;
	end else if comp is TComboBox then
  begin
    (comp as TComboBox).ItemIndex := fValue;
  end;
end;

function TSettingsBaseInteger.ReadValueFromComponent(comp: TComponent): Boolean; // called by Component via TSettingsBase.OnBoundComponentChange
begin
  Result := true;
  if comp is TEdit then
  begin
    try
      fValue := StrToInt((comp as TEdit).Text);
		except
      Result := false;
		end;
	end else if comp is TTrackBar then fValue := (comp as TTrackBar).Position
  else if comp is TComboBox then fValue := (comp as TComboBox).ItemIndex;
end;

function TSettingsBaseInteger.IsDefaultValue: Boolean;
begin
  Result := fValue = TIntegerBounds(fBounds).DefaultValue;
end;

procedure TSettingsBaseInteger.ResetToDefault;
begin
  Value := TIntegerBounds(fBounds).DefaultValue;
end;

function TSettingsBaseInteger.ToString: String;
begin
  Result := IntToStr(fValue);
end;

{$EndRegion}
{$Region 'TSettingsBaseFloat'}

procedure TSettingsBaseFloat.SetValue(val: Double);
begin
  if TFloatBounds(fBounds).Check(val) then inherited SetValue(val);
end;

procedure TSettingsBaseFloat.WriteValueToComponent(comp: TComponent); // called by TSettingsBase.
begin
  if comp is TEdit then
  begin
    (comp as TEdit).Text := Self.ToString;
	end;
end;

function TSettingsBaseFloat.ReadValueFromComponent(comp: TComponent): Boolean; // called by Component via TSettingsBase.OnBoundComponentChange
begin
  Result := true;
  if comp is TEdit then
  begin
    try
      fValue := StrToFloat((comp as TEdit).Text);
		except
      Result := false;
		end;
	end;
end;

function TSettingsBaseFloat.IsDefaultValue: Boolean;
begin
  Result := fValue = TIntegerBounds(fBounds).DefaultValue;
end;

procedure TSettingsBaseFloat.ResetToDefault;
begin
  Value := TFloatBounds(fBounds).DefaultValue;
end;

function TSettingsBaseFloat.ToString: String;
begin
  Result := FloatToStr(fValue);
end;

{$EndRegion}
{$Region 'TSettingsBaseBool'}
procedure TSettingsBaseBool.WriteValueToComponent(comp: TComponent); // called by TSettingsBase.
begin
  if comp is TCheckBox then
  begin
    (comp as TCheckBox).Checked := fValue;
	end;
end;

function TSettingsBaseBool.ReadValueFromComponent(comp: TComponent): Boolean; // called by Component via TSettingsBase.OnBoundComponentChange
begin
  if comp is TCheckBox then
  begin
    fValue := (comp as TCheckBox).Checked;
    Result := true;
	end;
end;

function TSettingsBaseBool.IsDefaultValue: Boolean;
begin
  if fBounds = nil then     raise Exception.Create('fBOunds is nil on: ' + Self.Name);
  Result := fValue = TBooleanBounds(fBounds).DefaultValue;
end;

procedure TSettingsBaseBool.ResetToDefault;
begin
  Value := TBooleanBounds(fBounds).DefaultValue;
end;

function TSettingsBaseBool.ToString: String;
begin
  Result := BoolToStr(fValue, true);
end;

{$EndRegion}
{$Region 'TSettingsBaseJSONObject'}

procedure TSettingsBaseScrollSettings.SetValue(val: TJSONObject);
begin
  if fValue = val then exit;
  if TScrollSettingBounds(fBounds).Check(val) then
  begin
    fValue.Free;
    inherited SetValue(val);
  end;
end;

procedure TSettingsBaseScrollSettings.WriteValueToComponent(comp: TComponent); // called by TSettingsBase.
begin
  if comp is TFTScrollGraphSettings then
  begin
    (comp as TFTScrollGraphSettings).GraphValuesJSONData := fValue;
	end;
end;

function TSettingsBaseScrollSettings.ReadValueFromComponent(comp: TComponent): Boolean; // called by Component via TSettingsBase.OnBoundComponentChange
begin
  if comp is TFTScrollGraphSettings then
  begin
    fValue.Free;
    fValue := TJSONObject((comp as TFTScrollGraphSettings).GraphValuesJSONData);
    Result := true;
	end;
end;

procedure TSettingsBaseScrollSettings.BindSubClass(var c: TComponentAndNotifyEvent; comp: TComponent);
begin
  if (comp is TFTScrollGraphSettings) then
  begin
    c.NotifyEvent := (comp as TFTScrollGraphSettings).OnChange;
    (comp as TFTScrollGraphSettings).OnChange := @OnBoundComponentChange;
	end;
end;

function TSettingsBaseScrollSettings.IsDefaultValue: Boolean;
begin
  Result := fValue.Equals(TScrollSettingBounds(fBounds).DefaultValue);
end;

procedure TSettingsBaseScrollSettings.ResetToDefault;
begin
  fValue.Free;
  Value := TJSONObject(TScrollSettingBounds(fBounds).DefaultValue.Clone);
end;

destructor TSettingsBaseScrollSettings.Destroy;
begin
  FreeAndNil(fValue);
  inherited Destroy;
end;

function TSettingsBaseScrollSettings.ToString: String;
begin
  Result := fValue.AsJSON;
end;

{$EndRegion}
{$Region 'TSettingsBaseHotkey'}

procedure TSettingsBaseHotkey.WriteValueToComponent(comp: TComponent); // called by TSettingsBase.
begin
  if comp is THotkey then
  begin
    (comp as THotkey).Hotkey := fValue;
	end;
end;

function TSettingsBaseHotkey.ReadValueFromComponent(comp: TComponent): Boolean; // called by Component via TSettingsBase.OnBoundComponentChange
begin
  if comp is THotkey then
  begin
    fValue.Key := (comp as THotkey).Hotkey.Key;
    fValue.Modifiers := (comp as THotkey).Hotkey.Modifiers;
    Result := true;
	end;
end;

procedure TSettingsBaseHotkey.BindSubClass(var c: TComponentAndNotifyEvent; comp: TComponent);
begin
  if (comp is THotkey) then
  begin
    c.NotifyEvent := (comp as THotkey).OnHotkeyChanged;
    (comp as THotkey).OnHotkeyChanged := @OnBoundComponentChange;
	end;
end;

procedure TSettingsBaseHotkey.SetForce(aValue: Boolean);
begin
  fValue.Force := aValue;
end;

procedure TSettingsBaseHotkey.NotifyAlreadyInUse(HotkeyName: String; ClearHotkey: Boolean = true);
var
  i: Integer;
  c: TComponent;
begin
  if ClearHotkey then fValue.Key := 0; fValue.Modifiers := [];
  for i := 0 to fBindings.Count-1 do
  begin
    c := fBindings.Items[i];
    THotkey(c).NotifyAlreadyInUse(HotkeyName);
 	end;
end;

function TSettingsBaseHotkey.IsDefaultValue: Boolean;
begin
  Result := fValue = THotkeyBounds(fBounds).DefaultValue;
end;

procedure TSettingsBaseHotkey.ResetToDefault;
begin
  Value := THotkeyBounds(fBounds).DefaultValue;
end;

function TSettingsBaseHotkey.ToString: String;
begin
  Result := GetNameOfShortcut(fValue);
  if fValue.Force then Result := Result + ' (! High Priority)';
end;

{$EndRegion}
{$Region 'TSettingsBaseString'}

procedure TSettingsBaseString.WriteValueToComponent(comp: TComponent);
begin
  if comp is TEdit then
  begin
    TEdit(comp).Text := fValue;
  end;
end;

function TSettingsBaseString.ReadValueFromComponent(comp: TComponent): Boolean;
begin
  if comp is TEdit then
  begin
    fValue := TEdit(comp).Text;
    Result := true;
  end else Result := false;
end;

function TSettingsBaseString.IsDefaultValue: Boolean;
begin
  Result := fValue = TStringBounds(fBounds).DefaultValue;
end;

procedure TSettingsBaseString.ResetToDefault;
begin
  Value := TStringBounds(fBounds).DefaultValue;
end;

function TSettingsBaseString.ToString: String;
begin
  Result := fValue;
end;

{$EndRegion}

end.

