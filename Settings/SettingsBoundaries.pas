unit SettingsBoundaries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, jsonparser, Hotkey;

type
  TIntegerList = specialize TFPGList<Integer>;

type

  { TJSONDataComparer }

  TJSONDataComparer = class helper for TJSONData
  public
    function Equals(aJSONData: TJSONData): boolean;
  end;

type

  { TBounds }

  TBounds = class
  public
    constructor Create(aBoundsInfos: TJSONData); virtual; abstract;
  end;

  generic TGBounds<T> = class(TBounds)
  private
    fDefaultValue: T;
  public
    property DefaultValue: T read fDefaultValue;

    function Check(aValue: T): Boolean; virtual;
    constructor Create(aDefaultValue: T); overload; virtual;
  end;

  generic TGJSONBounds<T> = class(specialize TGBounds<T> )
  protected
    function Parse(aValue: String): TJSONObject;
  end;

  { TIntegerBounds }

  TIntegerBoundMode = (ibmMinMax, ibmAllowedValues);
  _INTEGER_BOUNDS = specialize TGBounds<Integer>;
  TIntegerBounds = class(_INTEGER_BOUNDS)
  private
    fMin, fMax: Integer;
    fAllowedValues: TIntegerList;

    function GetBoundMode: TIntegerBoundMode;
  public
    property Mode: TIntegerBoundMode read GetBoundMode;
    property Min: Integer read fMin;
    property Max: Integer read fMax;
    property AllowedValues: TIntegerList read fAllowedValues;

    function Check(aValue: Integer): Boolean; override;

    constructor Create(aBoundsInfos: TJSONData); override;
    destructor Destroy; override;
  end;

  { TFloatBounds }

  _FLOAT_BOUNDS = specialize TGBounds<Double>;
  TFloatBounds = class(_FLOAT_BOUNDS)
  private
    fMin, fMax: Double;
    CheckLowerBound, CheckUpperBound: Boolean;
  public
    property Min: Double read fMin;
    property Max: Double read fMax;

    function Check(aValue: Double): Boolean; override;

    constructor Create(aBoundsInfos: TJSONData); override;
  end;

  { TBooleanBounds }

  _BOOLEAN_BOUNDS = specialize TGBounds<Boolean>;
  TBooleanBounds = class(_BOOLEAN_BOUNDS);

  { TStringBounds }

  _STRING_BOUNDS = specialize TGBounds<String>;
  TStringBounds = class(_STRING_BOUNDS);

  { TJSONBounds }
  {
  TJSONBounds = class(_GSTRING_BOUNDS)
  private
    function Parse(aValue: String): TJSONObject;
  public
    //function Check(aValue: String): Boolean; override;

  end;     }

  { TScrollSettingBounds }

  _SCROLL_SETTING_BOUNDS = specialize TGJSONBounds<TJSONObject>;
  TScrollSettingBounds = class(_SCROLL_SETTING_BOUNDS)
  public
    function Check(aValue: String): Boolean; overload;
    function Check(aValue: TJSONObject): Boolean; override;
    destructor Destroy; override;
  end;

  { THotkeyBounds }

  _HOTKEY_BOUNDS = specialize TGJSONBounds<TShortcut>;
  THotkeyBounds = class(_HOTKEY_BOUNDS)
  public
    function Check(aValue: String): Boolean; overload;
  end;

implementation

uses Unit1;

{ TJSONDataComparer }

function TJSONDataComparer.Equals(aJSONData: TJSONData): boolean;
var
  elem: TJSONEnum;
  value_2: TJSONData;
  i: Integer;
begin
  Result := false;
  if Self.JSONType <> aJSONData.JSONType then exit;

  if Self is TJSONObject then
  begin
    for elem in Self do
    begin
      value_2 := TJSONObject(aJSONData).Find(elem.Key, TJSONObject(Self).Types[elem.Key]);
      if value_2 <> nil then
      begin
        if not elem.Value.Equals(value_2) then exit;
      end else exit;
    end;
  end else if Self is TJSONArray then
  begin
    for i := 0 to Self.Count-1 do
    begin
      if not i >= TJSONArray(aJSONData).Count then
      begin
        if not Self.Items[i].Equals(TJSONArray(aJSONData).Items[i]) then exit;
      end else exit;
    end;
  end else if Self is TJSONString then
  begin
    if not (Self.AsString = aJSONData.AsString) then exit;
  end else if Self is TJSONIntegerNumber then
  begin
    if not (Self.AsInteger = aJSONData.AsInteger) then exit;
  end else if Self is TJSONInt64Number then
  begin
    if not (Self.AsInt64 = aJSONData.AsInt64) then exit;
  end else if Self is TJSONFloatNumber then
  begin
    if not (Self.AsFloat = aJSONData.AsFloat) then exit;
  end  else if Self is TJSONBoolean then
  begin
    if not (Self.AsBoolean = aJSONData.AsBoolean) then exit;
  end else Result := true;
end;

{ TBounds }

function TGBounds.Check(aValue: T): Boolean;
begin
  Result := true;
end;

constructor TGBounds.Create(aDefaultValue: T);
begin
  fDefaultValue := aDefaultValue;
end;

{ TIntegerBounds }

constructor TIntegerBounds.Create(aBoundsInfos: TJSONData);
var
  i: Integer;
begin
  {$IfDef Debug}if not (aBoundsInfos is TJSONObject) then raise Exception.Create('Invalid default config (TIntegerBounds.Create)');{$EndIf}

  fDefaultValue := TJSONObject(aBoundsInfos).Integers['Default'];
  if (TJSONObject(aBoundsInfos).Find('Min', jtNumber) <> nil) and (TJSONObject(aBoundsInfos).Find('Max', jtNumber) <> nil) then
  begin
    fMin := TJSONObject(aBoundsInfos).Integers['Min'];
    fMax := TJSONObject(aBoundsInfos).Integers['Max'];
  end else if (TJSONObject(aBoundsInfos).Find('AllowedValues', jtArray) <> nil) then
  begin
    fAllowedValues := TIntegerList.Create;
    for i := 0 to TJSONObject(aBoundsInfos).Arrays['AllowedValues'].Count-1 do
    begin
      fAllowedValues.Add(TJSONObject(aBoundsInfos).Arrays['AllowedValues'].Integers[i]);
    end;
  end;
end;

function TIntegerBounds.GetBoundMode: TIntegerBoundMode;
begin
  if fAllowedValues = nil then
    Result := ibmMinMax
  else
    Result := ibmAllowedValues;
end;

function TIntegerBounds.Check(aValue: Integer): Boolean;
begin
  //inherited Check;
  if fAllowedValues = nil then
  begin
    Result := (aValue >= fMin) and (aValue <= fMax);
  end else begin
    Result := (fAllowedValues.IndexOf(aValue) > -1);
  end;
end;

destructor TIntegerBounds.Destroy;
begin
  if fAllowedValues <> nil then FreeAndNil(fAllowedValues);
  inherited Destroy;
end;

{ TFloatBounds }

constructor TFloatBounds.Create(aBoundsInfos: TJSONData);
var
  i: Integer;
begin
  {$IfDef Debug}if not (aBoundsInfos is TJSONObject) then raise Exception.Create('Invalid default config (TFloatBounds.Create)');{$EndIf}

  fDefaultValue := TJSONObject(aBoundsInfos).Floats['Default'];
  CheckLowerBound := (TJSONObject(aBoundsInfos).Find('Min', jtNumber) <> nil);
  CheckUpperBound := (TJSONObject(aBoundsInfos).Find('Max', jtNumber) <> nil);
  if CheckLowerBound then fMin := TJSONObject(aBoundsInfos).Floats['Min'];
  if CheckUpperBound then fMax := TJSONObject(aBoundsInfos).Floats['Max'];
end;

function TFloatBounds.Check(aValue: Double): Boolean;
begin
  Result := ((not CheckLowerBound) or (aValue >= fMin)) and ((not CheckUpperBound) or (aValue <= fMax));
end;

{ TJSONBounds }

function TGJSONBounds.Parse(aValue: String): TJSONObject;
var
  p: TJSONParser;
  tmp: TJSONData;
begin
  Result := nil;
  try
    p := TJSONParser.Create(aValue);
    tmp := p.Parse;
    if tmp is TJSONObject then Result := TJSONObject(tmp);
  finally
    p.Free;
  end;
end;

     {
function TJSONBounds.Check(aValue: String): Boolean;
var
  j: TJSONObject;
begin
  j := Parse(aValue);
  Result := j <> nil;
  if Result then j.Free;
end;
      }

{ TScrollSettingBounds }

function TScrollSettingBounds.Check(aValue: TJSONObject): Boolean;
var
  elem: TJSONArray;
  i: Integer;
  point: TJSONObject;
  x, y: TJSONNumber;
begin
  Result := false;
  if aValue = nil then exit;

  elem := TJSONArray(aValue.Find('Upscaling', jtArray));
  if elem <> nil then
  begin
    Result := true;
    for i := 0 to elem.Count-1 do
    begin
      point := elem.Objects[i];
      x := TJSONNumber(point.Find('X', jtNumber)); y := TJSONNumber(point.Find('Y', jtNumber));
      if (x = nil) or (y = nil) then
      begin
        Result := false;
        break;
      end else
      begin
        if (x.AsFloat < 0) or (x.AsFloat > frm_Settings.FTScrollGraphSettings1.ScrollSpeedMax) or
        (y.AsFloat < frm_Settings.FTScrollGraphSettings1.UpscalingFactorMin) or (y.AsFloat > frm_Settings.FTScrollGraphSettings1.UpscalingFactorMax) then
        begin
          Result := false;
          break;
        end;
      end;
    end;

    elem := TJSONArray(aValue.Find('Deceleration', jtArray));
    if (elem <> nil) and Result then
    begin
      for i := 0 to elem.Count-1 do
      begin
        point := elem.Objects[i];
        x := TJSONNumber(point.Find('X', jtNumber)); y := TJSONNumber(point.Find('Y', jtNumber));
        if (x = nil) or (y = nil) then
        begin
          Result := false;
          break;
        end else
        begin
          if (x.AsFloat < 0) or (x.AsFloat > frm_Settings.FTScrollGraphSettings1.ScrollSpeedMax) or
          (y.AsFloat < frm_Settings.FTScrollGraphSettings1.DecelerationFactorMin) or (y.AsFloat > frm_Settings.FTScrollGraphSettings1.DecelerationFactorMax) then
          begin
            Result := false;
            break;
          end;
        end;
      end;
    end;
  end;
end;

function TScrollSettingBounds.Check(aValue: String): Boolean;
var
  tmp_json: TJSONObject;
begin
  Result := false;
  try
    tmp_json := Parse(aValue);
  except
  end;

  if tmp_json <> nil then
  begin
    Result := Check(tmp_json);
    tmp_json.Free;
  end;
end;

destructor TScrollSettingBounds.Destroy;
begin
  fDefaultValue.Free;
  inherited Destroy;
end;

{ THotkeyBounds }

function THotkeyBounds.Check(aValue: String): Boolean;
var
  tmp_json: TJSONObject;
begin
  Result := false;
  try
    tmp_json := Parse(aValue);
    JSONToShortcut(tmp_json);
    Result := true;
  finally
    if tmp_json <> nil then tmp_json.Free;
  end;
end;

end.

