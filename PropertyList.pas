unit PropertyList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo;

type

  TPropertyList = class;

  TProperty = record
     Name: String;
     PropertyInfo: TPropInfo;
     PropertyClassType: TClass;
 	end;
  TPropertyArray = Array of TProperty;

  { TPropertyListObjectEnumerator }
  TPropertyListObjectEnumerator = class
  private
    fProperties: TPropertyArray;
    fInstance: TObject;
    fIndex: Integer;
		function GetCurrent: TObject;
  public
    property Current: TObject read GetCurrent;
    function MoveNext: Boolean;
    constructor Create(aPropertyArray: TPropertyArray; aInstance: TObject);
	end;

  { TPropertyList }

  TPropertyList = class(TPersistent)
  private
    fInstance: TObject;
    fProperties: TPropertyArray;
		function GetName(Index: Integer): String;
    function GetPropertyClassType(Index: Integer): TClass;
    function GetCount: Integer;
    procedure SetPropertyValue(Index: Integer; Value: TObject); overload;
    function GetPropertyValue(Index: Integer): TObject; overload;
    procedure SetPropertyValue(Index: String; Value: TObject); overload;
    function GetPropertyValue(Index: String): TObject; overload;
  public
    property Count: Integer read GetCount;
    property Names[Index: Integer]: String read GetName;
    property ValuesByIndex[Index: Integer]: TObject read GetPropertyValue write SetPropertyValue;
    property ValuesByName[Index: String]: TObject read GetPropertyValue write SetPropertyValue;
    property ClassType[Index: Integer]: TClass read GetPropertyClassType;
    function GetEnumerator: TPropertyListObjectEnumerator;

    //function GetEnumerator: TPropertyListNameEnumerator; overload;

{     overload;
    procedure SetValue(Name: String; Value: Variant); overload;
    function GetValue(Index: Integer): Variant; overload;
    function GetValue(Name: String): Variant; overload;}

    constructor Create(aInstance: TObject); virtual;
    destructor Destroy; override;
	end;

//function GetPropName(aClass: TClass; aIndex: Integer): String;

implementation

{function GetPropName(aClass: TClass; aIndex: Integer): String;
var
  propList: PPropList;
	propCount: Integer;
begin
  propCount := GetPropList(aClass, propList);
  if aIndex < propCount then
    Result := propList^[aIndex]^.Name
  else
    raise Exception.Create('Could not find property name with index ' + IntToStr(aIndex) + ' on class ' + aClass.ClassName);

  FreeMem(propList);
end;  }

{ TStringListEnumerator }

function TPropertyListObjectEnumerator.GetCurrent: TObject;
begin
  Result := GetObjectProp(fInstance, @fProperties[fIndex].PropertyInfo);
end;

function TPropertyListObjectEnumerator.MoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex < Length(fProperties);
end;

constructor TPropertyListObjectEnumerator.Create(aPropertyArray: TPropertyArray; aInstance: TObject);
begin
  fIndex := -1;
  fProperties := aPropertyArray;
  fInstance := aInstance;
end;

{ TPropertyList }

function TPropertyList.GetName(Index: Integer): String;
begin
  Result := fProperties[Index].Name;
end;

function TPropertyList.GetPropertyClassType(Index: Integer): TClass;
begin
  Result := fProperties[Index].PropertyClassType;
end;

function TPropertyList.GetCount: Integer;
begin
  Result := Length(fProperties);
end;

function TPropertyList.GetEnumerator: TPropertyListObjectEnumerator;
begin
  Result := TPropertyListObjectEnumerator.Create(fProperties, fInstance);
end;

procedure TPropertyList.SetPropertyValue(Index: Integer; Value: TObject);
begin
  SetObjectProp(fInstance, @fProperties[Index].PropertyInfo, Value);
end;

procedure TPropertyList.SetPropertyValue(Index: String;
  Value: TObject);
begin
  SetObjectProp(fInstance, Index, Value);
end;

function TPropertyList.GetPropertyValue(Index: Integer): TObject;
begin
  Result := GetObjectProp(fInstance, @fProperties[Index].PropertyInfo);
end;

function TPropertyList.GetPropertyValue(Index: String): TObject;
begin
  Result := GetObjectProp(fInstance, Index);
end;

constructor TPropertyList.Create(aInstance: TObject);
var
  propList: PPropList;
  typeData: PTypeData;
	i, propCount: Integer;
begin
  inherited Create;

  fInstance := aInstance;
  // iterating over all published properties
  propCount := GetPropList(fInstance, propList);
  SetLength(fProperties, propCount);
  for i := 0 to propCount - 1 do
  begin
    if propList^[i]^.PropType^.Kind <> tkClass then continue;
    fProperties[i].Name := propList^[i]^.Name;
    fProperties[i].PropertyInfo := propList^[i]^;
    typeData := GetTypeData(propList^[i]^.PropType);
    fProperties[i].PropertyClassType := typeData^.ClassType;
  end;
  FreeMem(propList);
end;

destructor TPropertyList.Destroy;
begin
  SetLength(fProperties, 0);
  inherited Destroy;
end;

end.


