unit SettingsBase;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, typinfo, fgl, StdCtrls, fpjson, Dialogs, ComCtrls, ReplaceList, PropertyList, ChangeHandler, SettingsBoundaries, FensterToolsCommon, ToolBox, fpjsonrtti, Hotkey{$IfDef Logging}, Logging{$EndIf};

type
  TComponentAndNotifyEvent = record
    Component: TComponent;
    NotifyEvent: TNotifyEvent;
    class operator Equal(a,b: TComponentAndNotifyEvent): Boolean;
    class operator Implicit(a: TComponentAndNotifyEvent): TComponent;
    class operator Implicit(a: TComponentAndNotifyEvent): TNotifyEvent;
    class operator Implicit(a: TComponent): TComponentAndNotifyEvent;
	end;

  TSettingsBase = class;
  TComponentList = TFPGList<TComponentAndNotifyEvent>;

  (*
    There are 2 ways of changing a setting:
      -> by GUI, this triggers OnBoundComponentChange which calls ReadValueFromComponent, and propagates the new value to all other components and triggers change handlers of linked objects
      -> by CODE, this triggers SetValue (for the property),  and propagates the new value to every component and triggers change handlers of linked objects
    Both methods handle change-propagation seperately.

    By the way: OnBoundComponentChange should call the original NotifyEvent of its sender-component right at the beginning
                At the moment the NotifyEvent of the sender is not called.
  *)

	{ TSettingsBase }

  TSettingsBase = class(TPersistent{, IChangeHandler})
  private
    fChangeHandler: TChangeHandler;
    fDeferChangeHandler: Boolean;
    fMustUpdate: Boolean;
    fParent: TSettingsBase;
    isUpdating: Boolean;

    fName: String;
    //fIndex: Integer;
    fProperties: TPropertyList;
    function GetName: String;
    procedure SetDeferChangeHandler(aValue: Boolean);

    //procedure BeforeReadObject(Sender : TObject; AObject : TObject; JSON : TJSONObject);
    //procedure AfterReadObject(Sender : TObject; AObject : TObject; JSON : TJSONObject);
  protected
    fBindings: TComponentList;
    fBounds: TBounds;

    procedure SetValue; virtual; // called when a change triggered by code occurs
    procedure OnBoundComponentChange(Sender: TObject); // called when a change triggered by a component occurs

		procedure WriteValueToComponent(comp: TComponent); virtual; abstract;
    function ReadValueFromComponent(comp: TComponent): Boolean; virtual; abstract;

    //procedure NotifyChangeHandlers(aSetting: TSettingsBase);
    procedure BindSubClass(var c: TComponentAndNotifyEvent; comp: TComponent); virtual; abstract;
  public
    property Name: String read GetName;
    property Bounds: TBounds read fBounds write fBounds;
    //property Index: Integer read fIndex;
    property Parent: TSettingsBase read fParent;
    property Properties: TPropertyList read fProperties;
    property ChangeHandler: TChangeHandler read fChangeHandler;// implements IChangeHandler;
    { If DeferChangeHandler is true the execution of ChangeHandlers takes place immediately after a change of the setting. Otherwise the ChangeHandler execution is deferred until DeferChangeHandler is set to false. }
    property DeferChangeHandler: Boolean read fDeferChangeHandler write SetDeferChangeHandler;
    { MustUpdate is true whenever the value of a setting changed, but ChangeHandler execution was deferred. }
    property MustUpdate: Boolean read fMustUpdate;

    function IsDefaultValue: Boolean; virtual; abstract;
    procedure ResetToDefault; virtual; abstract;

    procedure Bind(comp: TComponent); virtual;
    //procedure AddChangeHandler(changeHandler: TNotifyEvent);    // only for simplicity, calls fChangeHandler.Add
    //procedure RemoveChangeHandler(changeHandler: TNotifyEvent); // only for simplicity, calls fChangeHandler.RemoveChangeHandler

    procedure UpdateAll; virtual;

    constructor Create(aParent: TSettingsBase = nil); virtual;
    destructor Destroy; override;
	end;
  TSettingsBaseClass = class of TSettingsBase;

  { TSettingsBaseTop }

  TSettingsBaseTop = class(TSettingsBase)
  private
    function GetAsJson: TJSONObject;
    procedure SetFromJSON(AValue: TJSONObject);
  public
    procedure RestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; Var Handled: Boolean);
    procedure RestorePropertyDefaultValues(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; Var Handled: Boolean);
    procedure StreamProperty(Sender : TObject; AObject : TObject; Info : PPropInfo; var Res : TJSONData);

    property AsJSON: TJSONObject read GetAsJson write SetFromJSON;
  end;

implementation

uses SettingsBaseTypes, SettingsManager, Hotkey.List;

class operator TComponentAndNotifyEvent.Equal(a,b: TComponentAndNotifyEvent): Boolean;
begin
  Result := a.Component = b.Component;
end;

class operator TComponentAndNotifyEvent.Implicit(a: TComponentAndNotifyEvent): TComponent;
begin
  Result := a.Component;
end;

class operator TComponentAndNotifyEvent.Implicit(a: TComponentAndNotifyEvent): TNotifyEvent;
begin
  Result := a.NotifyEvent;
end;

class operator TComponentAndNotifyEvent.Implicit(a: TComponent): TComponentAndNotifyEvent;
begin
  Result.Component := a;
  Result.NotifyEvent := nil;
end;

{operator = (a: TComponentAndNotifyEvent; b: TComponentAndNotifyEvent) res: Boolean;
begin
  res := a.Component = b.Component;
end;}

{ TSettingsBase }

constructor TSettingsBase.Create(aParent: TSettingsBase = nil);
var
  i: Integer;
begin
  inherited Create;

  fMustUpdate := false;
  fDeferChangeHandler := false;
  fParent := aParent;
  fProperties := TPropertyList.Create(Self);
  fBindings := TComponentList.Create;
  if fParent <> nil then
    fChangeHandler := TChangeHandler.Create(fParent.ChangeHandler)
  else
    fChangeHandler := TChangeHandler.Create(nil);

  isUpdating := false;

  // TSettingsBase iteriert über alle published propertries, die ein Objekt sind
  // und instanziiert automatisch die richtige Klasse
  // somit sind sämtliche TObject-abgeleitete Sub-Settings automatisch instanziiert
  for i := 0 to fProperties.Count-1 do
  begin
    if fProperties.ClassType[i].InheritsFrom(TSettingsBase) then
    begin
      //if fProperties.Names[i] = 'Update' then
        //DebugLn('Creating: ' + fProperties.Names[i]);
      fProperties.ValuesByIndex[i] := TSettingsBaseClass(fProperties.ClassType[i]).Create(Self);
      TSettingsBase(fProperties.ValuesByIndex[i]).fName := fProperties.Names[i];

      //TSettingsBase(fProperties.ValuesByIndex[i]).fIndex := i;
    end;
  end;

end;

function TSettingsBase.GetName: String;
var
  p: TSettingsBase;
begin
  Result := fName;
  p := fParent;
  while p <> nil do
  begin
    if p.fName <> '' then
      Result := p.fName + '.' + Result
    else break;
    p := p.Parent;
 	end;
end;

procedure TSettingsBase.SetDeferChangeHandler(aValue: Boolean);
var
  i: Integer;
begin
  // iterate over all published children inheriting from TSettingsBase and call UpdateAll on them
  for i := 0 to fProperties.Count-1 do
  begin
    if fProperties.ClassType[i].InheritsFrom(TSettingsBase) then
      TSettingsBase(fProperties.ValuesByIndex[i]).SetDeferChangeHandler(aValue);
	end;

  // if set to false, execute all pending change handlers here
  if not aValue and fDeferChangeHandler then
  begin
    if fMustUpdate then
    begin
      fMustUpdate := false;

      if (fParent <> nil) and (fParent.DeferChangeHandler) then
      begin
        fParent.fMustUpdate := true;
        // don't notify ChangeHandler parent, because notification of parents is achieved via SettingsBase.SetDeferCangeHandler, so that every parent change handler is only called once and not with each changed child
        fChangeHandler.Notify(Self, false);
      end else
      begin
        // parents want instant notification, so notify change handler and all its parents immediately via change handler parent
        fChangeHandler.Notify(Self, false);
      end;
    end;
  end;

  fDeferChangeHandler := aValue;
end;

  (*
procedure TSettingsBase.BeforeReadObject(Sender: TObject; AObject: TObject;
  JSON: TJSONObject);
begin
  if AObject.InheritsFrom(TClipboardSettings) then
  begin
    TClipboardSettings(AObject).ReplaceList.Lock;
    {$IfDef Logging}Logger.Add('BeforeReadObject: AObject = ' + AObject.ClassName + ', ReplaceList is locked.');{$EndIf}
  end;
end;

procedure TSettingsBase.AfterReadObject(Sender: TObject; AObject: TObject;
  JSON: TJSONObject);
begin
  if AObject.InheritsFrom(TClipboardSettings) then
  begin
    TClipboardSettings(AObject).ReplaceList.Unlock;
    {$IfDef Logging}Logger.Add('AfterReadObject: AObject = ' + AObject.ClassName + ', ReplaceList is unlocked.');{$EndIf}
  end;
end;
     *)
procedure TSettingsBase.Bind(comp: TComponent);
var
  c: TComponentAndNotifyEvent;
begin
  if (fBindings.IndexOf(comp) < 0) then
  begin
    c.Component := comp;
    if comp is TEdit then
    begin
      c.NotifyEvent := (comp as TEdit).OnChange;
      (comp as TEdit).OnChange := Self.OnBoundComponentChange;
  	end else if comp is TCheckbox then
    begin
      c.NotifyEvent := (comp as TCheckBox).OnChange;
      (comp as TCheckBox).OnChange := Self.OnBoundComponentChange;
  	end else if comp is TTrackbar then
    begin
      c.NotifyEvent := (comp as TTrackbar).OnChange;
      (comp as TTrackBar).OnChange := Self.OnBoundComponentChange;
    end else if comp is TComboBox then
    begin
      c.NotifyEvent := (comp as TComboBox).OnChange;
      (comp as TComboBox).OnChange := Self.OnBoundComponentChange;
    end else begin
      BindSubClass(c, comp);
		end;
		fBindings.Add(c);

    // not necessary because it is done via SettingsManager.LoadFromFile
    //WriteValueToComponent(comp);
	end;
end;

{procedure TSettingsBase.AddChangeHandler(changeHandler: TNotifyEvent);
begin
  fChangeHandler.Add(changeHandler);
end;

procedure TSettingsBase.RemoveChangeHandler(changeHandler: TNotifyEvent);
begin
  fChangeHandler.RemoveChangeHandler(changeHandler);
end;}
     (*
procedure TSettingsBase.NotifyChangeHandlers(aSetting: TSettingsBase);
var
  SettingChangedHandler: TOnSettingChanged;
  i: Integer;
begin
  // die Controller-Objekte benachrichtigen, dass die Einstellung geändert wurde

  {$Message Hint 'Why does the following line not work with -O2 or -O3 ?'}
  //for SettingChangedHandler in fChangeHandlers do
  for i := 0 to fChangeHandlers.Count-1 do
  begin
    SettingChangedHandler := fChangeHandlers.Items[i];
    SettingChangedHandler(aSetting);
	end;

  // Notify parent setting about the change
  if fParent <> nil then fParent.NotifyChangeHandlers(aSetting);
end;  *)

procedure TSettingsBase.OnBoundComponentChange(Sender: TObject);
var
  c: TComponentAndNotifyEvent;
begin
  // hier sollte zuerst das NotifyEvent der Sender-Komponente aufgerufen werden
  if isUpdating then exit;

  if ReadValueFromComponent(TComponent(Sender)) then
  begin
    for c in fBindings do
    begin
      if c.Component <> Sender then
      begin
        // die anderen Komponenten müssen den neuen Wert erhalten
        WriteValueToComponent(c);

        // den originalen (im Objektinspektor festgelegten) ComponentChanged-Handler aufrufen (für alle Komponenten)
        if Assigned(c.NotifyEvent) then c.NotifyEvent(c.Component);
			end;
		end;
    if not fDeferChangeHandler then
      fChangeHandler.Notify(Self)
    else
      fMustUpdate := true;
	end;
end;

procedure TSettingsBase.SetValue;
var
  c: TComponentAndNotifyEvent;
begin
  {$IfDef Debug}if isUpdating then raise Exception.Create('Re-entered SetValue during update process of ' + Name);{$EndIf}
  isUpdating := true;

  // alle Komponenten benachrichtigen
  for c in fBindings do
  begin
    WriteValueToComponent(c);
    // den originalen (im Objektinspektor festgelegten) ComponentChanged-Handler aufrufen
    if Assigned(c.NotifyEvent) then
      c.NotifyEvent(c.Component);
	end;

  if not fDeferChangeHandler then
    fChangeHandler.Notify(Self)
  else
    fMustUpdate := true;

  isUpdating := false;
end;

procedure TSettingsBase.UpdateAll;
var
  i: Integer;
begin
  // iterate over all published children inheriting from TSettingsBase and call UpdateAll on them
  for i := 0 to fProperties.Count-1 do
  begin
    if fProperties.ClassType[i].InheritsFrom(TSettingsBase) then
      TSettingsBase(fProperties.ValuesByIndex[i]).UpdateAll
    else if fProperties.ClassType[i].InheritsFrom(TReplaceList) then
      TReplaceList(fProperties.ValuesByIndex[i]).Update(nil);
	end;

  // updates component bindings and triggers change handlers
  SetValue;
end;

destructor TSettingsBase.Destroy;
var
  i: Integer;
begin
  for i := 0 to fProperties.Count-1 do
  begin
    if fProperties.ClassType[i].InheritsFrom(TSettingsBase) then fProperties.ValuesByIndex[i].Free;
  end;
  TryFreeAndNil(fBounds);
  fChangeHandler.Free;
  fBindings.Free;
  fProperties.Free;
  inherited Destroy;
end;

{ TSettingsBaseTop }

function TSettingsBaseTop.GetAsJson: TJSONObject;
var
  js: TJSONStreamer;
begin
  js := TJSONStreamer.Create(nil);
  //js.CollectionToJSON();
  js.OnStreamProperty := Self.StreamProperty;
  //Settings.Clipboard.ReplaceList.Lock; // hier nicht nötig, da nur lesender Zugriff, noch dazu aus GUI-Thread
  try
    Result := js.ObjectToJSON(Self);
  finally
	end;
  //Settings.Clipboard.ReplaceList.Unlock;
  js.Free;
end;

procedure TSettingsBaseTop.SetFromJSON(AValue: TJSONObject);
var
  ds: TJSONDeStreamer;
begin

  ds := TJSONDeStreamer.Create(nil);
  ds.OnRestoreProperty := Self.RestoreProperty;
  //ds.BeforeReadObject := Self.BeforeReadObject;
  //ds.AfterReadObject := Self.AfterReadObject;

  try
    Settings.Clipboard.ReplaceList.BeginUpdate;
    ds.JSONToObject(AValue, Self);
  finally
		Settings.Clipboard.ReplaceList.EndUpdate;
  end;

  ds.Free;
end;

procedure TSettingsBaseTop.RestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
var
  prop: TObject;
begin
  //{$IfDef Logging}Logger.Add('RestoreProperty: ' + AObject.ClassName + '.' + Info^.Name);{$EndIf}
  if Info^.PropType^.Kind = tkClass then
  begin
    prop := GetObjectProp(AObject, Info);
  	Handled := true;
    try
      if prop is TSettingsBaseInteger then (prop as TSettingsBaseInteger).Value := AValue.AsInteger
      else if prop is TSettingsBaseFloat then (prop as TSettingsBaseFloat).Value := AValue.AsFloat
      else if prop is TSettingsBaseBool then (prop as TSettingsBaseBool).Value := AValue.AsBoolean
  	  else if prop is TSettingsBaseString then (prop as TSettingsBaseString).Value := AValue.AsString
      else if prop is TSettingsBaseScrollSettings then
      begin
        if AValue is TJSONObject then
          (prop as TSettingsBaseScrollSettings).Value := TJSONObject(AValue.Clone)
        else
          raise EJSON.Create('TJSONObject expected');
		  end else if prop is THotkeyList then
        (prop as THotkeyList).AsJSON := AValue
          {else if prop is TSettingsBaseHotkey then
        (prop as TSettingsBaseHotkey).Value := TJSONObject(AValue) // the needed converting-operator is defined in Hotkey.pas  }

      else Handled := false;
    except
      if prop is TSettingsBase then TSettingsBase(prop).ResetToDefault;
    end;
	end;
end;

procedure TSettingsBaseTop.RestorePropertyDefaultValues(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
var
  prop: TObject;
begin
  if Info^.PropType^.Kind = tkClass then
  begin
    prop := GetObjectProp(AObject, Info);
  	Handled := true;
    if prop is TSettingsBaseInteger then TSettingsBaseInteger(prop).Bounds := TIntegerBounds.Create(TJSONObject(AValue))
    else if prop is TSettingsBaseBool then TSettingsBaseBool(prop).Bounds := TBooleanBounds.Create(AValue.AsBoolean)
    else if prop is TSettingsBaseFloat then TSettingsBaseFloat(prop).Bounds := TFloatBounds.Create(TJSONObject(AValue))
    else if prop is TSettingsBaseString then TSettingsBaseString(prop).Bounds := TStringBounds.Create(AValue.AsString)
    else if prop is TSettingsBaseScrollSettings then
    begin
      TSettingsBaseScrollSettings(prop).Bounds := TScrollSettingBounds.Create(TJSONObject(AValue.Clone));
		end {else if prop is TSettingsBaseHotkey then
    begin
      TSettingsBaseHotkey(prop).Bounds := THotkeyBounds.Create(TShortcut(TJSONObject(AValue))); // the converting-operator is defined in Hotkey.pas
    end }
    else if prop is THotkeyList then
    begin
      THotkeyList(prop).AsJSON := AValue;
    end else Handled := false;
    if Handled then TSettingsBase(prop).ResetToDefault;
	end;
end;

procedure TSettingsBaseTop.StreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
var
  prop: TObject;
begin
  if Info^.PropType^.Kind = tkClass then
  begin
    prop := GetObjectProp(AObject, Info);
  	//Handled := true;
    // DebugLn(Info^.Name + ': ' + Res.AsJSON);
    //try
    //if prop is TSe then
		//except
      //DebugLn('Res.Free crashed at: ' + Info^.Name + '; Res=' + IntToHex(Integer(Res), 8));
		//end;
		if prop is TSettingsBaseInteger then
    begin
      Res.Free;
      Res := TJSONIntegerNumber.Create((prop as TSettingsBaseInteger).Value)
		end else if prop is TSettingsBaseFloat then
    begin
      Res.Free;
      Res := TJSONFloatNumber.Create((prop as TSettingsBaseFloat).Value)
		end else if prop is TSettingsBaseString then
    begin
      Res.Free;
      Res := TJSONString.Create((prop as TSettingsBaseString).Value)
		end else if prop is TSettingsBaseBool then
    begin
      Res.Free;
      Res := TJSONBoolean.Create((prop as TSettingsBaseBool).Value)
		end else if prop is TSettingsBaseScrollSettings then
    begin
      Res.Free; Res := (prop as TSettingsBaseScrollSettings).Value.Clone;
		end else if prop is THotkeyList then
    begin
      Res.Free;
      Res := THotkeyList(prop).AsJSON;
		end; { else if prop is TSettingsBaseHotkey then
    begin
      Res.Free;
      Res := TSettingsBaseHotkey(prop).Value;
		end; }
	end;
end;


end.

