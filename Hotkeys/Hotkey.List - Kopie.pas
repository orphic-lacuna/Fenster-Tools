unit Hotkey.List;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ThreadCollections, ChangeHandler, Hotkey, Hotkey.Actions;

type

  { THotkeyListItem }

  THotkeyListItem = class(TThreadCollectionItem)
  private
    fIsPrimary: Boolean;
    fShortcut: TShortcut;
    procedure SetShortcut(aValue: TShortcut);
  public
    property IsPrimary: Boolean read fIsPrimary write fIsPrimary;

    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Shortcut: TShortcut read fShortcut write SetShortcut;
  end;

  { THotkeyList }

  THotkeyList = class(TThreadCollection)
  private
    fChangeHandler: TChangeHandler;
  public
    property ChangeHandler: TChangeHandler read fChangeHandler;

    function GetPrimaryHotkey(aActionID: THotkeyActionID): THotkeyListItem;

    procedure Update(Item: TCollectionItem); override;
    constructor Create(aParent: TChangeHandler); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ THotkeyListItem }

constructor THotkeyListItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

procedure THotkeyListItem.SetShortcut(aValue: TShortcut);
begin
  {$IfDef Debug}if not TThreadCollection(Collection).UpdateCount > 0 then raise Exception.Create('Tried to change property of THotkeyListItem without previous call of BeginUpdate');{$EndIf}
  fShortcut := aValue;
end;

destructor THotkeyListItem.Destroy;
begin
  inherited Destroy;
end;

{ THotkeyList }

procedure THotkeyList.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  fChangeHandler.NotifyChangeHandlers(Self);
end;

constructor THotkeyList.Create(aParent: TChangeHandler);
begin
  inherited Create(THotkeyListItem);
  fChangeHandler := TChangeHandler.Create(aParent);
end;

destructor THotkeyList.Destroy;
begin
  fChangeHandler.Free;
  inherited Destroy;
end;

end.

