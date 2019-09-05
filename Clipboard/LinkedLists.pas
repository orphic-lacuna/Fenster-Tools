unit LinkedLists;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Dialogs;

type
  TLinkedList = class;

  { TLinkedListItem }

  TLinkedListItem = class
  private
    fIndex: NativeUInt;
    function GetPreviousItem: TLinkedListItem;
    procedure UpdateNeighbours(prevItem, nextItem: TLinkedListItem);
  protected
    fParent: TLinkedList;
    fPreviousItem, fNextItem: TLinkedListItem;
  public
    property Index: NativeUInt read fIndex write fIndex;
    property PreviousItem: TLinkedListItem read GetPreviousItem;
    property PreviousItemIfLoaded: TLinkedListItem read fPreviousItem;
    property NextItem: TLinkedListItem read fNextItem;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLinkedList }

  TLinkedList = class
  private
    type
      TLinkedItemList = specialize TFPGList<TLinkedListItem>;
  private
    fItemList: TLinkedItemList;
    function GetCount: NativeUInt;
    function GetFirstItem: TLinkedListItem;
    function GetLastItem: TLinkedListItem;
    { Returns the item according to the index stored in TLinkedListItem, not the index (=position) in fItemList }
    function GetItemByIndex(Index: NativeUInt): TLinkedListItem;
  protected
    { Handles access to items whose header is not loaded yet }
    procedure HandleAccessToItemNotYetLoaded(aIndex: NativeUInt); virtual; abstract;

    { Is called whenever a new item is added or inserted to the list. Override this function in dervived classes. }
    procedure DoNewItemInList(aItem: TLinkedListItem); virtual;
  public
    { Returns the item according to the index stored in TLinkedListItem, not the index (=position) in fItemList }
    property Items[Index: NativeUInt]: TLinkedListItem read GetItemByIndex;
    property Count: NativeUInt read GetCount;
    { FirstItem is always the first item in fItemList. There may be items before this, but they are not loaded yet. }
    property FirstItem: TLinkedListItem read GetFirstItem;
    { LastItem is always the last item in fItemList. It is provided that this item is loaded in every case. }
    property LastItem: TLinkedListItem read GetLastItem;

    { Add sets the index of aItem according to index of last item }
    procedure Add(aItem: TLinkedListItem);
    { Insert respects the index given by aItem, assuming that this index does not exist in list yet }
    procedure Insert(aItem: TLinkedListItem);

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TGLinkedList }

  generic TGLinkedList<T> = class(TLinkedList)
  private
    function GetItemByIndex(Index: NativeUInt): T;
    function GetFirstItem: T;
    function GetLastItem: T;
  public
    property Items[Index: NativeUInt]: T read GetItemByIndex;
    property FirstItem: T read GetFirstItem;
    property LastItem: T read GetLastItem;
  end;

  { TGLinkedListItem }

  generic TGLinkedListItem<T_Parent, T_Item> = class(TLinkedListItem)
  private
    function GetParent: T_Parent; inline;
    function GetPreviousItemIfLoaded: T_Item;
  protected
    function GetNextItem: T_Item; virtual;
    function GetPreviousItem: T_Item; virtual;
  public
    property Parent: T_Parent read GetParent;
    property PreviousItem: T_Item read GetPreviousItem;
    property PreviousItemIfLoaded: T_Item read GetPreviousItemIfLoaded;
    property NextItem: T_Item read GetNextItem;
  end;

implementation

{ TGLinkedListItem }

function TGLinkedListItem.GetParent: T_Parent;
begin
  Result := T_Parent(fParent);
end;

function TGLinkedListItem.GetPreviousItemIfLoaded: T_Item;
begin
  Result := T_Item(inherited PreviousItemIfLoaded);
end;

function TGLinkedListItem.GetNextItem: T_Item;
begin
  Result := T_Item(inherited NextItem);
end;

function TGLinkedListItem.GetPreviousItem: T_Item;
begin
  Result := T_Item(inherited PreviousItem);
end;

{ TGLinkedList }

function TGLinkedList.GetItemByIndex(Index: NativeUInt): T;
begin
  Result := T(inherited Items[Index]);
end;

function TGLinkedList.GetFirstItem: T;
begin
  Result := T(inherited FirstItem);
end;

function TGLinkedList.GetLastItem: T;
begin
  Result := T(inherited LastItem);
end;

{ TLinkedListItem }

constructor TLinkedListItem.Create;
begin
  inherited Create;
end;

procedure TLinkedListItem.UpdateNeighbours(prevItem, nextItem: TLinkedListItem);
begin
  fPreviousItem := prevItem;
  if fPreviousItem <> nil then fPreviousItem.fNextItem := Self;
  fNextItem := nextItem;
  if fNextItem <> nil then fNextItem.fPreviousItem := Self;
end;

function TLinkedListItem.GetPreviousItem: TLinkedListItem;
begin
  if (fPreviousItem = nil) and (fIndex > 0) then fParent.HandleAccessToItemNotYetLoaded(fIndex-1);
  Result := fPreviousItem;
end;

destructor TLinkedListItem.Destroy;
begin
  inherited Destroy;
end;

{ TLinkedList }

constructor TLinkedList.Create;
begin
  inherited Create;
  fItemList := TLinkedItemList.Create;
end;

function TLinkedList.GetItemByIndex(Index: NativeUInt): TLinkedListItem;
var
  item: TLinkedListItem;
begin
  Result := nil;
  if fItemList.Count > 0 then
  begin
    item := fItemList[0];
    if (Index-item.Index < 0) then
      HandleAccessToItemNotYetLoaded(Index);
    {$IfDef Debug} if (Index-fItemList[0].Index >= fItemList.Count) then raise Exception.Create('TLinkedList.GetItemByIndex: Index out of bounds (Index > Count-1)');{$EndIf}
    Result := fItemList[Index-fItemList[0].Index];
  end;
end;

procedure TLinkedList.DoNewItemInList(aItem: TLinkedListItem);
begin
  // nothing to do here
  // overwrite this function in derived classes
end;

function TLinkedList.GetFirstItem: TLinkedListItem;
begin
  if fItemList.Count > 0 then Result := fItemList[0] else Result := nil;
end;

function TLinkedList.GetLastItem: TLinkedListItem;
begin
  if fItemList.Count > 0 then Result := fItemList[fItemList.Count-1] else Result := nil;
end;

function TLinkedList.GetCount: NativeUInt;
begin
  Result := fItemList.Count;
end;

procedure TLinkedList.Add(aItem: TLinkedListItem);
var
  prevItem: TLinkedListItem;
begin
  {$IfDef Debug}if aItem = nil then raise Exception.Create('TLinkedList.Add: invalid item parameter.');{$EndIf}

  if fItemList.Count > 0 then
  begin
    prevItem := fItemList.Items[fItemList.Count-1];
    aItem.Index := prevItem.Index + 1;
  end else begin
    prevItem := nil;
    aItem.Index := 0;
  end;

  aItem.UpdateNeighbours(prevItem, nil);
  aItem.fParent := Self;
  fItemList.Add(aItem);
end;

procedure TLinkedList.Insert(aItem: TLinkedListItem);
var
  item: TLinkedListItem;
begin
  {$IfDef Debug}if aItem = nil then raise Exception.Create('TLinkedList.Add: invalid item parameter.');{$EndIf}

  // test, if it is just an Add-operation
  if (fItemList.Count > 0) and (aItem.Index > fItemList[fItemList.Count-1].Index) then
    Add(aItem) // then just add it
  else begin
    // otherwise insert it
    aItem.fParent := Self;

    item := GetItemByIndex(aItem.Index+1);
    if item = nil then
    begin
      // list must be empty (the case that aItem should be inserted at the end of list is already handled above)
      aItem.UpdateNeighbours(nil, nil);
      fItemList.Add(aItem);
    end else begin
      // list is not empty, everything ok
      aItem.UpdateNeighbours(item.fPreviousItem, item);
      fItemList.Insert(item.Index-fItemList[0].Index, aItem);
    end;
  end;
end;

destructor TLinkedList.Destroy;
begin
  fItemList.Free;
  inherited Destroy;
end;

end.

