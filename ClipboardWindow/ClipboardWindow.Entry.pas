unit ClipboardWindow.Entry;

{$mode objfpc}{$H+}

interface

{$IfDef Logging}
  {.$Define EnableLog}
{$EndIf}

uses
  Classes, SysUtils, ToolBox, Clipboard.List{$IfDef EnableLog}, Logging in '../Logging.pas'{$EndIf}, Clipboard.Item.Header;

type

  _CBWndEntryList = specialize TFPGThreadList<TRenderingInfo>;

  { TCBWndEntryList contains a subset of the items in TClipboardStack. The text is preformatted for
  displaying on screen. }

  TCBWndEntryList = class(_CBWndEntryList)
  private
    fFromIndex, fToIndex: Integer; // store the index of source- and dest-entries from this list
    fItemsBefore, fItemsAfter: Integer; { TODO: determine this dynamically }
    procedure BuildEmptyList;
  public
    property FromIndex: Integer read fFromIndex;
    property ToIndex: Integer read fToIndex;
    procedure RebuildList(aStartItem, aEndItem: TClipboardItem; aFreezedItemIndex: Integer = -1);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

var
  anEmptyRenderingInfo: TEmptyRenderingInfo;

{ TCBWndEntryList }

procedure TCBWndEntryList.RebuildList(aStartItem, aEndItem: TClipboardItem; aFreezedItemIndex: Integer);
var
  freezedItem, startItem, endItem, tmp: TClipboardItem;
  i: Integer;
begin
  if (aStartItem = nil) and (aEndItem = nil) then
  begin
    BuildEmptyList;
    exit;
  end;

  Lock;

  // start < end is up   (clipboard)
  // start > end is down (clipboard)

  // start > end is up   (clipboard window list)
  // start < end is down (clipboard window list)

  // determine startItem and endItem with respect to freezedItem
  if (aFreezedItemIndex >= 0) then
  begin
    freezedItem := Items[aFreezedItemIndex].Parent;

    // check direction
    if aStartItem.Index > aEndItem.Index then
    begin
      // direction is down (go to older item)
      startItem := freezedItem;
{      if freezedItem.Index > aStartItem.Index then
      begin
        startItem := freezedItem;
      end else
      begin
        startItem := aStartItem;
      end;}
    end else
    begin
      // direction is up (go to newer item)
      startItem := freezedItem;
{      if freezedItem.Index < aStartItem.Index then
      begin
        startItem := freezedItem;
      end else
      begin
        startItem := aStartItem;
      end;}
    end;
  end else
  begin
    startItem := aStartItem;
  end;
  endItem := aEndItem;

  // now we have startItem and endItem with respect to freezedItem, now let's sort

//  if startItem.Index < aEndItem.Index then
  if startItem.Index < endItem.Index then
  begin
    tmp := startItem;
    startItem := endItem;
    endItem := tmp;
  end;

  // startItem is now the item with greatest id

  // at this point we know start and end item, now let's build the list

  // go back fItemsBefore
  i := 0;
  repeat
    if startItem.NextItem <> nil then
    begin
      startItem := startItem.NextItem;
      Inc(i);
    end;
  until (i >= fItemsBefore) or (startItem.NextItem = nil);

  // save indices depending on direction of animation
  if aStartItem.Index < aEndItem.Index then
    fToIndex := i
  else
    fFromIndex := i;

  Clear;

  //i := 0;
  while startItem.Index > endItem.Index do
  begin
    Add(startItem.RenderingInfo);      // add all rendering infos of the items to the list until we reach aEndItem
    startItem := startItem.PreviousItem;
    //Inc(i);
  end;

{  repeat
    Add(startItem.RenderingInfo);      // add all rendering infos of the items to the list until we reach aEndItem
    startItem := startItem.PreviousItem;
    //Inc(i);
  until startItem = endItem;}

  // save indices depending on direction of animation
  if aStartItem.Index < aEndItem.Index then
    fFromIndex := Count
  else
    fToIndex := Count;

  for i := 0 to fItemsAfter do // start from 0 because the EndItem has not been added yet
  begin
    Add(startItem.RenderingInfo);     // and add all the items to the list
    if (startItem.PreviousItem <> nil) then
    begin
      if (cbifActive in startItem.PreviousItem.Header.Flags) then
        startItem := startItem.PreviousItem
      else break;
    end
    else
      break;
  end;

  {$IfDef EnableLog}Logger.Add('TCBWndEntryList.RebuildList: List dump following');
  for i := 0 to Count-1 do
  begin
    if Items[i].Parent = freezedItem then
      Logger.Add(IntToStr(i)+' (freezed): ' + Items[i].Text)
    else if Items[i].Parent = aStartItem then
      Logger.Add(IntToStr(i)+' (start item): ' + Items[i].Text)
    else if Items[i].Parent = aEndItem then
      Logger.Add(IntToStr(i)+' (end item): ' + Items[i].Text)
    else
      Logger.Add(IntToStr(i)+': ' + Items[i].Text)
  end;
  {$EndIf}

  Unlock;
end;

procedure TCBWndEntryList.BuildEmptyList;
begin
  Lock;
  Clear;
  if anEmptyRenderingInfo = nil then anEmptyRenderingInfo := TEmptyRenderingInfo.Create;
  Add(anEmptyRenderingInfo);
  Unlock;
end;

constructor TCBWndEntryList.Create;
begin
  inherited Create;
  fItemsBefore := 3;
  fItemsAfter := 3;
end;

destructor TCBWndEntryList.Destroy;
begin
  TryFreeAndNil(anEmptyRenderingInfo);
  inherited Destroy;
end;

end.

