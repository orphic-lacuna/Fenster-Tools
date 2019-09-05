unit Clipboard.Manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Clipboard.Notifications, Clipboard.List, ExtendedWinAPI{$IfDef Logging}, Logging{$EndIf}, ClipboardWindow, ClipboardStatusWindow, AppFileInfo;

type

  TCopyMode = (cmNormal, cmBlockCopy, cmBlockPaste);
  TBlockDirection = (bdAsCopied, bdReversed);


  { TClipboardManager }

  TClipboardManager = class(TClipboardBase)

  private
    fHistory: TClipboardList;
    fActiveItem: TClipboardItem;
    fOnCopyModeChanged: TNotifyEvent;
    fOnBlockDirectionChanged: TNotifyEvent;
    fWindow: TClipboardWindow;
    fStatusWindow: TClipboardStatusWindow;

    fCopyMode: TCopyMode;
    //fIndexOfFirstItemInCurrentBlock: NativeInt;
    fFirstItemInBlock: TClipboardItem;
    fBlockDirection: TBlockDirection;


    procedure NavigateDown;
    procedure NavigateUp;
    procedure ChangeBlockCopyMode;
    procedure SetCopyMode(aValue: TCopyMode);
    procedure SetBlockDirection(aValue: TBlockDirection);

    procedure DoOnCopyModeChanged;
    procedure DoOnBlockDirectionChanged;
  protected
    procedure ClipboardChanged; override;
    procedure ClipboardPasted(FormatType: Cardinal; PID: Cardinal); override;

    procedure ThreadInternalCreate; override;
    procedure ThreadInternalDestroy; override;

    procedure OnSettingChanged(aSetting: TObject); override;
    procedure RegisterHotkeys;
    procedure UnregisterHotkeys;
  public
    property OnCopyModeChanged: TNotifyEvent read fOnCopyModeChanged write fOnCopyModeChanged;
    property OnBlockDirectionChanged: TNotifyEvent read fOnBlockDirectionChanged write fOnBlockDirectionChanged;
    property History: TClipboardList read fHistory;
    property CopyMode: TCopyMode read fCopyMode write SetCopyMode;
    property BlockDirection: TBlockDirection read fBlockDirection write SetBlockDirection;

    procedure ClearClipboard; override;

    constructor Create;
    destructor Destroy; override;
  end;

var
  ClipbrdManager: TClipboardManager;

implementation

uses FensterToolsCommon, SettingsManager, Hotkey.Actions, Clipboard.Formats, Clipboard.Item.Header, LivePatching, ApplicationRestart;

{ TClipboardManager }

procedure TClipboardManager.NavigateDown;
begin
  {$IfDef Logging}Logger.Add('TClipboardManager.NavigateDown: navigating down');{$EndIf}
  if fActiveItem <> nil then
  begin
    if (fActiveItem.PreviousItem <> nil) and (cbifActive in fActiveItem.PreviousItem.Header.Flags) then
    begin
      fWindow.Animate(fActiveItem, fActiveItem.PreviousItem);
      fActiveItem := fActiveItem.PreviousItem;
      if fActiveItem.WriteToClipboard([TClipboardItemFormatString]) then
        {$IfDef Logging}Logger.Add('Content written to clipboard'){$EndIf}
      else
        {$IfDef Logging}Logger.Add('No string format found');{$EndIf}
    end else begin
      {$IfDef Logging}Logger.Add('No previous item found, displaying old item');{$EndIf}
      fWindow.Animate(fActiveItem, fActiveItem);
    end;
  end else fWindow.Animate(nil, nil);
end;

procedure TClipboardManager.NavigateUp;
begin
  {$IfDef Logging}Logger.Add('TClipboardManager.NavigateUp: navigating up');{$EndIf}
  if fActiveItem <> nil then
  begin
    if (fActiveItem.NextItem <> nil) and (cbifActive in fActiveItem.NextItem.Header.Flags) then
    begin
      fWindow.Animate(fActiveItem, fActiveItem.NextItem);
      fActiveItem := fActiveItem.NextItem;
      if fActiveItem.WriteToClipboard([TClipboardItemFormatString]) then
        {$IfDef Logging}Logger.Add('Content written to clipboard'){$EndIf}
      else
        {$IfDef Logging}Logger.Add('No string format found');{$EndIf}
    end else
    begin
      {$IfDef Logging}Logger.Add('No next item found, displaying old item');{$EndIf}
      fWindow.Animate(fActiveItem, fActiveItem);
    end;
  end else fWindow.Animate(nil, nil);
end;

procedure TClipboardManager.ChangeBlockCopyMode;
begin
  case CopyMode of
    cmNormal:
      CopyMode := cmBlockCopy;
    cmBlockCopy:
      CopyMode := cmBlockPaste;
    cmBlockPaste:
      CopyMode := cmNormal;
  end;
end;

procedure TClipboardManager.SetCopyMode(aValue: TCopyMode);
begin
  if fCopyMode = aValue then Exit;
  fCopyMode := aValue;
  case fCopyMode of
    cmNormal:
      begin
        {$IfDef Logging}Logger.Add('TClipboardManager.ChangeBlockCopyMode: switched CopyMode to cmNormal');{$EndIf}
      end;
    cmBlockCopy:
      begin
        fFirstItemInBlock := nil;
        {$IfDef Logging}Logger.Add('TClipboardManager.ChangeBlockCopyMode: switched CopyMode to cmBlockCopy');{$EndIf}
      end;
    cmBlockPaste:
      begin
        if fBlockDirection = bdAsCopied then
        begin
          fActiveItem := fFirstItemInBlock;
          fActiveItem.WriteToClipboard([TClipboardItemFormatString]);
        end;
        {$IfDef Logging}Logger.Add('TClipboardManager.ChangeBlockCopyMode: switched CopyMode to cmBlockPaste');{$EndIf}
      end;
  end;

  DoOnCopyModeChanged;
end;

procedure TClipboardManager.SetBlockDirection(aValue: TBlockDirection);
begin
  if fBlockDirection = aValue then Exit; // this line is important for clBlockCopy, fFirstItemInBlock = nil, BlockDirection = bdReversed
  fBlockDirection := aValue;
  case fCopyMode of
    cmNormal:
      begin
        {$IfDef Logging}Logger.Add('TClipboardManager.SetBlockDirection: cmNormal, nothing to do');{$EndIf}
      end;
    cmBlockCopy:
      begin
        if fFirstItemInBlock = nil then
        begin
          {$IfDef Logging}Logger.Add('TClipboardManager.SetBlockDirection: cmBlockCopy, nothing copied yet, nothing to do');{$EndIf}
        end else
        begin
          if fBlockDirection = bdAsCopied then
          begin
            fActiveItem := fFirstItemInBlock;
            fFirstItemInBlock.WriteToClipboard([TClipboardItemFormatString]);
            {$IfDef Logging}Logger.Add('TClipboardManager.SetBlockDirection: cmBlockCopy+bdAsCopied, wrote first item of block to clipboard');{$EndIf}
          end else
          begin
            // previous block direction was bdAsCopied -> last copied item must be written back to clipboard
            fActiveItem := fFirstItemInBlock.FindLastItemOfBlock;
            fActiveItem.WriteToClipboard([TClipboardItemFormatString]);
          end;
        end;
      end;
    cmBlockPaste:
      begin
        {$IfDef Logging}Logger.Add('TClipboardManager.SetBlockDirection: CopyMode is cmBlockPaste');{$EndIf}

        if fBlockDirection = bdAsCopied then
        begin
          fActiveItem := fFirstItemInBlock;
          fActiveItem.WriteToClipboard([TClipboardItemFormatString]);
        end;
        {$IfDef Logging}Logger.Add('TClipboardManager.SetBlockDirection: switched CopyMode to cmBlockPaste');{$EndIf}
      end;
  end;

  DoOnBlockDirectionChanged;
end;

procedure TClipboardManager.DoOnCopyModeChanged;
begin
  if IsRunInThread then
    DoCallbackProc(@DoOnCopyModeChanged)
  else begin
    if Assigned(fOnCopyModeChanged) then fOnCopyModeChanged(Self);
  end;
end;

procedure TClipboardManager.DoOnBlockDirectionChanged;
begin
  if IsRunInThread then
    DoCallbackProc(@DoOnBlockDirectionChanged)
  else begin
    if Assigned(fOnBlockDirectionChanged) then fOnBlockDirectionChanged(Self);
  end;
end;

procedure TClipboardManager.ClipboardChanged;
var
  item: TClipboardItem;
begin
  item := fHistory.AddFromClipboard;
  if item <> nil then
  begin
    {$IfDef Logging}Logger.Add('TClipboardManager.ClipboardChanged: Clipboard captured');{$EndIf}
    case CopyMode of
      cmNormal:
        begin
          fActiveItem := item;
        end;
      cmBlockCopy:
        begin
          // determine first item of copy block
          if fFirstItemInBlock = nil then
          begin
            // save first item in the block
            fFirstItemInBlock := item;
            fFirstItemInBlock.BelongsTo := fFirstItemInBlock; // associate the item to itself to mark this item as start item of a copy block
          end else
          begin
            // attach all following items to block owner (first item of a block)
            item.BelongsTo := fFirstItemInBlock;
          end;

          // determine according to block direction what should happen next (what the active item is)
          if fBlockDirection = bdAsCopied then
          begin
            fActiveItem := fFirstItemInBlock;
          end else if fBlockDirection = bdReversed then
          begin
            fActiveItem := item;
          end;
        end;
      cmBlockPaste:
        begin
          { TODO: unexpected user behaviour. What should happen if users copies in block paste mode? }
        end;
    end;
  end else
  begin
    {$IfDef Logging}Logger.Add('TClipboardManager.ClipboardChanged: Clipboard not captured (unsupported format?)');{$EndIf}
  end;
end;

procedure TClipboardManager.ClipboardPasted(FormatType: Cardinal; PID: Cardinal);
begin
  {$IfDef Logging}Logger.Add('TClipboardManager.ClipboardPasted');{$EndIf}
  if fActiveItem <> nil then
  begin
    fActiveItem.Header.IncPastedCount;
    case CopyMode of
      cmNormal:
        begin
          // nothing special here
        end;
      cmBlockCopy:
        begin
          { TODO: unexpected user behaviour. What should happen if users pastes in block copy mode? }
        end;
      cmBlockPaste:
        begin
          if fBlockDirection = bdAsCopied then
          begin
            if (fActiveItem.NextItem <> nil) and ((fActiveItem.NextItem.BelongsTo = fFirstItemInBlock) or (fActiveItem.NextItem = fFirstItemInBlock)) then
            begin
              fActiveItem := fActiveItem.NextItem;
              if fActiveItem.WriteToClipboard([TClipboardItemFormatString]) then
                {$IfDef Logging}Logger.Add('NextItem written to clipboard'){$EndIf}
              else
                {$IfDef Logging}Logger.Add('NextItem could not be written to clipboard: No string format found');{$EndIf}
            end else
            begin
              // seems we have reached the end of our copy block -> quit block paste mode
              CopyMode := cmNormal;
            end;
          end else if fBlockDirection = bdReversed then
          begin
            if (fActiveItem.PreviousItem <> nil) and ((fActiveItem.PreviousItem.BelongsTo = fFirstItemInBlock) or (fActiveItem.PreviousItem = fFirstItemInBlock)) then
            begin
              fActiveItem := fActiveItem.PreviousItem;
              if fActiveItem.WriteToClipboard([TClipboardItemFormatString]) then
                {$IfDef Logging}Logger.Add('PreviousItem written to clipboard'){$EndIf}
              else
                {$IfDef Logging}Logger.Add('PreviousItem could not be written to clipboard: No string format found');{$EndIf}
            end else
            begin
              // seems we have reached the end of our copy block -> quit block paste mode
              CopyMode := cmNormal;
            end;
          end;
        end;
    end;
  end;
end;

procedure TClipboardManager.ThreadInternalCreate;
begin
  inherited ThreadInternalCreate;
  fHistory := TClipboardList.Create(ClipboardIndexFilename, ClipboardDataFilename, Self);
  fWindow := TClipboardWindow.Create;
  fWindow.BuildWindow();

  fStatusWindow := TClipboardStatusWindow.Create;
{  fStatusWindow.Left := 700; fStatusWindow.Right := 1100; fStatusWindow.Top := 500; fStatusWindow.Bottom := 600;
  fStatusWindow.BuildWindow();}
  RegisterHotkeys;

  ClipboardPasteNotificationEnabled := true;

  if not AppStartParams.AppHasBeenRestarted then
  begin
    DoThreadProcDelayed(@ClipboardChanged, Settings.Clipboard.ReadDelay, false);
    {$IfDef Logging}Logger.Add('TClipboardManager.ThreadInternalCreate: App has not been restarted -> capture current clipboard content');{$EndIf}
  end else
  begin
    fActiveItem := fHistory.LastItem;
    { TODO: test this case. }
    {$IfDef Logging}Logger.Add('TClipboardManager.ThreadInternalCreate: App has been restarted -> don''t capture current clipboard content');{$EndIf}
  end;
end;

procedure TClipboardManager.ThreadInternalDestroy;
begin
  ClipboardPasteNotificationEnabled := false;

  UnregisterHotkeys;
  fWindow.Free;
  fStatusWindow.Free;
  fHistory.Free;
  inherited ThreadInternalDestroy;
end;

procedure TClipboardManager.OnSettingChanged(aSetting: TObject);
begin
  inherited OnSettingChanged(aSetting);
end;

procedure TClipboardManager.RegisterHotkeys;
begin
  // may be entered by any thread
  HotkeyActionManager.Actions[haNavigateDown].RegisterActionHandler(@NavigateDown, Self);
  HotkeyActionManager.Actions[haNavigateUp].RegisterActionHandler(@NavigateUp, Self);
  HotkeyActionManager.Actions[haBlockCopy].RegisterActionHandler(@ChangeBlockCopyMode, Self);
end;

procedure TClipboardManager.UnregisterHotkeys;
begin
  // may be entered by any thread
  HotkeyActionManager.Actions[haNavigateDown].UnregisterActionHandler;
  HotkeyActionManager.Actions[haNavigateUp].UnregisterActionHandler;
  HotkeyActionManager.Actions[haBlockCopy].UnregisterActionHandler;
end;

procedure TClipboardManager.ClearClipboard;
begin
  if IsRunInThread then
  begin
    fHistory.ClearActiveItems;
    fActiveItem := nil;
    inherited ClearClipboard;
  end else DoThreadProc(@ClearClipboard);
end;

constructor TClipboardManager.Create;
begin
  fCopyMode := cmNormal;
  fBlockDirection := bdReversed;
  inherited Create;
end;

destructor TClipboardManager.Destroy;
begin
  inherited Destroy;
end;

end.

