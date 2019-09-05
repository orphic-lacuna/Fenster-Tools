unit WindowManager;

interface

uses
  Forms, Windows, Messages, ExtendedWinAPI, Sysutils, Classes, FensterToolsCommon, ToolBox, Menus, fpjson, jsonparser, ApplicationRestart, fgl{$IfDef Logging}, Logging{$EndIf};

type

	{ THiddenWindow }

  THiddenWindow = class
  private
    PID: Cardinal;
    FCaption: UnicodeString;
    Windows: Array of Hwnd;

    FHidden: boolean;
		function GetAsJSON: TJSONObject;
    function HideWindow(handle: HWND): boolean;
  public
    property AsJSON: TJSONObject read GetAsJSON;
    property Hidden: boolean read FHidden;
    property Caption: UnicodeString read FCaption;

    procedure Show;

    constructor Create(handle: HWND; SingleWindowProgramList: TStringList); overload;
    constructor Create(hiddenWnd: TJSONObject); overload;
//    destructor Destroy; override;
  end;

type
  THiddenWindowList = specialize TFPGList<THiddenWindow>;

type

  { TWindowManager }

  TWindowManager = class
  private
    HiddenWindows: THiddenWindowList;
    SingleWindowProgramList: TStringList;
    FMenuItem, FSeparatorMenuItem: TMenuItem;

    procedure MenuItemClicked(Sender: TObject);
    class procedure ShakeWindow(handle: HWND);
    procedure LoadFromFile();
  public

    property MenuItem: TMenuItem read FMenuItem write FMenuItem;

    procedure CloseAllWindows;
    procedure HideWindow; overload;
    function HideWindow(handle: HWND): Boolean; overload;
    procedure ToggleWindowTopMost; overload;
    procedure ToggleWindowTopMost(handle: HWND); overload;
    constructor Create(aMenuItem, SeparatorMenuItem: TMenuItem);
    procedure SaveToFile(Action: TWindowAction = waNone; Handle: HWND = 0);
    destructor Destroy; override;
  end;

var
  WndManager: TWindowManager;

implementation

uses Hotkey.Actions;

var
  ProgramManagerHandle, TaskbarHandle, StartButtonHandle, StartmenuHandle: Hwnd;

function EnumWindowCallback(hwnd: HWND; lParam: Integer): LongBool; stdcall;
var
  procID: Cardinal;
begin
  Result := true;
  if not IsWindow(hwnd) or not IsWindowVisible(hwnd) or (hwnd = ProgramManagerHandle) or (hwnd = StartbuttonHandle) or (Length(GetWindowText(hwnd)) = 0) or not (GetWindowLong(hwnd, GWL_STYLE) and WS_CAPTION = WS_CAPTION) then exit;
  if lParam = 0 then
  begin
    PostMessage(hwnd, WM_CLOSE, 0, 0);
  end else if TObject(lParam) is THiddenWindow then
  begin
    GetWindowThreadProcessId(hwnd, procID);
    if THiddenWindow(lParam).PID = procID then
    begin
      SetLength(THiddenWindow(lParam).Windows, Length(THiddenWindow(lParam).Windows)+1);
      THiddenWindow(lParam).Windows[High(THiddenWindow(lParam).Windows)] := hwnd;
      if not THiddenWindow(lParam).HideWindow(hwnd) then
      begin
        Application.RestartParams.TargetActionHandle := hwnd;
        Application.RestartParams.TargetAction := waHide;
        Application.Restart;
        Result := false;
      end;
    end;
  end;
end;

{$Region 'THiddenWindow'}

constructor THiddenWindow.Create(hiddenWnd: TJSONObject);
var
  handle_list: TJSONArray;
  i: Integer;
begin
  PID := hiddenWnd.Integers['PID'];
  handle_list := hiddenWnd.Arrays['handles'];
  SetLength(Windows, handle_list.Count);
  for i := 0 to High(Windows) do
  begin
    Windows[i] := handle_list.Integers[i];
  end;
end;

constructor THiddenWindow.Create(handle: HWND; SingleWindowProgramList: TStringList);
var
  DialogResult: Integer;
  ProgramName: String;
begin
  FHidden := false;
  if Handle = GetDesktopWindow then exit;

  FCaption := GetWindowText(handle);
  //Das Owner-Window herausfinden!! wegen Text und wegen Focus setzen, beim sichtbar machen
  GetWindowThreadProcessID(handle, PID);
  Programname := GetModuleFileName(PID);
  if Length(FCaption) > 100 then
  begin
    SetLength(FCaption, 96);
    FCaption := FCaption + ' ...';
  end;

  if (FCaption = '') or (handle = TaskbarHandle) or (handle = StartButtonHandle) or (handle = ProgramManagerHandle) and (Handle <> StartmenuHandle) then
  begin
      if TaskDialog(0, 0, 'FensterTools', 'Kein Fenster markiert', 'Es wurde kein Fenster zum Ausblenden angeklickt. Vor dem Drücken der Tastenkombination muss das Fenster angeklickt werden.', TDCBF_OK_BUTTON, MakeIntResourceW(TD_QUESTION_ICON), DialogResult{%H-}) <> S_OK then
        MessageBox(0, 'Es wurde kein Fenster zum Ausblenden angeklickt. Vor dem Drücken der Tastenkombination muss das Fenster angeklickt werden.', 'Ungereimtheiten beim Ausblenden eines Fensters...', MB_ICONQUESTION);
      exit;
  end;

  if (SingleWindowProgramList <> nil) and (SingleWindowProgramList.IndexOf(lowercase(ProgramName)) >= 0) then
  begin
    SetLength(Windows, 1); Windows[0] := handle;
    if HideWindow(handle) then
    begin
      SetForegroundWindow(GetDesktopWindow);
      FHidden := true;
    end else begin
      Application.RestartParams.TargetActionHandle := handle;
      Application.RestartParams.TargetAction := waHide;
      Application.Restart;
    end;
  end else
  begin
    SetLength(Windows, 0);
    EnumWindows(@EnumWindowCallback, Integer(Self));
    SetForegroundWindow(GetDesktopWindow);
    if Length(Windows) > 0 then FHidden := true;
  end;
end;

function THiddenWindow.HideWindow(handle: HWND): boolean;
var
  Timeout: UInt64;
begin
  ShowWindowAsync(handle, SW_HIDE);
  Timeout := GetTickCount64;
  while (GetTickCount64-Timeout < 1000) and (IsWindowVisible(handle)) do sleep(1);
  Result := not IsWindowVisible(handle);
end;

function THiddenWindow.GetAsJSON: TJSONObject;
var
  a: TJSONArray;
  i: Integer;
begin
  Result := TJSONObject.Create;
  Result.Add('PID', PID);
  Result.Add('Caption', UTF8Encode(Self.Caption));
  a := TJSONArray.Create;
  for i := 0 to High(Windows) do
  begin
    a.Add(Windows[i]);
	end;
  Result.Add('handles', a);
end;

procedure THiddenWindow.Show;
var
  i: Integer;
begin
  for i := 0 to High(Windows) do
  begin
    ShowWindowAsync(Windows[i], SW_SHOW);
  end;
  FHidden := false;
end;


{$EndRegion}

{$Region 'TWindowManager'}
constructor TWindowManager.Create(aMenuItem, SeparatorMenuItem: TMenuItem);
begin
  HiddenWindows := THiddenWindowList.Create;
  StartbuttonHandle := GetStartButtonhwnd;
  ProgramManagerHandle := FindWindow(nil, 'Program Manager');
  TaskbarHandle := FindWindow('Shell_TrayWnd', nil);
  StartmenuHandle := FindWindow('DV2ControlHost', nil);

  FMenuItem := aMenuItem;
  FSeparatorMenuItem := SeparatorMenuItem;

  SingleWindowProgramList := TStringList.Create;
  SingleWindowProgramList.Add('firefox.exe');
  SingleWindowProgramList.Add('explorer.exe');
  LoadFromFile;

  HotkeyActionManager.Actions[haCloseAllWindows].RegisterActionHandler(@CloseAllWindows);
  HotkeyActionManager.Actions[haHideWindow].RegisterActionHandler(@HideWindow);
  HotkeyActionManager.Actions[haMakeWindowTopmost].RegisterActionHandler(@ToggleWindowTopMost);
end;

class procedure TWindowManager.ShakeWindow(handle: HWND);
const
  MAXDELTA = 5;
  COUNT = 40;
var
  oRect, wRect :TRect;
  deltax : integer;
  deltay : integer;
  i: integer;
begin
  //remember original position
  GetWindowRect(handle, wRect{%H-}) ;
  oRect := wRect;

  for i := 0 to COUNT do
  begin
    wRect := oRect;
    deltax := Round(Random(2*MAXDELTA))-MAXDELTA;
    deltay := Round(Random(2*MAXDELTA))-MAXDELTA;
    OffsetRect(wRect, deltax, deltay) ;
    MoveWindow(handle, wRect.Left, wRect.Top, wRect.Right - wRect.Left, wRect.Bottom - wRect.Top, true) ;
    sleep(15);
  end;
  //return to start position
  MoveWindow(handle, oRect.Left,oRect.Top,oRect.Right - oRect.Left,oRect.Bottom - oRect.Top,true) ;
end;

procedure TWindowManager.ToggleWindowTopMost(handle: HWND);
var
  WindowLongBefore: Integer;
  DialogResult: Integer;
begin

  if (GetWindowText(handle) <> '') and (handle <> StartButtonHandle) and (handle <> ProgramManagerHandle) and (handle <> TaskbarHandle) then
  begin
    WindowLongBefore := GetWindowLong(handle, GWL_EXSTYLE) and WS_EX_TOPMOST;
    if WindowLongBefore = WS_EX_TOPMOST then
      SetWindowPos(handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE)
    else
      SetWindowPos(handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
    if GetWindowLong(handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> WindowLongBefore then
      ShakeWindow(handle)
    else begin
      Application.RestartParams.TargetActionHandle := handle;
      Application.RestartParams.TargetAction := waToggleTopMost;
      Application.Restart;
    end;
  end else
  begin
    if TaskDialog(0, 0, 'FensterTools', 'Kein Fenster markiert', 'Es wurde kein Fenster, welches dauerhaft im Vordergrund bleiben soll, angeklickt. Vor dem Drücken der Tastenkombination muss das Fenster angeklickt werden.', TDCBF_OK_BUTTON, MakeIntResourceW(TD_QUESTION_ICON), DialogResult{%H-}) <> S_OK then
      MessageBox(0, 'Es wurde kein Fenster, welches dauerhaft im Vordergrund bleiben soll, angeklickt. Vor dem Drücken der Tastenkombination muss das Fenster angeklickt werden.', 'Kein Fenster markiert...', MB_ICONQUESTION);
  end;
end;

procedure TWindowManager.MenuItemClicked(Sender: TObject);
var
  index: Integer;
begin
  if not (Sender is TMenuItem) then exit;
  if TMenuItem(Sender).Owner = FMenuItem then
  begin
    index := FMenuItem.IndexOf(TMenuItem(Sender));
    if index > -1 then
    begin
      FMenuItem.Delete(index);
      THiddenWindow(HiddenWindows[index]).Show;
      THiddenWindow(HiddenWindows[index]).Free;
      HiddenWindows.Delete(index);

      if HiddenWindows.Count = 0 then
      begin
        FMenuItem.Visible := false;
        FSeparatorMenuItem.Visible := false;
      end;
    end;
  end;
end;

function TWindowManager.HideWindow(handle: HWND): Boolean;
var
  hw: THiddenWindow;
  mi: TMenuItem;
begin
  hw := THiddenWindow.Create(handle, SingleWindowProgramList);
  Result := hw.Hidden;
  if hw.Hidden then
  begin
    HiddenWindows.Insert(0, hw);
    if FMenuItem <> nil then
    begin
      FMenuItem.Visible := true;
      mi := TMenuItem.Create(MenuItem);
      mi.Caption := UTF8Encode(hw.Caption);
      mi.OnClick := @MenuItemClicked;
      FMenuItem.Insert(0, mi);
    end;
    if FSeparatorMenuItem <> nil then FSeparatorMenuItem.Visible := true;
  end else
    hw.Free;
end;

procedure TWindowManager.ToggleWindowTopMost;
begin
  ToggleWindowTopMost(GetForegroundWindow);
end;

procedure TWindowManager.CloseAllWindows();
begin
  EnumWindows(@EnumWindowCallback, 0); 
end;

procedure TWindowManager.HideWindow;
begin
  HideWindow(GetForegroundWindow);
end;


procedure TWindowManager.SaveToFile(Action: TWindowAction = waNone; Handle: HWND = 0);
var
  j: TJSONObject;
  list: TJSONArray;
  i: Integer;
  s: String;
begin
  j := TJSONObject.Create;
  list := TJSONArray.Create;
  for i := 0 to HiddenWindows.Count-1 do
  begin
    list.Add(HiddenWindows[i].GetAsJSON);
  end;
  j.Add('HiddenWindows', list);
  if Action <> waNone then
  begin
    j.Add('Action', Ord(Action));
    j.Add('Handle', Integer(Handle));
  end;
  with TFileStream.Create(Filename_HiddenWindows, fmOpenWrite or fmCreate) do
  begin
    s := j.AsJSON;
    Write(s[1], Length(s));
    Free;
	end;

  // Liste löschen, damit beim Schließen die Fenster nicht angezeigt werden
  for i := 0 to HiddenWindows.Count-1 do
  begin
    //THiddenWindow(HiddenWindows[i]).Show;
    THiddenWindow(HiddenWindows[i]).Free;
  end;
  HiddenWindows.Clear;
end;

procedure TWindowManager.LoadFromFile();
var
  json: TJSONObject;
  list: TJSONArray;
  i: Integer;
  mi: TMenuItem;
  parser: TJSONParser;
  fs: TFileStream;
begin
  if not FileExists(Filename_HiddenWindows) then exit;
  fs := nil; parser := nil; json := nil;
  try
  	try
      fs := TFileStream.Create(Filename_HiddenWindows, fmOpenRead);
    	parser := TJSONParser.Create(fs);
      json := TJSONObject(parser.Parse);
      if not (TJSONData(json) is TJSONObject) then
      begin
        FreeAndNil(json);
        raise Exception.Create('Could not parse HiddenWindows.json');
			end;

      list := json.Arrays['HiddenWindows'];
      {$IfDef Debug}
      if HiddenWindows.Count > 0 then raise Exception.Create('TWindowManager.LoadFromFile must be called before the user can hide a window.');
      HiddenWindows.Clear;
      {$EndIf}
      for i := 0 to list.Count-1 do
      begin
        HiddenWindows.Add(THiddenWindow.Create(list.Objects[i]));
        if FMenuItem <> nil then
        begin
          mi := TMenuItem.Create(MenuItem);
          mi.Caption := list.Objects[i].Strings['Caption'];
          mi.OnClick := @MenuItemClicked;
          FMenuItem.Insert(0, mi);
        end;
      end;
      if list.Count > 0 then
      begin
        if FMenuItem <> nil then FMenuItem.Visible := true;
        if FSeparatorMenuItem <> nil then FSeparatorMenuItem.Visible := true;
      end;
      if json.Find('Action', jtNumber) <> nil then
      begin
        if TWindowAction(json.Integers['Action']) = waHide then
        begin
          HideWindow(json.Integers['Handle']);
        end else if TWindowAction(json.Integers['Action']) = waToggleTopMost then
        begin
          ToggleWindowTopMost(json.Integers['Handle']);
        end;
      end;
    except
      {$IfDef Logging}Logger.Add('TWindowManager.LoadFromFile: Could not load the file', LT_ERROR);{$EndIf}
      {$IfDef Debug}raise Exception.Create('Could not load HiddenWindows.dat');{$EndIf}
  	end;
  finally
    TryFreeAndNil(parser);
    TryFreeAndNil(fs);
    TryFreeAndNil(json);
    DeleteFile(Filename_HiddenWindows);
	end;
end;

destructor TWindowManager.Destroy;
var
  i: Integer;
begin
  HotkeyActionManager.Actions[haCloseAllWindows].UnregisterActionHandler;
  HotkeyActionManager.Actions[haHideWindow].UnregisterActionHandler;
  HotkeyActionManager.Actions[haMakeWindowTopmost].UnregisterActionHandler;
  SingleWindowProgramList.Free;
  for i := 0 to HiddenWindows.Count-1 do
  begin
    THiddenWindow(HiddenWindows[i]).Show;
    THiddenWindow(HiddenWindows[i]).Free;
  end;
  HiddenWindows.Free;
  inherited Destroy;
end;

{$EndRegion}



end.
