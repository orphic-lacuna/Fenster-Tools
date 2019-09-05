unit Replacer;

interface

uses Sysutils, Classes, ReplaceList, FensterToolsCommon, SettingsManager;


const
  VALID_MAKROS: Array [0..7] of UnicodeString = ('DATUM', 'HEUTE', 'ZEIT', 'TAG', 'VORGESTERN', 'GESTERN', 'MORGEN', 'ÜBERMORGEN');

type
  TReplacer = class
  private
    function ResolveMakrosInReplaceString(Str: UnicodeString): UnicodeString;
    function DoReplaceMakro(Makro: Integer): UnicodeString;

    //procedure ReplaceListChanged(Sender: TObject);
  public
    procedure Replace(var Str: UnicodeString);

    constructor Create{(aReplaceList: TReplaceList)};
    destructor Destroy; override;
  end;

const
  LongDayNames: Array [1..7] of String = ('Sonntag', 'Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag', 'Samstag');

implementation

constructor TReplacer.Create{(aReplaceList: TReplaceList)};
begin
  //Settings.Clipboard.ReplaceList.ChangeHandler.AddChangeHandler(@ReplaceListChanged);
end;

{procedure TReplacer.ReplaceListChanged(Sender: TObject);
var
  i: Integer;
begin
  Settings.Clipboard.ReplaceList.Lock;
  for i := 0 to Settings.Clipboard.ReplaceList.Count-1 do
  begin
    if TReplaceListItem(Settings.Clipboard.ReplaceList.Items[i]).IsRegEx then
    begin
      if not TReplaceListItem(Settings.Clipboard.ReplaceList.Items[i]).RegEx.Compiled then TReplaceListItem(Settings.Clipboard.ReplaceList.Items[i]).RegEx.Compile;
      if not TReplaceListItem(Settings.Clipboard.ReplaceList.Items[i]).RegEx.Optimized then TReplaceListItem(Settings.Clipboard.ReplaceList.Items[i]).RegEx.Optimize;
		end;
	end;
  Settings.Clipboard.ReplaceList.Unlock;
end;
}

        {
procedure TReplacer.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
var
  i: Integer;
begin
  for i := 0 to fReplaceList.Count-1 do
  begin
    if TReplaceListItem(fReplaceList.Items[i]).IsRegEx then
    begin
      if not TReplaceListItem(fReplaceList.Items[i]).RegEx.Compiled then TReplaceListItem(fReplaceList.Items[i]).RegEx.Compile;
      if not TReplaceListItem(fReplaceList.Items[i]).RegEx.Optimized then TReplaceListItem(fReplaceList.Items[i]).RegEx.Optimize;
		end;
	end;
end;

procedure TReplacer.SetReplaceList(Value: TReplaceList);
var
  i: Integer;
begin
  if fReplaceList <> nil then fReplaceList.FPODetachObserver(Self);
  fReplaceList := Value;
  if fReplaceList <> nil then fReplaceList.FPOAttachObserver(Self);
end;      }

destructor TReplacer.Destroy;
begin
  //Settings.Clipboard.ReplaceList.ChangeHandler.RemoveChangeHandler(@ReplaceListChanged);
  inherited Destroy;
end;

procedure TReplacer.Replace(var Str: UnicodeString);
var
//  c1, c2, freq: Int64;
  item: TReplaceListItem;
  flags: TReplaceFlags;
begin
//  QueryPerformanceFrequency(freq);
//  QueryPerformanceCounter(c1);
//  Result := false;
  if Str = '' then
  begin
    exit;
  end;

  Settings.Clipboard.ReplaceList.Lock;

  //for i := 0 to fReplaceList.Count-1 do
  for TCollectionItem(item) in Settings.Clipboard.ReplaceList do
  begin
    if item.IsRegEx then
    begin
      item.RegEx.Subject := Str;
      if item.OnlyExtract then
      begin
        // Extract                 
        if item.RegEx.Match then
        begin
          Str := item.RegEx.MatchedString;
          //if MatchAll then
          //begin
            while item.RegEx.Match do Str := Str + item.RegEx.MatchedString;
          //end;
        end;
      end else
      begin
        // Replace
        item.RegEx.Replacement := ResolveMakrosInReplaceString(item.UnicodeReplaceWith);
        if item.RegEx.Replace({MatchAll}true) then
          Str := item.RegEx.ReplacedString;
      end;
    end else
    begin
      // normales Suchen und Ersetzen
      flags := [rfReplaceAll];
      if not item.CaseSensitive then flags := flags + [rfIgnoreCase];
      Str := UnicodeStringReplace(Str, item.UnicodeLookFor, ResolveMakrosInReplaceString(item.UnicodeReplaceWith), flags);
    end;
  end;

  Settings.Clipboard.ReplaceList.Unlock;

//  Result := true;

//  QueryPerformanceCounter(c2);
//  MessageBox(0, PChar(FloatToStr((c2-c1)/freq)), '', 64);
end;

function TReplacer.DoReplaceMakro(Makro: Integer): UnicodeString;
begin
  case Makro of
    0,1: Result := UnicodeString(DateToStr(Date));
    2: Result := UnicodeString(TimeToStr(Time));
    3: Result := UnicodeString(LongDayNames[DayOfWeek(Date)]);
    4: Result := UnicodeString(DateToStr(Date-2));
    5: Result := UnicodeString(DateToStr(Date-1));
    6: Result := UnicodeString(DateToStr(Date+1));
    7: Result := UnicodeString(DateToStr(Date+2));
  end;
end;

function TReplacer.ResolveMakrosInReplaceString(Str: UnicodeString): UnicodeString;
var
  i, j, StrLen: Integer;
  Gefunden: boolean;
begin
  StrLen := Length(Str);
  Result := '';
  i := 1;
  while i <= StrLen do
  begin
    Gefunden := false;
    for j := Low(VALID_MAKROS) to High(VALID_MAKROS) do
    begin
      if (i+Length(VALID_MAKROS[j])-1 <= StrLen) and (Str[i] = VALID_MAKROS[j][1]) and (CompareMem(@Str[i], @VALID_MAKROS[j][1], Length(VALID_MAKROS[j]))) then
      begin
        if (i=1) or (Str[i-1] <> '\') then
        begin
          Gefunden := true;
          i := i + Length(VALID_MAKROS[j]);
          Result := Result + DoReplaceMakro(j);
          break;
        end else SetLength(Result, Length(Result)-1);
      end;
    end;
    if not Gefunden then
    begin
      Result := Result + Str[i];
      Inc(i);
    end;
  end;
end;

end.
