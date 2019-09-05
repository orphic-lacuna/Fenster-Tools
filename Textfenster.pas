unit Textfenster;

interface

uses Forms, Windows, Messages, Sysutils, Classes, Graphics, GDIPOBJ, GDIPapi, AccurateTimer, Win32Extra;

type
  TWndProc = function (var msg: TMsg): Integer of object;

type
  TTextfensterIcon = (tfi_up, tfi_down);
  TTextfensterIcons = set of TTextfensterIcon;

type
  TMonitorMode = (mmFixed, mmActiveWindow, mmMousePos, mmAll);
  TPositionInfo = record
    // UseRelativeCoords: Boolean; immer true !!!
    // in 1:1000
    Autosize: Boolean;
    Left, Right, Top, Bottom: Integer;
    case MonitorMode: TMonitorMode of
      mmFixed: (MonitorNumber: DWord);
  end;

type
  TTextFenster = class
  private
    Timer: Cardinal;
    WndClass: TWndClass;
    WndHandle: HWND;

    Bmp: TGPBitmap;
    gp: TGPGraphics;
    gpfont: TGPFont;
    gpstringFormat: TGPStringFormat;
    gpfontbrush: TGPBrush;
    textRect: TGPRectF;
    wnd_x, wnd_y, wnd_width, wnd_height: Integer;
    FadeOutTransparency: Byte;

    FDynamicSize: Boolean;
    FFontName: String;
    FFontSize: Integer;
    FFontColor, FBackgroundColor: TColor;
    FTransparency: Byte;
    FBorderFadeRadius: Integer;
    FBorderRadius: Integer;
    FText: UnicodeString;
    FWindowTimeout: Integer;
    FLeft, FTop, FWidth, FHeight: Integer;
    FFadeOutTime: Integer;
    FIcons: TTextfensterIcons;

    FadeOutTimer: TAccurateTimer;

    {PushLayeredWindow}
    hdc_layered_window: HDC;
    hbitmap_layered_window: HBITMAP;
    BitmapPos: TPoint;
    BitmapSize: TSize;

    function WindowProc(var Msg: TMsg): Integer;

    procedure RenderWindow;
    procedure CalcSize;
    procedure PushLayeredWindow();

    procedure SetText(const Value: UnicodeString);
    procedure SetFontName(Value: String);
    procedure SetFontSize(Value: Integer);
    procedure SetFontColor(Value: TColor);
  public
    property DynamicSize: Boolean read FDynamicSize write FDynamicSize;
    property FontName: String read FFontName write SetFontName;
    property FontSize: Integer read FFontSize write SetFontSize;
    property FontColor: TColor read FFontColor write SetFontColor;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property Transparency: Byte read FTransparency write FTransparency;
    property BorderFadeRadius: Integer read FBorderFadeRadius write FBorderFadeRadius;
    property BorderRadius: Integer read FBorderRadius write FBorderRadius;
    property Text: UnicodeString read FText write SetText;
    property WindowTimeout: Integer read FWindowTimeout write FWindowTimeout;
    {Left and Top are ignored if DynamicSize = true}
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    {Width and Height represent maximum values for width and height if DynamicSize = true}
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property FadeOutTime: Integer read FFadeOutTime write FFadeOutTime;
    property Icons: TTextfensterIcons read FIcons write FIcons;

    procedure Show;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

const
  WND_CLASS_NAME = 'FensterTools ClipboardContent WndClass' + #0;
  GWL_METHODCODE = SizeOf(pointer) * 0;
  GWL_METHODDATA = SizeOf(pointer) * 1;
  TimerID = 56545;

function TColor2RGB(Color: TColor; Alpha: Byte = 255): ARGB;
begin
  if Color shr 24 = $FF then
    Color := GetSysColor(Color and $FF)
  else if Color shr 24 > $02 then
    Color := 0;
  {$RangeChecks Off}
  Result := MakeColor(Alpha, Color, Color shr 8, Color shr 16);
end;

function NonObjectWndProc(hwnd: HWND; Msg: Cardinal; wParam: Integer; lParam: Integer): Integer; stdcall;
var
  instanceWndProc: TMethod;
  msg_            : TMsg;
begin
  instanceWndProc.Code := {%H-}Pointer(GetWindowLong(hwnd, GWL_METHODCODE));
  instanceWndProc.Data := {%H-}Pointer(GetWindowLong(hwnd, GWL_METHODDATA));
  if Assigned(TWndProc(instanceWndProc)) then
  begin
    msg_.hwnd := hwnd;
    msg_.message := Msg;
    msg_.wParam := WParam;
    msg_.lParam := LParam;
    Result := TWndProc(instanceWndProc)(msg_);
  end else Result := DefWindowProc(hwnd, Msg, WParam,LParam);
end;

constructor TTextFenster.Create;
var
  alreadyRegistered: Boolean;
  tmpClass: TWndClass;
begin
  FText := '';
  FDynamicSize := true;
  FLeft := 250; FTop := 250; FWidth := Screen.Width-500; FHeight := Screen.Height-500;
  FFontName := 'Arial';
  FFontSize := 15;
  FFontColor := clWhite;
  FBackgroundColor := clBlack;
  FTransparency := 120;
  FBorderRadius := 50; FBorderFadeRadius := 20;
  FWindowTimeout := 2500;
  FFadeOutTime := 256;

  WndClass.lpszClassName := WND_CLASS_NAME;
  WndClass.lpfnWndProc   := @NonObjectWndProc;
  WndClass.Style         := CS_VREDRAW or CS_HREDRAW;
  WndClass.hInstance     := hInstance;
  WndClass.hIcon         := LoadIcon(0, IDI_APPLICATION);
  WndClass.hCursor       := LoadCursor(0, IDC_ARROW);
  WndClass.hbrBackground := (COLOR_WINDOW + 1);
  WndClass.lpszMenuName  := nil;
  WndClass.cbClsExtra    := 0;
  WndClass.cbWndExtra    := SizeOf(TMethod);
  alreadyRegistered := GetClassInfo(HInstance, WND_CLASS_NAME, tmpClass{%H-});
  if not alreadyRegistered then Windows.RegisterClass(WndClass);

  gpfont := TGPFont.Create(FFontName, FFontSize, FontStyleRegular);
  gpfontbrush := TGPSolidBrush.Create(TColor2RGB(FFontColor));
  gpstringFormat := TGPStringFormat.Create();
  gpstringFormat.SetAlignment(StringAlignmentCenter);
  gpstringFormat.SetLineAlignment(StringAlignmentCenter);
  gpstringFormat.SetTrimming(StringTrimmingEllipsisPath);
end;

procedure TTextFenster.SetText(const Value: UnicodeString);
begin
  if Length(Value) > 16384 then
    FText := Copy(Value, 1, 16384)
  else
    FText := Value;
end;

procedure TTextFenster.SetFontName(Value: String);
begin
  if FFontName <> Value then
  begin
    if gpfont <> nil then gpfont.Free;
    FFontName := Value;
    gpfont := TGPFont.Create(FFontName, FFontSize, FontStyleRegular);
  end;
end;

procedure TTextFenster.SetFontSize(Value: Integer);
begin
  if FFontSize <> Value then
  begin
    if gpfont <> nil then gpfont.Free;
    FFontSize := Value;
    gpfont := TGPFont.Create(FFontName, FFontSize, FontStyleRegular);
  end;
end;
procedure TTextFenster.SetFontColor(Value: TColor);
begin
  if FFontColor <> Value then
  begin
    if gpfontbrush <> nil then gpfontbrush.Free;
    FFontColor := Value;
    gpfontbrush := TGPSolidBrush.Create(TColor2RGB(FFontColor));
  end;
end;

procedure TTextFenster.Show;
var
  msg: tagMSG;
begin
  CalcSize;

  if WndHandle = 0 then
  begin
    FadeOutTransparency := 255;

    // Layered und Klicktransparenz
    WndHandle := CreateWindowEx(WS_EX_NOACTIVATE or WS_EX_LAYERED or WS_EX_TRANSPARENT or WS_EX_TOPMOST, WND_CLASS_NAME, '', WS_VISIBLE, wnd_x, wnd_y, wnd_width, wnd_height, 0, 0, hInstance, nil);
    SetWindowLongPtr(WndHandle, GWL_METHODDATA, LONG_PTR(Self));
    SetWindowLongPtr(WndHandle, GWL_METHODCODE, {%H-}LONG_PTR(@TTextFenster.WindowProc));
    // Topmost
    SetWindowPos(WndHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
  end else
  begin
    KillTimer(WndHandle, TimerID);
    if FadeOutTimer <> nil then FreeAndNil(FadeOutTimer);
    while PeekMessage(msg{%H-}, WndHandle, WM_TIMER, WM_TIMER, PM_REMOVE) do;
    FadeOutTransparency := 255;
    SetWindowPos(WndHandle, 0, wnd_x, wnd_y, wnd_width, wnd_height, SWP_NOZORDER);
  end;

  RenderWindow;
  PushLayeredWindow;

  Timer := SetTimer(WndHandle, TimerID, WindowTimeout, nil);
end;

procedure TTextFenster.CalcSize;
var
  dc: HDC;
  predefinedRect: TGPRectF;
begin
  textRect.X := FBorderFadeRadius div 2;//FBorderRadius + FBorderFadeRadius;
  textRect.Y := FBorderFadeRadius div 2;//FBorderRadius + FBorderFadeRadius;

  if FWidth < 0 then
    textRect.Width := Screen.Width - FBorderFadeRadius
  else
    textRect.Width := FWidth - FBorderFadeRadius;

  if FHeight < 0 then
    textRect.Height := Screen.Height - FBorderFadeRadius
  else
    textRect.Height := FHeight - FBorderFadeRadius;

  if FDynamicSize then
  begin
    if FText <> '' then
    begin
      dc := GetDC(0);

      predefinedRect := textRect;

      gp := TGPGraphics.Create(dc);
      gp.MeasureString(FText, Length(FText), gpfont, predefinedRect, gpstringFormat, textRect);
      gp.Free;

      ReleaseDC(0, dc);
    end else
    begin
      textRect.Width := 300;
      textRect.Height := 150;
    end;

    // center the window

    wnd_width := Round(textRect.Width+2*FBorderFadeRadius);
    wnd_height := Round(textRect.Height+FBorderFadeRadius);
    if wnd_width < 200 then wnd_width := 200;
    if wnd_height < 75 then wnd_height := 75;
    if wnd_width > Screen.Width then wnd_width := Screen.Width;
    if wnd_height > Screen.Height then wnd_height := Screen.Height;
    wnd_x := (Screen.Width - wnd_width) div 2;
    wnd_y := (Screen.Height - wnd_height) div 2;

    textRect.X := FBorderFadeRadius div 2;//FBorderRadius + FBorderFadeRadius;
    textRect.Y := FBorderFadeRadius div 2;//FBorderRadius + FBorderFadeRadius;
    textRect.Width := wnd_width - FBorderFadeRadius;
    textRect.Height := wnd_height - FBorderFadeRadius;
  end else begin
    wnd_x := FLeft;
    wnd_y := FTop;
    wnd_width := FWidth;
    wnd_height := FHeight;
  end;

  bmp := TGPBitmap.Create(wnd_width, wnd_height, PixelFormat32bppPARGB);
  gp := TGPGraphics.Create(bmp);
  gp.SetCompositingQuality(CompositingQualityHighSpeed);
  gp.SetCompositingMode(CompositingModeSourceOver);
  gp.SetSmoothingMode(SmoothingModeHighQuality);
  gp.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
end;

procedure TTextFenster.RenderWindow;
var
  factors, pos: Array [0..3] of Single;
  p: TGPGraphicsPath;
  b3: TGPPathGradientBrush;
  color_: ARGB;
  count: Integer;

  img_height, offset: Integer;

  img: TGPImage;
begin
  p := TGPGraphicsPath.Create;

  p.AddArc(0, 0, FBorderRadius, FBorderRadius, 180, 90); // topleft
  p.AddArc(wnd_width - FBorderRadius, 0, FBorderRadius, FBorderRadius, 270, 90); // topright
  p.AddArc(wnd_width - FBorderRadius, wnd_height - FBorderRadius, FBorderRadius, FBorderRadius, 0, 90); // bottomright
  p.AddArc(0, wnd_height - FBorderRadius, FBorderRadius, FBorderRadius, 90, 90); // bottomleft}
  p.CloseFigure;

  b3 := TGPPathGradientBrush.Create(p);

  b3.SetCenterColor(TColor2RGB(FBackgroundColor, FTransparency));
  color_ := MakeColor(0, 0, 0, 0);
  count := 1;
  b3.SetSurroundColors(@color_,count);

  b3.SetFocusScales(1-(FBorderFadeRadius/wnd_width), 1-(FBorderFadeRadius/wnd_height));
  factors[0] := 0;
  pos[0] := 0;
  factors[1] := 0.05;
  pos[1] := 0.15;
  factors[2] := 0.90;
  pos[2] := 0.85;
  factors[3] := 1;
  pos[3] := 1;
  b3.SetBlend(@factors[0], @pos[0],4);

  gp.FillPath(b3, p);

  gp.DrawString(FText, Length(FText), gpfont, textRect, gpstringFormat, gpfontbrush);
  //gp.FillRectangle(TGPSolidBrush.Create(MakeColor(40, 255, 255, 255)), textRect);

  p.Free;

  if FText = '' then
  begin
    img := TGPImage.Create('Icons/Empty.png');

    img_height := wnd_height - FBorderFadeRadius - 42;
    if img_height > 200 then img_height := 200;

    gp.DrawImage(img, (wnd_width - img_height) div 2, (wnd_height - img_height) div 2 - 21, img_height, img_height);
    img.Free;

    textRect.X := FBorderFadeRadius div 2;//FBorderRadius + FBorderFadeRadius;
    offset := (wnd_height) div 2 - 21 + (img_height div 2);

    textRect.Y := offset;//(wnd_height) div 2 + 22 div 2;//FBorderRadius + FBorderFadeRadius;
    textRect.Width := wnd_width - FBorderFadeRadius;
    textRect.Height := wnd_height - offset;

    gp.DrawString('Zwischenablage ist leer', -1, gpfont, textRect, gpstringFormat, gpfontbrush);
  end;

{  img := TGPImage.Create('go-up-5.png');
  gp.DrawImage(img, 30, 30);}
  {$Message Hint 'Up-Down moeglich Anzeige'}

  hdc_layered_window := CreateCompatibleDC(GetDC(0));
  bmp.GetHBITMAP(0, hbitmap_layered_window);
  SelectObject(hdc_layered_window, hbitmap_layered_window);

  BitmapPos := Point(0, 0);
  BitmapSize.cx := Bmp.GetWidth;
  BitmapSize.cy := Bmp.GetHeight;
end;

procedure TTextFenster.PushLayeredWindow;
var
  BlendFunction: TBlendFunction;
begin
  BlendFunction.BlendOp := AC_SRC_OVER;
  BlendFunction.BlendFlags := 0;
  BlendFunction.SourceConstantAlpha := FadeOutTransparency;
  BlendFunction.AlphaFormat := AC_SRC_ALPHA;

  UpdateLayeredWindow(WndHandle, 0, nil, @BitmapSize, hdc_layered_window, @BitmapPos, 0, @BlendFunction, ULW_ALPHA);
end;

function TTextFenster.WindowProc(var Msg: TMsg): Integer;
begin
  case Msg.Message of
    WM_TIMER:
      begin
        Result := 0;
        case FadeOutTransparency of
          255: begin
            KillTimer(Msg.hwnd, TimerID);

            FadeOutTimer := TAccurateTimer.Create(FFadeOutTime*1000 div 256, WndHandle);
            FadeOutTimer.StartTimer;
            Dec(FadeOutTransparency);
            PushLayeredWindow;
          end;
          1..254: begin
            Dec(FadeOutTransparency);
            PushLayeredWindow;
          end;
          0: begin
            FreeAndNil(FadeOutTimer);
            DeleteObject(hbitmap_layered_window);
            DeleteDC(hdc_layered_window);
            FreeAndNil(bmp);
            FreeAndNil(gp);
            DestroyWindow(Msg.hwnd);
            WndHandle := 0;
          end;
        end;
      end;
    else Result := DefWindowProc(Msg.hwnd, Msg.Message, Msg.wParam, Msg.LParam);
  end;
end;

destructor TTextFenster.Destroy;
begin
  gpstringFormat.Free;
  gpfontbrush.Free;
  gpfont.Free;

  Windows.UnregisterClass(WND_CLASS_NAME, hInstance);
end;

end.
