unit SettingsDerivedBaseTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SettingsBase, SettingsBaseTypes, DX12.D2D1;

type

  { TWindowSizeSettings }

  TWindowSizeSettings = class(TSettingsBase)
  private
    fLeft, fTop, fRight, fBottom: TSettingsBaseFloat;
  published
    property Left: TSettingsBaseFloat read fLeft write fLeft;
    property Top: TSettingsBaseFloat read fTop write fTop;
    property Right: TSettingsBaseFloat read fRight write fRight;
    property Bottom: TSettingsBaseFloat read fBottom write fBottom;
  end;

  { TSettingsBaseDirect2DColor }

  TSettingsBaseDirect2DColor = class(TSettingsBase)
  private
    fAlpha: TSettingsBaseFloat;
    fBlue: TSettingsBaseFloat;
    fGreen: TSettingsBaseFloat;
    fRed: TSettingsBaseFloat;
  published
    property Red: TSettingsBaseFloat read fRed write fRed;
    property Green: TSettingsBaseFloat read fGreen write fGreen;
    property Blue: TSettingsBaseFloat read fBlue write fBlue;
    property Alpha: TSettingsBaseFloat read fAlpha write fAlpha;
  end;

  operator := (a: TSettingsBaseDirect2DColor) b: TD2D1_COLOR_F;


implementation

operator := (a: TSettingsBaseDirect2DColor) b: TD2D1_COLOR_F;
begin
  b.Init(a.Red.Value, a.Green.Value, a.Blue.Value, a.Alpha.Value);
end;

end.

