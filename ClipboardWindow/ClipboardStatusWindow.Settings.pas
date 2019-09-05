unit ClipboardStatusWindow.Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DX12.D2D1;

type
  TCBSTATUSWND_Settings = class
  private

  public
    // window dimensions
	  Left, Top, Right, Bottom: Cardinal;

    // main element colors
    WindowBackgroundColor: TD2D1_COLOR_F;
    EntryBackgroundColor: TD2D1_COLOR_F;

    // fonts
{    EntryFont: UnicodeString;
    EntryFontColor: TD2D1_COLOR_F;
    TimestampFont: UnicodeString;
    EntryFontsize: Single;
    TimestampFontsize: Single;
    TimestampFontColor: TD2D1_COLOR_F;

	  // main element dimensions
	  DefaultEntryHeight: Single;
	  ActiveEntryHeight: Single;
	  MarginEntryEntry: Single;
	  MarginLeft, MarginRight: Single;
	  PaddingLeft, PaddingRight, PaddingTop, PaddingBottom: Single;

	  // Segmentation line
	  SegmentationLineDarkColor: TD2D1_COLOR_F;
	  SegmentationLineLightColor: TD2D1_COLOR_F;
	  SegmentationLineMargin: Single;

    // icon
	  IconMarginLeft, IconMarginTop, IconMarginRight: Single;
	  IconWidth, IconHeight: Single;

    // delays and timings
    ScrollAnimationDuration: Single;
    FadeoutDelay: DWord;
    FadeoutDuration: Single;

	  // misc
	  EntryRectRoundingRadius: Single;}
  end;

implementation

end.

