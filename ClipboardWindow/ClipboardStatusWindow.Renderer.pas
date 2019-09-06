unit ClipboardStatusWindow.Renderer;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, LayeredWindowRenderer, ClipboardStatusWindow.Settings;

type

  { TCBStatusWndRenderer }

  TCBStatusWndRenderer = class(TLayeredWindowRenderer)
  private
    fSettings: TCBSTATUSWND_Settings;
  protected
    procedure CreateDeviceDependentResources; override;
    procedure DiscardDeviceDependentResources; override;
    procedure DoRender; override;
  public
    property Settings: TCBSTATUSWND_Settings read fSettings write fSettings;

    procedure UpdateRenderTarget; override;

    constructor Create(aWnd: HWND);
    destructor Destroy; override;
  end;

implementation

{ TCBStatusWndRenderer }

constructor TCBStatusWndRenderer.Create(aWnd: HWND);
begin
  inherited Create(aWnd);
end;

procedure TCBStatusWndRenderer.CreateDeviceDependentResources;
begin

end;

procedure TCBStatusWndRenderer.DiscardDeviceDependentResources;
begin

end;

procedure TCBStatusWndRenderer.DoRender;
begin
  //RenderTarget.Clear(fSettings.WindowBackgroundColor);
  //RenderTarget.SetTransform(IdentityMatrix);


end;

procedure TCBStatusWndRenderer.UpdateRenderTarget;
begin
  inherited UpdateRenderTarget;
end;



destructor TCBStatusWndRenderer.Destroy;
begin
  inherited Destroy;
end;

end.

