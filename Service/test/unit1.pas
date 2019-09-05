unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NamedPipes, Windows;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

type
  THelperSvcCommandType = (ctDoUpdate, ctRestartElevated, ctPatch, ctUnpatch, ctVerifyPatch);

  THelperSvcCommand = record
    Result: LongBool;
    case Command: THelperSvcCommandType of
      ctDoUpdate:();
      ctRestartElevated:();
      ctPatch, ctUnpatch, ctVerifyPatch: (
        PID: Cardinal;
        ModuleName: Array [0..255] of Char;
        FuncName: Array [0..255] of Char;
        CallbackWnd: Int64;
        CallbackMsg: Cardinal;
        Auth_HelperStartUpTime: Cardinal;
        Auth_CallerPID: Cardinal;
      );
  end;
  PHelperSvcCommand = ^THelperSvcCommand;
  THelperSvcCommandResponse = record
      Result: LongBool;
    end;
    PHelperSvcCommandResponse = ^THelperSvcCommandResponse;

var
  Form1: TForm1;
  x: TNamedPipeClient;
const
  {$ifdef WIN32}
  _SUFFIX = '32';
  {$else}
  _SUFFIX = '64';
  {$endif}
  PIPENAME = 'FT_HELPER_SVC_'+_SUFFIX;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //try
  //x := TNamedPipeClient.Create(PIPENAME);
  //if not x.Open then FOrm1.Caption := 'Could not connect';
  //except
    //Form1.Caption := 'ERROR:';
  //end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  cmd: THelperSvcCommand;
begin
  if not x.Open then FOrm1.Caption := 'Could not connect (2x)';
  cmd.Command := ctRestartElevated;
  Form1.Caption := Form1.Caption + ' ' + IntToStr(SizeOf(cmd));
  x.Write(cmd, SizeOf(cmd));
  cmd.Command := ctPatch;
  Form1.Caption := Form1.Caption + ' ' + IntToStr(SizeOf(cmd));
  x.Write(cmd, SizeOf(cmd));
  cmd.Command := ctUnpatch;
  Form1.Caption := Form1.Caption + ' ' + IntToStr(SizeOf(cmd));
  x.Write(cmd, SizeOf(cmd));
  cmd.Command := ctVerifyPatch;
  Form1.Caption := Form1.Caption + ' ' + IntToStr(SizeOf(cmd));
  x.Write(cmd, SizeOf(cmd));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  res: THelperSvcCommandResponse;
  cmd: THelperSvcCommand;
begin
  if x = nil then x := TNamedPipeClient.Create(PIPENAME);
  if not x.Open then FOrm1.Caption := 'Could not connect (2x)';
  cmd.Command := ctRestartElevated;
  x.Write(cmd, SizeOf(cmd));
  while x.BytesAvailable < SizeOf(res) do sleep(1);
  x.Read(res, SizeOf(res));
  if res.Result then Form1.Caption  := FOrm1.Caption + 'SUCCESS';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  x.Free;
end;

end.

