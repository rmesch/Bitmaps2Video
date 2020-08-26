program DemoBitmaps2Video;

uses
  Vcl.Forms,
  UDemoBitmaps2Video in 'UDemoBitmaps2Video.pas' {Form1},
  UBitmaps2Video in '..\EncoderClassWin\UBitmaps2Video.pas',
  UFormats in '..\EncoderClassWin\UFormats.pas',
  UTools in '..\EncoderClassWin\UTools.pas',
  FFMPEG in '..\FFMPeg\Source\FFMPEG.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

