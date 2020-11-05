program MultiDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMultiDemo in 'UMultiDemo.pas' {Form1},
  FFMPEG in '..\FFMPeg\Source\FFMPEG.pas',
  UBitmaps2VideoM in '..\EncoderClassMulti\UBitmaps2VideoM.pas',
  UFormatsM in '..\EncoderClassMulti\UFormatsM.pas',
  UToolsM in '..\EncoderClassMulti\UToolsM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.InvertedLandscape];
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
