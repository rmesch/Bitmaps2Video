program MultiDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMultiDemo in 'UMultiDemo.pas' {Form1},
  FFMPEG in '..\FFMPeg\Source\FFMPEG.pas',
  UBitmaps2VideoM in '..\EncoderClassMulti\UBitmaps2VideoM.pas',
  UFormatsM in '..\EncoderClassMulti\UFormatsM.pas',
  UToolsM in '..\EncoderClassMulti\UToolsM.pas'
  {$IFDEF ANDROID}
  ,
  UAndroidTools in 'UAndroidTools.pas'
  {$ENDIF};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.InvertedLandscape];
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
