unit UMultiDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ExtCtrls, FMX.StdCtrls, FMX.ListBox, FMX.Controls.Presentation, FMX.Edit,
  FMX.EditBox,
{$IFDEF ANDROID}
  System.Permissions,
  Androidapi.Jni.Os,
  Androidapi.Helpers,
{$ENDIF}
  FMX.SpinBox;

type
  TMovieProc = procedure(const filename: string) of object;

type
  TForm1 = class(TForm)
    Button1: TButton;
    SD: TSaveDialog;
    Label2: TLabel;
    RateCombo: TComboBox;
    Label1: TLabel;
    SpinBox1: TSpinBox;
    Label3: TLabel;
    CodecCombo: TComboBox;
    ProgressBar1: TProgressBar;
    Button2: TButton;
    SlideshowPic1: TImageControl;
    SlideshowPic2: TImageControl;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
{$IFDEF ANDROID}
    FPermissionWriteExtStorage: string;
{$ENDIF}
    MovieProc: TMovieProc;
    /// <summary>
    /// This event will be called when the user answers the request to grant
    /// the necessary write permission to the external storage
    /// </summary>
{$IFDEF ANDROID}
    procedure LocationPermissionRequestResult(Sender: TObject;
      const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>);
{$ENDIF}
  public
    procedure MakeMovie(const filename: string);
    procedure MakeSlideshow(const filename: string);
    procedure ShowProgress(Videotime: int64);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  UFormatsM, UBitmaps2VideoM, FFMPEG,
  math, IOUtils, System.Threading;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Sender = Button1 then
    MovieProc := MakeMovie
  else
    MovieProc := MakeSlideshow;
{$IFDEF ANDROID}
  FPermissionWriteExtStorage :=
    JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE);
  PermissionsService.RequestPermissions([FPermissionWriteExtStorage],
    LocationPermissionRequestResult);
{$ELSE}
  if SD.Execute then
    MovieProc(SD.filename);
{$ENDIF}
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Index: integer;
begin
  ListSupportedCodecs('.mp4', CodecCombo.Items, Index);
  CodecCombo.ItemIndex := Index;
end;

{$IFDEF ANDROID}

procedure TForm1.LocationPermissionRequestResult(Sender: TObject;
  const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>);
begin
  // Check whether the user has allowed writing to the file system
  if (length(AGrantResults) > 0) and
    (AGrantResults[0] = TPermissionStatus.Granted) and
    (APermissions[0] = FPermissionWriteExtStorage) then
  begin
    MovieProc(TPath.Combine(TPath.GetSharedDownloadsPath, 'Video.mp4'));
  end;
end;
{$ENDIF}

procedure TForm1.MakeMovie(const filename: string);
var
  bme: TBitmapEncoderM;
  t: double; // time in seconds
  TheColor: TBGR;
  PeriodRed, PeriodGreen, PeriodBlue: double;
  Width, Height: integer;
  FrameTime: double;
  CodecID: TAVCodecID;
  FrameRate: integer;

  aTask: ITask;
begin
  aTask := TTask.Create(
    procedure
    begin
      Width := 1080;
      Height := 720;
      CodecID := AV_CODEC_ID_MJPEG;
      if CodecCombo.ItemIndex >= 0 then
        CodecID := TAVCodecID(TCodecIdWrapper(CodecCombo.Items.Objects
          [CodecCombo.ItemIndex]).CodecID);
      FrameRate := StrToInt(RateCombo.Items.Strings[RateCombo.ItemIndex]);
      FrameTime := 1 / FrameRate;
      bme := TBitmapEncoderM.Create(filename, Width, Height, FrameRate,
        trunc(SpinBox1.Value), CodecID, vsBicubic);
      try
        bme.OnProgress := ShowProgress;
        PeriodRed := 1 / 8;
        PeriodGreen := 1 / 10.4; // actually 1/period
        PeriodBlue := 1 / 12.7;
        t := 0;
        // 30seconds of movie
        While t < 30 do
        begin
          TheColor.Red := trunc(127 * (sin(2 * Pi * PeriodRed * t) + 1.01));
          TheColor.Green := trunc(127 * (sin(2 * Pi * PeriodGreen * t) + 1.01));
          TheColor.Blue := trunc(127 * (sin(2 * Pi * PeriodBlue * t) + 1.01));
          bme.AddColorFrame(TheColor);
          t := t + FrameTime;
        end;
        bme.CloseFile;
      finally
        bme.free;
      end;
    end);
  aTask.Start;
end;

procedure TForm1.MakeSlideshow(const filename: string);
var
  bme: TBitmapEncoderM;
  Width, Height: integer;
  CodecID: TAVCodecID;
  FrameRate: integer;
  aTask: ITask;
  bm: TBitmap;
  i: integer;
begin
  aTask := TTask.Create(
    procedure
    begin
      bm := TBitmap.Create;
      try
        Height := 720;
        Width := round(Height * SlideshowPic1.Bitmap.Width /
          SlideshowPic1.Bitmap.Height);
        if odd(Width) then
          dec(Width);
        CodecID := AV_CODEC_ID_MJPEG;
        if CodecCombo.ItemIndex >= 0 then
          CodecID := TAVCodecID(TCodecIdWrapper(CodecCombo.Items.Objects
            [CodecCombo.ItemIndex]).CodecID);
        FrameRate := StrToInt(RateCombo.Items.Strings[RateCombo.ItemIndex]);
        bme := TBitmapEncoderM.Create(filename, Width, Height, FrameRate,
          trunc(SpinBox1.Value), CodecID, vsBicubic);
        try
          bme.OnProgress := ShowProgress;
          i:=1;
          while i<4 do
          begin
            bm.Assign(SlideshowPic1.Bitmap);
            bme.AddStillImage(bm, 5000);
            bm.Assign(SlideshowPic2.Bitmap);
            bme.AddStillImage(bm, 5000);
            inc(i);
          end;
          bme.CloseFile;
        finally
          bme.free;
        end;
      finally
        bm.free;
      end;
    end);
  aTask.Start;
end;

procedure TForm1.ShowProgress(Videotime: int64);
begin
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      ProgressBar1.Value := Videotime;
    end);
end;

end.
