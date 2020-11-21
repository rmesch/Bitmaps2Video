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
  UAndroidTools,
{$ENDIF}
  FMX.SpinBox, FMX.TabControl, FMX.MediaLibrary, FMX.Platform,
  System.Messaging, FMX.ScrollBox, FMX.Memo, FFMPEG;

type
  TMovieProc = procedure(const filename: string) of object;

type
  TForm1 = class(TForm)
    TBC: TTabControl;
    TabSimple: TTabItem;
    TabInsertVideo: TTabItem;
    PanelSettings: TPanel;
    Label2: TLabel;
    RateCombo: TComboBox;
    Label1: TLabel;
    SpinBox1: TSpinBox;
    Label3: TLabel;
    CodecCombo: TComboBox;
    Label4: TLabel;
    Edit1: TEdit;
    Label5: TLabel;
    FormatCombo: TComboBox;
    Label6: TLabel;
    HeightCombo: TComboBox;
    Label7: TLabel;
    OD: TOpenDialog;
    PanelSimple: TPanel;
    SlideshowPic1: TImageControl;
    SlideshowPic2: TImageControl;
    Button1: TButton;
    Button2: TButton;
    ProgressBar1: TProgressBar;
    Label8: TLabel;
    PanelInsertVideo: TPanel;
    ButtonLoadVideo: TButton;
    ImageControlFrame: TImageControl;
    Memo1: TMemo;
    Edit2: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    SpinBoxFrameNo: TSpinBox;
    Label11: TLabel;
    Label12: TLabel;
    ProgressBar2: TProgressBar;
    Label13: TLabel;
    Button3: TButton;
    Label14: TLabel;
    Label15: TLabel;
    AspectCombo: TComboBox;
    Label16: TLabel;
    Label17: TLabel;
    BgCombo: TComboBox;
    Button4: TButton;
    TabItem1: TTabItem;
    Button5: TButton;
    Label18: TLabel;
    OAD: TOpenDialog;
    Label19: TLabel;
    Button6: TButton;
    Button7: TButton;
    Label20: TLabel;
    ZoomOptionCombo: TComboBox;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    ScrollBox1: TScrollBox;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormatComboChange(Sender: TObject);
    procedure ButtonLoadVideoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinBoxFrameNoChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure SlideshowPic1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
{$IFDEF ANDROID}
    FPermissionWriteExtStorage: string;
{$ENDIF}
    MovieProc: TMovieProc;
    fVideoFilename, fOutputFile, fAudioFilename: string;
    procedure UpdateCodecCombo;
{$IFDEF ANDROID}
    /// <summary>
    /// This event will be called when the user answers the request to grant
    /// the necessary write permission to the external storage
    /// </summary>
    procedure LocationPermissionRequestResult(Sender: TObject;
      const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>);
{$ENDIF}
    procedure BrowseForVideoFile;
    procedure ProcessVideoFile;
    procedure GrabAFrame;
    /// Example how to safely(?) use the canvas of a TBitmap from threads
    procedure MakeTextBitmap(const bm: TBitmap; const text: String;
      cb, ct: Cardinal);
    procedure WriteTextOnBitmap(const bm: TBitmap; const text: String;
      ct: Cardinal);
    procedure InsertVideo(NewW, NewH, FrameRate: integer; CodecID: TAVCodecId);
    procedure AddAudio;
  public
    TheProgressbar: TProgressBar;
    procedure MakeMovie(const filename: string);
    procedure MakeSlideshow(const filename: string);
    procedure ShowProgress(Videotime: int64);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.SmXhdpiPh.fmx ANDROID}
{$R *.NmXhdpiPh.fmx ANDROID}

uses
  UFormatsM, UBitmaps2VideoM,
  math, IOUtils, System.Threading, FMX.DialogService,
  UToolsM, System.UIConsts, System.Diagnostics, System.Timespan
{$IFDEF Android}
    , Posix.Unistd, Androidapi.Jni.JavaTypes
{$ENDIF}
{$IFDEF MSWINDOWS}
    , winapi.shellapi
{$ENDIF};

const
  MovieHeights: array [0 .. 2] of integer = (720, 540, 1080);
  AspectRatios: array [0 .. 3] of double = (4 / 3, 3 / 2, 16 / 9, 16 / 10);
  BgColors: array [0 .. 3] of byte = (0, 255, 40, 200);

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Sender = Button1 then
    MovieProc := MakeMovie
  else
    MovieProc := MakeSlideshow;
{$IFDEF ANDROID}
  fOutputFile := TPath.Combine(TPath.GetSharedDownloadsPath,
    Edit1.text + FormatCombo.Items[FormatCombo.ItemIndex]);
{$ELSE}
  fOutputFile := TPath.Combine(TPath.GetMoviesPath,
    Edit1.text + FormatCombo.Items[FormatCombo.ItemIndex]);
{$ENDIF}
  MovieProc(fOutputFile);
end;

procedure TForm1.MakeTextBitmap(const bm: TBitmap; const text: String;
  cb, ct: Cardinal);
var
  am: TBitmap;
begin
  // Run this in the main thread
  TThread.Synchronize(nil,
    procedure
    begin
      // Use the canvas of a temporary bitmap
      am := TBitmap.Create;
      try
        am.SetSize(bm.Width, bm.Height);
        am.Canvas.BeginScene;
        am.Canvas.Clear(cb);
        am.Canvas.Font.Size := 30;
        am.Canvas.Fill.Color := ct;
        am.Canvas.FillText(RectF(0, 0, bm.Width, bm.Height), text, False, 1, [],
          TTextAlign.Center, TTextAlign.Center);
        am.Canvas.EndScene;
        // Copy the pixels to the result-bitmap
        bm.CopyFromBitmap(am);
      finally
        am.Free;
      end;
    end);
end;

procedure TForm1.WriteTextOnBitmap(const bm: TBitmap; const text: String;
ct: Cardinal);
var
  am: TBitmap;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      am := TBitmap.Create;
      try
        am.SetSize(bm.Width, bm.Height);
        am.CopyFromBitmap(bm);
        am.Canvas.BeginScene;
        am.Canvas.Font.Size := 30;
        am.Canvas.Fill.Color := ct;
        am.Canvas.FillText(RectF(0, 0, bm.Width, bm.Height), text, True, 1, [],
          TTextAlign.Center, TTextAlign.Leading);
        am.Canvas.EndScene;
        bm.CopyFromBitmap(am);
      finally
        am.Free;
      end;
    end);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  P: TVideoProps;
  NewW, NewH: integer;
  CodecID: TAVCodecId;
  FrameRate: integer;
  aTask: TThread;
begin
  if fVideoFilename = '' then
  begin
    TDialogService.ShowMessage
      ('Browse for a video file first. Under Android pick it by opening Documents');
    Exit;
  end;
  P := GetVideoProps(fVideoFilename);
  ProgressBar2.Max := 0.001 * P.Duration + 5;
  ProgressBar2.Value := 0;
  TheProgressbar := ProgressBar2;
  NewH := MovieHeights[HeightCombo.ItemIndex];
  NewW := Round(NewH * AspectRatios[AspectCombo.ItemIndex]);
  if odd(NewW) then
    dec(NewW);
  CodecID := AV_CODEC_ID_NONE;
  if CodecCombo.ItemIndex >= 0 then
    CodecID := TAVCodecId(TCodecIdWrapper(CodecCombo.Items.Objects
      [CodecCombo.ItemIndex]).CodecID);
  FrameRate := StrToInt(RateCombo.Items.Strings[RateCombo.ItemIndex]);
{$IFDEF ANDROID}
  fOutputFile := TPath.Combine(TPath.GetSharedDownloadsPath,
    Edit1.text + FormatCombo.Items[FormatCombo.ItemIndex]);
{$ELSE}
  fOutputFile := TPath.Combine(TPath.GetMoviesPath,
    Edit1.text + FormatCombo.Items[FormatCombo.ItemIndex]);
{$ENDIF}
  if Sender = Button3 then
  begin
    aTask := TThread.CreateAnonymousThread(
      procedure
      begin
        InsertVideo(NewW, NewH, FrameRate, CodecID);
      end);
    aTask.Start;
  end
  else
    InsertVideo(NewW, NewH, FrameRate, CodecID);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  If not FileExists(fOutputFile) then
  begin
    TDialogService.ShowMessage('Create a video first');
    Exit;
  end;
{$IFDEF ANDROID}
  TFileBrowser.BrowseForFile(ctAudio,
    procedure(var filename: string; var success: boolean)
    begin
      if success then
      begin
        fAudioFilename := filename;
        AddAudio;
      end
      else
        TDialogService.ShowMessage
          ('Pick the file by opening Documents, don''t use any of the other apps.');
    end);
{$ELSE}
  if OAD.Execute then
  begin
    fAudioFilename := OAD.filename;
    AddAudio;
  end;
{$ENDIF}
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if not FileExists(fOutputFile) then
  begin
    TDialogService.ShowMessage('Make a video first');
    Exit;
  end;
{$IFDEF ANDROID}
  PlayVideo(fOutputFile);
{$ELSE}
  ShellExecute(0, 'Open', PChar(fOutputFile), nil, nil, 1);
{$ENDIF}
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  bm: TBitmap;
  bme: TBitmapEncoderM;
  h, w, bw, bh: integer;
  ZoomTarget, ZoomSource, ZoomTarget2: TZoom;
  ZoomOption: TZoomOption;
  VideoAspect: double;
  CodecID: TAVCodecId;
  FrameRate: integer;
  aTask: TThread;
  Stopwatch: TStopwatch;
begin
  aTask := TThread.CreateAnonymousThread(
    procedure
    begin
      Stopwatch := TStopwatch.StartNew;
      VideoAspect := AspectRatios[AspectCombo.ItemIndex];
      bm := TBitmap.Create;
      try
        bm.Assign(SlideshowPic1.Bitmap);
        bw := bm.Width;
        bh := bm.Height;
        h := StrToInt(HeightCombo.Items[HeightCombo.ItemIndex]);
        w := Round(h * VideoAspect);
        if odd(w) then
          w := w - 1;
        ZoomSource := MakeZoom(0, 0, 1);
        // half of original size, offset by left, top which are random in the top-left quarter
        ZoomTarget2 := MakeZoom(0.5 * random, 0.5 * random, 0.5);
        // 0.3 times original size, centered
        ZoomTarget := MakeZoom(0.35, 0.35, 0.3);
        ZoomOption := zoAAx2; // eliminate compiler warning
        case ZoomOptionCombo.ItemIndex of
          0:
            ZoomOption := zoAAx2;
          1:
            ZoomOption := zoAAx4;
          2:
            ZoomOption := zoAAx6;
          3:
            ZoomOption := zoResample;
        end;

        CodecID := AV_CODEC_ID_NONE;
        if CodecCombo.ItemIndex >= 0 then
          CodecID := TAVCodecId(TCodecIdWrapper(CodecCombo.Items.Objects
            [CodecCombo.ItemIndex]).CodecID);
        FrameRate := StrToInt(RateCombo.Items.Strings[RateCombo.ItemIndex]);
{$IFDEF ANDROID}
        fOutputFile := TPath.Combine(TPath.GetSharedDownloadsPath,
          Edit1.text + FormatCombo.Items[FormatCombo.ItemIndex]);
{$ELSE}
        fOutputFile := TPath.Combine(TPath.GetMoviesPath,
          Edit1.text + FormatCombo.Items[FormatCombo.ItemIndex]);
{$ENDIF}
        bme := TBitmapEncoderM.Create(fOutputFile, w, h, FrameRate,
          trunc(SpinBox1.Value), CodecID, vsBicubic,
          BgColors[BgCombo.ItemIndex]);

        try
          TheProgressbar := ProgressBar1;
          ProgressBar1.Max := 27;
          ProgressBar1.Value := 0;
          bme.OnProgress := ShowProgress;
          // 28 seconds of movie
          bme.AddStillImage(bm, 1000);

          bme.ZoomPan(bm, ZoomSource, ZoomTarget, 6000, ZoomOption, zeFastSlow);
          bme.Freeze(1000);
          bme.ZoomPan(bm, ZoomTarget, ZoomTarget2, 5000, ZoomOption,
            zeSlowSlow);
          bme.Freeze(1000);
          bme.ZoomPan(bm, ZoomTarget2, ZoomTarget, 5000, ZoomOption,
            zeSlowFast);
          bme.Freeze(1000);
          // "transition"
          bme.ZoomPan(bm, ZoomTarget, MakeZoom(0.4, 0.4, 0.2), 1000, ZoomOption,
            zeSlowSlow);
          bm.Assign(SlideshowPic2.Bitmap);
          bme.ZoomPan(bm, MakeZoom(0.4, 0.4, 0.2), ZoomTarget, 1000,
            ZoomOption, zeFastSlow);
          bme.ZoomPan(bm, ZoomTarget, ZoomSource, 5000, ZoomOption,
            zeSlowSlow);
          bme.Freeze(1000);
          bme.CloseFile; // movie should be done
          TThread.Synchronize(TThread.Current,
            procedure
            begin
              Label22.text := 'Writing speed: ' + FloatToStrF(bme.FrameCount /
                Stopwatch.Elapsed.Seconds, ffFixed, 6, 2) + ' fps';
            end);
        finally
          bme.Free;
        end;
      finally
        bm.Free;
      end;

    end);
  aTask.Start;
end;

procedure TForm1.AddAudio;
var
  Temp: string;
begin
  Temp := TPath.Combine(TPath.GetTempPath, 'TempVideo' + FormatCombo.Items
    [FormatCombo.ItemIndex]);
  TFile.Copy(fOutputFile, Temp, True);
  try
    MuxStreams2(Temp, fAudioFilename, fOutputFile);
    Label18.text := Edit1.text + ' now contains ' + fAudioFilename +
      ' as audio stream';
  except
    TFile.Copy(Temp, fOutputFile, True);
    Label18.text :=
      'The formats are not compatible, try another video format or another audio file.'
  end;
  DeleteFile(Temp);
end;

procedure TForm1.InsertVideo(NewW, NewH, FrameRate: integer;
CodecID: TAVCodecId);
var
  bme: TBitmapEncoderM;
  am, bm: TBitmap;
  Stopwatch: TStopwatch;
begin
  Stopwatch := TStopwatch.StartNew;
  bme := TBitmapEncoderM.Create(fOutputFile, NewW, NewH, FrameRate,
    trunc(SpinBox1.Value), CodecID, vsBicubic, BgColors[BgCombo.ItemIndex]);
  bme.OnProgress := ShowProgress;
  try
    bm := TBitmap.Create;
    try
      GrabFrame(bm, fVideoFilename, 1);
      am := TBitmap.Create;
      try
        am.Assign(bm);
        WriteTextOnBitmap(bm, ' '#13'THE START', claDarkRed);
        bme.AddStillImage(bm, 3000);
        bme.CrossFade(bm, am, 1000);
      finally
        am.Free;
      end;
    finally
      bm.Free;
    end;
    bme.AddVideo(fVideoFilename);
    bm := TBitmap.Create;
    try
      bm.SetSize(NewW, NewH);
      MakeTextBitmap(bm, 'THE END', claBlack, claYellow);
      am := TBitmap.Create;
      try
        GrabFrame(am, fVideoFilename, bme.LastVideoFrameCount - 1);
        bme.CrossFade(am, bm, 1000);
      finally
        am.Free;
      end;
      bme.AddStillImage(bm, 1000);
      bme.ZoomPan(bm, MakeZoom(0, 0, 1), MakeZoom(0.2, 0.2, 0.6), 2000,
        TZoomOption.zoAAx4, TZoomSpeedEnvelope.zeSlowSlow);
      bme.Freeze(2000);
      TThread.Synchronize(TThread.Current,
        procedure
        begin
          Label23.text := 'Writing speed: ' + FloatToStrF(bme.FrameCount /
            Stopwatch.Elapsed.Seconds, ffFixed, 6, 2) + ' fps';
        end);
    finally
      bm.Free;
    end;
    bme.CloseFile;
  finally
    bme.Free;
  end;
end;

procedure TForm1.GrabAFrame;
var
  bm: TBitmap;
begin
  bm := TBitmap.Create;
  try
    GrabFrame(bm, fVideoFilename, Round(SpinBoxFrameNo.Value));
    ImageControlFrame.Bitmap.Assign(bm);
  finally
    bm.Free;
  end;
end;

procedure TForm1.ButtonLoadVideoClick(Sender: TObject);
begin
  BrowseForVideoFile;
end;

procedure TForm1.ProcessVideoFile;
var
  P: TVideoProps;
begin
  P := GetVideoProps(fVideoFilename);
  Memo1.lines.Clear;
  With Memo1.lines do
  begin
    add('Properties of');
    add(ExtractFilename(fVideoFilename) + ':');
    add('Width: ' + IntTostr(P.Width));
    add('True Height: ' + IntTostr(P.TrueHeight));
    add('Frame rate: ' + IntTostr(P.FrameRate));
    add('No. video streams: ' + IntTostr(P.nrVideostreams));
    add('No. audio streams: ' + IntTostr(P.nrAudiostreams));
    add('Duration: ' + FloatToStrF(0.001 * P.Duration, ffFixed, 8, 2) + ' sec');
    add('Video codec: ' + avCodec_Find_Decoder(P.VideoCodec).name);
  end;
  Memo1.Repaint;
  GrabAFrame;
end;

procedure TForm1.BrowseForVideoFile;
begin
{$IFDEF ANDROID}
  TFileBrowser.BrowseForFile(ctVideo,
  // This procedure is called when the user has picked an item
    procedure(var filename: string; var success: boolean)
    begin
      if success then
      begin
        fVideoFilename := filename;
        Edit2.text := fVideoFilename;
        ProcessVideoFile;
      end
      else
        TDialogService.ShowMessage
          ('Pick the file by opening the Documents folder. Don''t use any of the other apps.');
    end);

{$ELSE}
  if OD.Execute then
  begin
    fVideoFilename := OD.filename;
    Edit2.text := fVideoFilename;
    ProcessVideoFile;
  end;
{$ENDIF}
end;

procedure TForm1.FormatComboChange(Sender: TObject);
begin
  UpdateCodecCombo;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateCodecCombo;
end;

procedure TForm1.FormResize(Sender: TObject);
var fact: single;
begin
  fact:=max(min(clientwidth/panel1.width,clientheight/panel1.Height),0.7);
  panel1.Scale.X:=fact;
  panel1.Scale.Y:=fact;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
{$IFDEF ANDROID}
  FPermissionWriteExtStorage :=
    JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE);
  PermissionsService.RequestPermissions([FPermissionWriteExtStorage],
    LocationPermissionRequestResult);
{$ENDIF}
end;

procedure TForm1.UpdateCodecCombo;
var
  Index: integer;
begin
  ListSupportedCodecs(FormatCombo.Items[FormatCombo.ItemIndex],
    CodecCombo.Items, Index);
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
  else
  begin
    FMX.DialogService.TDialogService.PreferredMode :=
      TDialogService.TPreferredMode.Sync;
    TDialogService.ShowMessage
      ('For this app to function it needs your permission to access media files. Without this permission it does nothing.');
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
  CodecID: TAVCodecId;
  FrameRate: integer;
  asp: double;
  aTask: iTask;
begin
  ProgressBar1.Max := 18;
  TheProgressbar := ProgressBar1;
  aTask := TTask.Create(
    procedure
    begin
      Height := MovieHeights[HeightCombo.ItemIndex];
      asp := AspectRatios[AspectCombo.ItemIndex];
      Width := Round(asp * Height);
      CodecID := AV_CODEC_ID_NONE;
      if CodecCombo.ItemIndex >= 0 then
        CodecID := TAVCodecId(TCodecIdWrapper(CodecCombo.Items.Objects
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
        // 18 seconds of movie
        While t < 18 do
        begin
          TheColor.Red := trunc(127 * (sin(2 * Pi * PeriodRed * t) + 1.01));
          TheColor.Green := trunc(127 * (sin(2 * Pi * PeriodGreen * t) + 1.01));
          TheColor.Blue := trunc(127 * (sin(2 * Pi * PeriodBlue * t) + 1.01));
          bme.AddColorFrame(TheColor);
          t := t + FrameTime;
        end;
        // Hold end frame for 2 seconds
        bme.Freeze(2000);
        bme.CloseFile;
      finally
        bme.Free;
      end;
    end);
  aTask.Start;
end;

procedure TForm1.MakeSlideshow(const filename: string);
var
  bme: TBitmapEncoderM;
  Width, Height: integer;
  CodecID: TAVCodecId;
  FrameRate: integer;
  aTask: iTask;
  am, bm: TBitmap;
  I: integer;
  asp: double;
begin
  ProgressBar1.Max := 22;
  TheProgressbar := ProgressBar1;
  aTask := TTask.Create(
    procedure
    begin
      bm := TBitmap.Create;
      try
        bm.Assign(SlideshowPic1.Bitmap);
        am := TBitmap.Create;
        try
          am.Assign(SlideshowPic2.Bitmap);
          Height := MovieHeights[HeightCombo.ItemIndex];
          asp := AspectRatios[AspectCombo.ItemIndex];
          Width := Round(Height * asp);
          if odd(Width) then
            dec(Width);
          CodecID := AV_CODEC_ID_NONE;
          if CodecCombo.ItemIndex >= 0 then
            CodecID := TAVCodecId(TCodecIdWrapper(CodecCombo.Items.Objects
              [CodecCombo.ItemIndex]).CodecID);
          FrameRate := StrToInt(RateCombo.Items.Strings[RateCombo.ItemIndex]);
          bme := TBitmapEncoderM.Create(filename, Width, Height, FrameRate,
            trunc(SpinBox1.Value), CodecID, vsBicubic,
            BgColors[BgCombo.ItemIndex]);
          try
            bme.OnProgress := ShowProgress;
            // 23 sec of movie
            I := 1;
            while I < 4 do
            begin
              if I > 1 then
                bme.CrossFade(am, bm, 1000);
              bme.AddStillImage(bm, 3000);
              bme.CrossFade(bm, am, 1000);
              bme.AddStillImage(am, 3000);
              inc(I);
            end;
            // Hold end frame for 2 seconds
            bme.Freeze(2000);
            bme.CloseFile;
          finally
            bme.Free;
          end;
        finally
          am.Free;
        end;
      finally
        bm.Free;
      end;
    end);
  aTask.Start;
end;

procedure TForm1.ShowProgress(Videotime: int64);
begin
  TThread.Queue(nil,
    procedure
    begin
      TheProgressbar.Value := 0.001 * Videotime;
    end);
end;

procedure TForm1.SlideshowPic1Click(Sender: TObject);
begin
{$IFDEF ANDROID}
  TFileBrowser.BrowseForFile(ctImages,
    procedure(var filename: string; var success: boolean)
    begin
      if success then
        TImageControl(Sender).LoadFromFile(filename)
      else
        TDialogService.ShowMessage
          ('Open Documents to load the picture, don''t use any of the other apps.');
    end);
{$ENDIF}
end;

procedure TForm1.SpinBoxFrameNoChange(Sender: TObject);
begin
  if fVideoFilename = '' then
  begin
    TDialogService.ShowMessage
      ('Browse for a video file first. Under Android pick it by opening Documents');
    Exit;
  end;
  GrabAFrame;
end;

end.
