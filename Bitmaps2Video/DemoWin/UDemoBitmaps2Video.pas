unit UDemoBitmaps2Video;

interface

{.$DEFINE UseBass}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UBitmaps2Video, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, Vcl.ExtCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  Vcl.Imaging.pngImage,
  Vcl.ExtDlgs, UFormats, FFMPeg,
  System.Generics.Collections;

type

  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    ImageZoomPicture: TImage;
    ButtonLoadZoomPicture: TButton;
    rgrpZoom: TRadioGroup;
    ButtonMakeZoomMovie: TButton;
    Label4: TLabel;
    ProgressBar1: TProgressBar;
    Label9: TLabel;
    ButtonLoadVideoClip: TButton;
    Button6: TButton;
    ButtonAddAudio: TButton;
    Label5: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Memo1: TMemo;
    Label15: TLabel;
    Label16: TLabel;
    ProgressBar2: TProgressBar;
    MemoProps: TMemo;
    ImageShowFrame: TImage;
    FrameSpin: TSpinEdit;
    Label17: TLabel;
    EditVideoFile: TEdit;
    Label19: TLabel;
    ButtonInsertClipMainThread: TButton;
    ButtonInsertClipBackgroundThread: TButton;
    TabSheet5: TTabSheet;
    Panel1: TPanel;
    ButtonStartPlaying: TButton;
    ButtonStopPlaying: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label18: TLabel;
    EditOutput: TEdit;
    ButtonOpenOutput: TButton;
    SpinEditQuality: TSpinEdit;
    HC: TComboBox;
    CodecCombo: TComboBox;
    RateCombo: TComboBox;
    FormatCombo: TComboBox;
    ButtonRegisterNewCodec: TButton;
    RadioGroupAspect: TRadioGroup;
    BgCombo: TComboBox;
    SD: TSaveDialog;
    OPD: TOpenPictureDialog;
    OD: TOpenDialog;
    TabSheet6: TTabSheet;
    ButtonAnimation: TButton;
    ProgressBar3: TProgressBar;
    Panel4: TPanel;
    ButtonPlayOutput: TButton;
    OVD: TFileOpenDialog;
    ButtonLoadVideoForPlaying: TButton;
    TrackBar1: TTrackBar;
    Label20: TLabel;
    ImageAni: TImage;
    Label21: TLabel;
    TabSheet7: TTabSheet;
    ScrollboxSlideshow: TScrollBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    ButtonMakeSlideshow: TButton;
    ProgressBar4: TProgressBar;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    procedure ButtonMakeZoomMovieClick(Sender: TObject);
    procedure ButtonOpenOutputClick(Sender: TObject);
    procedure ButtonLoadZoomPictureClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonAddAudioClick(Sender: TObject);
    procedure FormatComboChange(Sender: TObject);
    procedure ButtonRegisterNewCodecClick(Sender: TObject);
    procedure HCChange(Sender: TObject);
    procedure RateComboChange(Sender: TObject);
    procedure SpinEditQualityChange(Sender: TObject);
    procedure CodecComboChange(Sender: TObject);
    //procedure Button6Click(Sender: TObject);
    procedure ButtonLoadVideoClipClick(Sender: TObject);
    procedure FrameSpinChange(Sender: TObject);
    procedure ButtonInsertClipMainThreadClick(Sender: TObject);
    procedure ButtonStartPlayingClick(Sender: TObject);
    procedure ButtonStopPlayingClick(Sender: TObject);
    procedure ButtonAnimationClick(Sender: TObject);
    procedure ButtonPlayOutputClick(Sender: TObject);
    procedure ButtonLoadVideoForPlayingClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure ButtonMakeSlideshowClick(Sender: TObject);
  private
    fVideoFile, fPlayerFile: string;
    fStopVideo: boolean;
    fSlideshowList: TList<TImage>;
    procedure UpdateCodecCombo;
    procedure UpdateSizeinMB;
    procedure InsertVideo(UpdateProc: TVideoProgressEvent);
    function GetFrameRate: integer;
    function GetVideoHeight: integer;
    function GetVideoWidth: integer;
    function GetOutputFile: string;
    function GetEncoderId: TAVCodecID;
    function GetEncodingQuality: Byte;
    function GetVideoBackGround: Byte;
    { Private declarations }
  public
    { Public declarations }
    TheProgressbar: TProgressBar;
    procedure UpdateVideo(Videotime: int64);
    procedure UpdateVideoThreaded(Videotime: int64);
    //properties translating the setup controls' state into values
    //to pass to TBitmapEncoder.Create.
    property FrameRate: integer read GetFrameRate;
    property VideoHeight: integer read GetVideoHeight;
    property VideoWidth: integer read GetVideoWidth;
    property OutputFile: string read GetOutputFile;
    property EncoderId: TAVCodecID read GetEncoderId;
    property EncodingQuality: Byte read GetEncodingQuality;
    property VideoBackGround: Byte read GetVideoBackGround;
  end;

  // Example for a new TCodecSetupClass
  TRawSetup = class(TBaseCodecSetup)
  public
    Constructor Create(CodecID: TAVCodecID); override;
    // for raw video the bitrate is automatic, so we return 0
    function QualityToBitrate(Quality: Byte; Width, Height, Rate: integer)
      : int64; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses mmSystem, System.Types, math, UTools, Winapi.ShlObj, Winapi.ActiveX,
  System.Threading, Winapi.ShellApi
{$IFDEF UseBass}
    , Bass
{$ENDIF}
    ;

const
  Aspects: array [0 .. 2] of double = (16 / 9, 4 / 3, 3 / 2);
  BgColors: array [0 .. 3] of Byte = (0, 255, 40, 180);

function PIDLToPath(IdList: PItemIDList): string;
begin
  SetLength(Result, MAX_PATH);
  if SHGetPathFromIdList(IdList, PChar(Result)) then
    SetLength(Result, StrLen(PChar(Result)))
  else
    Result := '';
end;

function PidlFree(var IdList: PItemIDList): boolean;
var
  Malloc: IMalloc;
begin
  Result := false;
  if IdList = nil then
    Result := true
  else
  begin
    if Succeeded(SHGetMalloc(Malloc)) and (Malloc.DidAlloc(IdList) > 0) then
    begin
      Malloc.Free(IdList);
      IdList := nil;
      Result := true;
    end;
  end;
end;

function GetDesktopFolder: string;
var
  FolderPidl: PItemIDList;
begin
  if Succeeded(SHGetSpecialFolderLocation(0, $0000, FolderPidl)) then
  begin
    Result := PIDLToPath(FolderPidl);
    PidlFree(FolderPidl);
  end
  else
    Result := '';
end;

procedure TForm1.ButtonStartPlayingClick(Sender: TObject);
{$IFDEF UseBass}
var
  AudStream: HStream;
{$ENDIF}
begin
  ButtonLoadVideoForPlaying.Enabled := false;
  ButtonStartPlaying.Enabled := false;
  ButtonStopPlaying.Enabled := true;
  fStopVideo := false;

{$IFDEF UseBass}
  BASS_Init(-1, 44100, 0, Application.Handle, nil);
  AudStream := 0;
  try
    AudStream := BASS_StreamCreateFile(false, pAnsichar(AnsiString(fPlayerFile)
      ), 0, 0, 0);
  except
    AudStream := 0;
  end;
  if AudStream <> 0 then
    BASS_ChannelSetPosition(AudStream, Bass_ChannelSeconds2Bytes(AudStream,
      TrackBar1.Position), BASS_POS_BYTE);
{$ENDIF}

  // Playing the video on the canvas of a TPanel is less error prone than doing it on a paintbox.
  PlayVideoStream(Panel4, fPlayerFile,
    1000 * TrackBar1.Position,

{$IFDEF UseBass}
    procedure
    begin
      if AudStream <> 0 then
        BASS_ChannelPlay(AudStream, false);
    end,
{$ELSE}
  nil,

{$ENDIF}
    procedure(VideoTimeElapsed: int64; var quit: boolean)
    begin
      Application.ProcessMessages;
      TrackBar1.Position := VideoTimeElapsed div 1000;
      quit := fStopVideo;
{$IFDEF UseBass}
      if quit then
        if AudStream <> 0 then
          BASS_ChannelStop(AudStream)
{$ENDIF}
    end);

{$IFDEF UseBass}
  if AudStream <> 0 then
    Bass_StreamFree(AudStream);
  Bass_Free;
{$ENDIF}
  ButtonLoadVideoForPlaying.Enabled := true;
  ButtonStartPlaying.Enabled := true;
  ButtonStopPlaying.Enabled := false;
  Panel4.Repaint;
end;

procedure TForm1.ButtonStopPlayingClick(Sender: TObject);
begin
  fStopVideo := true;
end;

procedure TForm1.ButtonAnimationClick(Sender: TObject);
var
  i, j, w, h: integer;
  A, r, theta, dtheta: double;
  xCenter, yCenter: integer;
  scale: double;
  bm: TBitmap;
  points: array of TPoint;
  jmax: integer;
  bme: TBitmapEncoder;

  function dist(o: double): double;
  begin
    Result := 2 - 0.2 * o;
  end;

// map from world ([-2,2]x[-2,2]) to bitmap
  function map(p: TPointF): TPoint;
  begin
    Result.x := round(xCenter + scale * p.x);
    Result.y := round(yCenter - scale * p.y);
  end;

begin
  h := VideoHeight;
  w := VideoWidth;
  bme := TBitmapEncoder.Create(OutputFile, w, h, FrameRate, EncodingQuality,
    EncoderId, vsBiCubic, VideoBackGround);
  try
    // VideoTime = Framecount * FrameTime
    ProgressBar3.Max := round(179 * 1000 / FrameRate);
    ProgressBar3.Position := 0;
    TheProgressbar := ProgressBar3;
    bme.OnProgress := UpdateVideo;
    bm := TBitmap.Create;
    try
      // AntiAlias 2*Video-Height
      bm.SetSize(2 * h, 2 * h);
      xCenter := bm.Width div 2;
      yCenter := bm.Height div 2;
      scale := bm.Height / 4;

      bm.Canvas.brush.color := clMaroon;
      bm.Canvas.pen.color := clYellow;
      bm.Canvas.pen.Width := Max(h div 180, 2);

      dtheta := 2 / 150 * pi;

      for i := 0 to 178 do { 179 frames = 179 Polylines }
      begin
        A := 1 - 1 / 200 * i;
        jmax := trunc(10 / A / dtheta);
        SetLength(points, jmax);
        theta := 0;
        for j := 0 to jmax - 1 do
        begin
          r := dist(A * theta);
          points[j] := map(Pointf(r * cos(theta), r * sin(theta)));
          theta := theta + dtheta;
        end;
        bm.Canvas.Fillrect(bm.Canvas.clipRect);
        bm.Canvas.Polyline(points);
        bme.AddFrame(bm);
        if i mod 10 = 0 then
        begin
          ImageAni.Picture.Bitmap := bm;
          ImageAni.Update;
        end;
      end;
      bme.CloseFile;
    finally
      bm.Free;
    end;
  finally
    bme.Free;
  end;
end;

procedure TForm1.ButtonPlayOutputClick(Sender: TObject);
begin
  If not FileExists(OutputFile) then
  begin
    ShowMessage('Make a movie first.');
    exit;
  end;
  ShellExecute(0, 'Open', PChar(OutputFile), nil, nil, 1);
end;

procedure TForm1.ButtonLoadVideoForPlayingClick(Sender: TObject);
var
  p: TVideoProps;
begin
  if not OVD.execute then
    exit;
  fPlayerFile := OVD.filename;
  p := GetVideoProps(fPlayerFile);
  TrackBar1.Max := p.Duration div 1000;
  TrackBar1.Position := 0;
  Label20.Caption := IntToStr(p.Duration div 1000 div 60) + 'min ' +
    IntToStr((p.Duration div 1000) mod 60) + 'sec';
  ButtonStartPlaying.Enabled := true;
end;

procedure TForm1.ButtonMakeSlideshowClick(Sender: TObject);
var
  i: integer;
  bme: TBitmapEncoder;
  bm, cm: TBitmap;
  aTask: iTask;
begin
  ProgressBar4.Max := 7 * 7000 + 7 * 1200;
  ProgressBar4.Position := 0;
  TheProgressbar := ProgressBar4;
  Label22.Caption:='     ';
  aTask := TTask.Create(
    procedure
    begin
      bme := TBitmapEncoder.Create(OutputFile, VideoWidth, VideoHeight,
        FrameRate, EncodingQuality, EncoderId, vsBiCubic, VideoBackGround);
      try
        bme.OnProgress := UpdateVideoThreaded;
        bm := TBitmap.Create;
        try
          cm := TBitmap.Create;
          try
            bm.Assign(fSlideshowList.Items[0].Picture.Graphic);
            i := 1;
            while i < fSlideshowList.Count do
            begin
              bme.AddStillImage(bm, 7000);
              cm.Assign(fSlideshowList.Items[i].Picture.Graphic);
              bme.CrossFade(bm, cm, 1200);
              bm.Assign(cm);
              inc(i);
            end;
            bme.AddStillImage(bm, 6000);
            cm.Canvas.Lock;
            BitBlt(cm.Canvas.Handle, 0, 0, cm.Width, cm.Height, 0, 0, 0,
              BLACKNESS);
            cm.Canvas.Unlock;
            bme.CrossFade(bm, cm, 1200);
          finally
            cm.Free;
          end;
        finally
          bm.Free;
        end;
        bme.CloseFile;
      finally
        bme.Free;
      end;
      TThread.Synchronize(TThread.Current,
        procedure
        begin
          TheProgressbar.Position := TheProgressbar.Max;
          Label22.Caption:='Done';
        end);
    end);
  aTask.Start;
end;

procedure TForm1.ButtonMakeZoomMovieClick(Sender: TObject);
var
  bm: TBitmap;
  bme: TBitmapEncoder;
  h, w: integer;
  t: int64;
  fps: double;
  ZoomTarget, ZoomSource, ZoomTarget2: TZoom;
  ZoomOption: TZoomOption;

begin
  if (not assigned(ImageZoomPicture.Picture.Graphic)) then
  begin
    Vcl.Dialogs.ShowMessage('Load a picture first');
    exit;
  end;
  t := TimeGetTime;
  bm := TBitmap.Create;
  try
    bm.Assign(ImageZoomPicture.Picture.Graphic);
    bm.PixelFormat := pf32bit;

    h := VideoHeight;
    w := VideoWidth;
    ZoomSource := MakeZoom(0, 0, 1);
    // half of original size, offset by left, top which are random in the top-left quarter
    ZoomTarget2 := MakeZoom(0.5 * random, 0.5 * random, 0.5);
    // 0.3 times original size, centered
    ZoomTarget := MakeZoom(0.35, 0.35, 0.3);
    ZoomOption := zoAAx2; // eliminate compiler warning
    case rgrpZoom.ItemIndex of
      0:
        ZoomOption := zoAAx2;
      1:
        ZoomOption := zoAAx4;
      2:
        ZoomOption := zoAAx6;
      3:
        ZoomOption := zoResample;
    end;

    bme := TBitmapEncoder.Create(OutputFile, w, h, FrameRate, EncodingQuality,
      EncoderId, vsBiCubic, VideoBackGround);
    // vsBiCubic: if it needs to scale it scales nicely

    try
      TheProgressbar := ProgressBar1;
      ProgressBar1.Max := 19000;
      ProgressBar1.Position := 0;
      bme.OnProgress := UpdateVideo;
      // 20 seconds of movie
      bme.AddStillImage(bm, 1000);

      bme.ZoomPan(bm, ZoomSource, ZoomTarget, 6000, ZoomOption, zeFastSlow);
      bme.Freeze(1000);
      bme.ZoomPan(bm, ZoomTarget, ZoomTarget2, 5000, ZoomOption, zeSlowSlow);
      bme.Freeze(1000);
      bme.ZoomPan(bm, ZoomTarget2, ZoomSource, 5000, ZoomOption, zeSlowFast);
      bme.Freeze(1000);

      bme.CloseFile; // movie should be done
      t := TimeGetTime - t;
      fps := 1000 * bme.framecount / t;
    finally
      bme.Free;
    end;
  finally
    bm.Free;
  end;

  Label4.Caption := 'Writing Speed: ' + FloatToStrF(fps, ffFixed, 5, 2)
    + ' fps';
end;

procedure TForm1.ButtonOpenOutputClick(Sender: TObject);
begin
  if SD.execute then
  begin
    EditOutput.Text := Copy(SD.filename, 1, length(SD.filename) -
      length(ExtractFileExt(SD.filename)));
    FormatCombo.ItemIndex := FormatCombo.Items.IndexOf
      (ExtractFileExt(SD.filename));
    UpdateCodecCombo;
  end;
end;

procedure TForm1.ButtonLoadZoomPictureClick(Sender: TObject);
begin
  if OPD.execute then
    ImageZoomPicture.Picture.LoadFromFile(OPD.filename);
end;

procedure TForm1.ButtonAddAudioClick(Sender: TObject);
var
  Tempfile, Audiofile: string;
begin
  if not OD.execute then
    exit;
  Try
    Label5.Caption := 'Writing audio';
    Audiofile := OD.filename;
    if not FileExists(OutputFile) then
    begin
      ShowMessage('Make a Movie first');
      exit;
    end;
    Tempfile := GetTempFolder + '\_VideoTemp' + ExtractFileExt(OutputFile);
    CopyFile(PWideChar(OutputFile), PWideChar(Tempfile), false);
    MuxStreams2(Tempfile, Audiofile, OutputFile);
    DeleteFile(Tempfile);
    Label5.Caption := 'Audio track now contains ' + ExtractFilename(Audiofile);
  except
    ShowMessage('Audio format not supported by container format ' +
      ExtractFileExt(OutputFile));
    // Restore orignal video
    CopyFile(PWideChar(Tempfile), PWideChar(OutputFile), false);
  End;
end;

procedure TForm1.ButtonRegisterNewCodecClick(Sender: TObject);
begin
  if RegisterEncoder(AV_CODEC_ID_RAWVIDEO, TRawSetup, false) then
    MessageBeep(0);
  UpdateCodecCombo;
end;

procedure TextOnBitmap(const bm: TBitmap; const _text: string);
var
  r: TRect;
  r1: TRectF;
begin
  bm.Canvas.Lock;
  try
    bm.Canvas.Font.color := clWhite;
    bm.Canvas.brush.style := bsClear;
    bm.Canvas.Font.Height := bm.Height div 10;
    r := Rect(0, 0, bm.Width, bm.Height);
    DrawText(bm.Canvas.Handle, PChar(_text), length(_text), r,
      dt_Center or dt_CalcRect);
    r1 := TRectF(r);
    CenterRect(r1, RectF(0, 0, bm.Width, bm.Height));
    r := r1.round;
    DrawText(bm.Canvas.Handle, PChar(_text), length(_text), r, dt_Center);
  Finally
    bm.Canvas.Unlock;
  End;
end;

procedure BlackBitmap(const bm: TBitmap; w, h: integer);
begin
  bm.PixelFormat := pf32bit;
  bm.SetSize(w, h);
  bm.Canvas.Lock; // necessary for threads
  BitBlt(bm.Canvas.Handle, 0, 0, w, h, 0, 0, 0, BLACKNESS);
  bm.Canvas.Unlock;
end;

{
  procedure TForm1.Button6Click(Sender: TObject);
  var
    bme: TBitmapEncoder;
    bm: TBitmap;

  begin
    if not OVD.execute then
      exit;
    Label15.Caption := 'Working';
    Label15.Repaint;
    bme := TBitmapEncoder.CreateFromVideo(OVD.filename, EditOutput.Text,
      vsBiCubic, BgColors[BgCombo.ItemIndex]);
    try
      bm := TBitmap.Create;
      try
        BlackBitmap(bm, bme.VideoWidth, bme.VideoHeight);
        TextOnBitmap(bm, 'End Slide Added');
        bme.AddStillImage(bm, 4000);
      finally
        bm.Free;
      end;
      bme.CloseFile;
    finally
      bme.Free;
    end;
    Label15.Caption := 'Done';
  end;
}

procedure TForm1.ButtonLoadVideoClipClick(Sender: TObject);
var
  p: TVideoProps;
begin
  if not OVD.execute then
    exit;
  fVideoFile := OVD.filename;
  EditVideoFile.Text := fVideoFile;
  FrameSpinChange(nil);
  p := GetVideoProps(fVideoFile);
  MemoProps.Clear;
  With MemoProps.Lines do
  begin
    add('Properties of');
    add(ExtractFilename(fVideoFile) + ':');
    add('Width: ' + IntToStr(p.Width));
    add('True Height: ' + IntToStr(p.TrueHeight));
    add('Frame rate: ' + IntToStr(p.FrameRate));
    add('No. video streams: ' + IntToStr(p.nrVideostreams));
    add('No. audio streams: ' + IntToStr(p.nrAudiostreams));
    add('Duration: ' + FloatToStrF(0.001 * p.Duration, ffFixed, 8, 2) + ' sec');
    add('Video codec: ' + avCodec_Find_Decoder(p.VideoCodec).name);
  end;

end;

procedure TForm1.ButtonInsertClipMainThreadClick(Sender: TObject);
var
  UpdateProc: TVideoProgressEvent;
  aTask: iTask;
begin
  Label12.Caption := 'Working';
  Label12.Repaint;
  ProgressBar2.Max := GetVideoTime(fVideoFile);
  ProgressBar2.Position := 0;
  TheProgressbar := ProgressBar2;
  if Sender = ButtonInsertClipMainThread then
  begin
    UpdateProc := UpdateVideo;
    InsertVideo(UpdateProc);
  end
  else
  begin
    UpdateProc := UpdateVideoThreaded;
    aTask := TTask.Create(
      procedure
      begin
        InsertVideo(UpdateProc);
      end);
    aTask.Start;
  end;
end;

procedure TForm1.Image1Click(Sender: TObject);
var
  aImage: TImage;
begin
  if not OPD.execute then
    exit;
  aImage := TImage(Sender);
  aImage.Picture.LoadFromFile(OPD.filename);
end;

procedure TForm1.InsertVideo(UpdateProc: TVideoProgressEvent);
var
  bm: TBitmap;
  w, h: integer;
  bme: TBitmapEncoder;
  t: int64;
  f: integer;
begin
  t := TimeGetTime;
  bm := TBitmap.Create;
  try
    h := VideoHeight;
    w := VideoWidth;
    // load the 1st frame from the video
    try
      GrabFrame(bm, fVideoFile, 1);
    except
      // We catch the exception, since GrabFrame does not
      // yet work reliably with foreign video content
      BlackBitmap(bm, w, h);
    end;

    bme := TBitmapEncoder.Create(OutputFile, w, h, FrameRate, EncodingQuality,
      EncoderId, vsBiCubic, VideoBackGround);
    try
      bme.OnProgress := UpdateProc;
      TextOnBitmap(bm, 'Intro Screen');
      bme.AddStillImage(bm, 5000);
      bme.AddVideo(fVideoFile);
      // bme.LastVideoFrameCount always contains the frame count of the last
      // added video.
      try
        GrabFrame(bm, fVideoFile, bme.LastVideoFrameCount - 2);
      except
        BlackBitmap(bm, w, h);
      end;
      TextOnBitmap(bm, 'The End');
      bme.AddStillImage(bm, 5000);
      f := bme.framecount;
      bme.CloseFile;
    finally
      bme.Free;
    end;
  finally
    bm.Free;
  end;
  t := TimeGetTime - t;
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      Label12.Caption := 'Writing speed: '#13 + FloatToStrF(1000 * f / t,
        ffFixed, 6, 2) + ' fps';
    end);
end;

procedure TForm1.CodecComboChange(Sender: TObject);
begin
  UpdateSizeinMB;
end;

procedure TForm1.UpdateCodecCombo;
var
  Index: integer;
begin
  ListSupportedCodecs(FormatCombo.Text, CodecCombo.Items, Index);
  if Index >= 0 then
    CodecCombo.ItemIndex := Index;
end;

procedure TForm1.UpdateSizeinMB;
begin
  Label9.Caption := FloatToStrF(VideoSizeinMB(30000, EncoderId, VideoWidth,
    VideoHeight, FrameRate, EncodingQuality), ffFixed, 6, 2) + ' MB';
end;

procedure TForm1.FormatComboChange(Sender: TObject);
begin
  UpdateCodecCombo;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  aImage: TImage;
  aColor: TColor;
begin
  EditOutput.Text := GetDesktopFolder + '\Example';
  ListSupportedFileFormats(FormatCombo.Items);
  FormatCombo.ItemIndex := 1;
  UpdateCodecCombo;
  if FileExists('GoodTestPicture.png') then
    ImageZoomPicture.Picture.LoadFromFile('GoodTestPicture.png');
  UpdateSizeinMB;
  Randomize;
  fSlideshowList := TList<TImage>.Create;
  for i := 1 to 7 do
  begin
    aImage := TImage(FindComponent('Image' + IntToStr(i)));
    aColor := RGB(random(255), random(255), random(255));
    aImage.Canvas.brush.color := aColor;
    aImage.Canvas.Fillrect(aImage.ClientRect);
    fSlideshowList.add(aImage);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fSlideshowList.Free;
end;

procedure TForm1.FrameSpinChange(Sender: TObject);
var
  bm: TBitmap;
begin
  if not FileExists(fVideoFile) then
    exit;
  bm := TBitmap.Create;
  try
    GrabFrame(bm, fVideoFile, FrameSpin.Value);
    ImageShowFrame.Picture.Bitmap := bm;
  finally
    bm.Free;
  end;
end;

function TForm1.GetEncoderId: TAVCodecID;
begin
  Result := TAVCodecID(CodecCombo.Items.Objects[CodecCombo.ItemIndex]);
end;

function TForm1.GetEncodingQuality: Byte;
begin
  Result := SpinEditQuality.Value;
end;

function TForm1.GetFrameRate: integer;
begin
  Result := StrToInt(RateCombo.Text);
end;

function TForm1.GetOutputFile: string;
begin
  Result := EditOutput.Text + FormatCombo.Text;
end;

function TForm1.GetVideoBackGround: Byte;
begin
  Result := BgColors[BgCombo.ItemIndex];
end;

function TForm1.GetVideoHeight: integer;
begin
  Result := StrToInt(HC.Text);
end;

function TForm1.GetVideoWidth: integer;
begin
  Result := round(VideoHeight * Aspects[RadioGroupAspect.ItemIndex]);
  if odd(Result) then
    dec(Result);
end;

procedure TForm1.HCChange(Sender: TObject);
begin
  UpdateSizeinMB;
end;

procedure TForm1.RateComboChange(Sender: TObject);
begin
  UpdateSizeinMB;
end;

procedure TForm1.SpinEditQualityChange(Sender: TObject);
begin
  UpdateSizeinMB;
end;

procedure TForm1.UpdateVideo(Videotime: int64);
begin
  TheProgressbar.Position := Videotime;
  TheProgressbar.Update;
end;

procedure TForm1.UpdateVideoThreaded(Videotime: int64);
begin
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      TheProgressbar.Position := Videotime;
      TheProgressbar.Update;
    end);
end;

{ TRawSetup }

constructor TRawSetup.Create(CodecID: TAVCodecID);
begin
  inherited;
  fPreferredOutputPixelFormat := AV_PIX_FMT_YUV420P;
end;

function TRawSetup.QualityToBitrate(Quality: Byte;
Width, Height, Rate: integer): int64;
begin
  Result := 0;
end;

initialization

ReportMemoryLeaksOnShutDown := true;

{$IFDEF UseBass}
{$UNDEF UseBass}
{$ENDIF}

end.
