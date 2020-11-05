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
  Androidapi.Jni.GraphicsContentViewText,
  Androidapi.Jni.Net,
  FMX.Platform.Android,
  Androidapi.JNIBridge,
  Androidapi.Jni.JavaTypes,
{$ENDIF}
  FMX.SpinBox, FMX.TabControl, FMX.MediaLibrary, FMX.Platform,
  System.Messaging, FMX.ScrollBox, FMX.Memo;

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
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormatComboChange(Sender: TObject);
    procedure ButtonLoadVideoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinBoxFrameNoChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
{$IFDEF ANDROID}
    FPermissionWriteExtStorage: string;
    FMessageSubscriptionID: integer;
{$ENDIF}
    MovieProc: TMovieProc;
    fVideoFilename: string;
    procedure UpdateCodecCombo;
{$IFDEF ANDROID}
    /// <summary>
    /// This event will be called when the user answers the request to grant
    /// the necessary write permission to the external storage
    /// </summary>
    procedure LocationPermissionRequestResult(Sender: TObject;
      const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>);
    /// <summary>
    /// This event will be called when the user has picked a video object
    /// </summary>
    procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
    /// <summary>
    /// This processes the picked video object and returns a valid file name, or fails, if
    /// the user did not pick the right viewer (documents folder)
    /// </summary>
    function HandleIntentAction(const Data: JIntent): Boolean;
{$ENDIF}
    procedure BrowseForVideoFile;
    procedure ProcessVideoFile;
    procedure GrabAFrame;
    /// Example how to safely(?) use the canvas of a TBitmap from threads
    procedure MakeTextBitmap(const bm: TBitmap; const text: String;
      cb, ct: Cardinal);
    procedure WriteTextOnBitmap(const bm: TBitmap; const text: String;
      ct: Cardinal);
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

uses
  UFormatsM, UBitmaps2VideoM, FFMPEG,
  math, IOUtils, System.Threading, FMX.DialogService,
  UToolsM, System.UIConsts;

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
  MovieProc(TPath.Combine(TPath.GetSharedDownloadsPath,
    Edit1.text + FormatCombo.Items[FormatCombo.ItemIndex]));
{$ELSE}
  MovieProc(TPath.Combine(TPath.GetMoviesPath, Edit1.text + FormatCombo.Items
    [FormatCombo.ItemIndex]));
{$ENDIF}
end;

procedure TForm1.MakeTextBitmap(const bm: TBitmap; const text: String;
  cb, ct: Cardinal);
var
  am: TBitmap;
begin
  //Run this in the main thread
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      //Use the canvas of a temporary bitmap
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
        //Copy the pixels to the result-bitmap
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
  TThread.Synchronize(TThread.Current,
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
          TTextAlign.Center, TTextAlign.Trailing);
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
  bme: TBitmapEncoderM;
  NewW, NewH: integer;
  OutputFile: string;
  CodecID: TAVCodecID;
  FrameRate: integer;
  aTask: TThread;
  bm: TBitmap;
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
    CodecID := TAVCodecID(TCodecIdWrapper(CodecCombo.Items.Objects
      [CodecCombo.ItemIndex]).CodecID);
  FrameRate := StrToInt(RateCombo.Items.Strings[RateCombo.ItemIndex]);
{$IFDEF ANDROID}
  OutputFile := TPath.Combine(TPath.GetSharedDownloadsPath,
    Edit1.text + FormatCombo.Items[FormatCombo.ItemIndex]);
{$ELSE}
  OutputFile := TPath.Combine(TPath.GetMoviesPath,
    Edit1.text + FormatCombo.Items[FormatCombo.ItemIndex]);
{$ENDIF}
  aTask := TThread.CreateAnonymousThread(
    procedure
    begin
      bme := TBitmapEncoderM.Create(OutputFile, NewW, NewH, FrameRate,
        trunc(SpinBox1.Value), CodecID, vsBicubic, BgColors[BgCombo.ItemIndex]);
      bme.OnProgress := ShowProgress;
      try
        bm := TBitmap.Create;
        try
          GrabFrame(bm, fVideoFilename, 1);
          WriteTextOnBitmap(bm, 'THE START'#13#13' ', claDarkRed);
          bme.AddStillImage(bm, 3000);
        finally
          bm.Free;
        end;
        bme.AddVideo(fVideoFilename);
        bm := TBitmap.Create;
        try
          bm.SetSize(NewW, NewH);
          MakeTextBitmap(bm, 'THE END', claBlack, claYellow);
          bme.AddStillImage(bm, 3000);
        finally
          bm.Free;
        end;
        bme.CloseFile;
      finally
        bme.Free;
      end;

    end);
  aTask.Start;
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
{$IFDEF ANDROID}
var
  Intent: JIntent;
  fName: string;
{$ENDIF}
begin
{$IFDEF ANDROID}
  // fMessageSubscriptionID: integer is a field of TForm1
  FMessageSubscriptionID := TMessageManager.DefaultManager.SubscribeToMessage
    (TMessageResultNotification, HandleActivityMessage);
  fName := TPath.GetSharedDocumentsPath;
  Intent := TJIntent.Create;
  Intent.setType(StringToJString('video/*'));
  Intent.setAction(TJIntent.JavaClass.ACTION_GET_CONTENT);
  MainActivity.startActivityForResult(Intent, 0);
{$ELSE}
  if OD.Execute then
  begin
    fVideoFilename := OD.filename;
    Edit2.text := fVideoFilename;
    ProcessVideoFile;
  end;
{$ENDIF}
end;

{$IFDEF ANDROID}

// when message is received
procedure TForm1.HandleActivityMessage(const Sender: TObject;
const M: TMessage);
begin
  if M is TMessageResultNotification then
    if HandleIntentAction(TMessageReceivedNotification(M).Value) then
    begin
      // A valid filename has been returned
      ProcessVideoFile;
    end;
  Edit2.text := fVideoFilename;
  if fVideoFilename = '' then
    TDialogService.ShowMessage
      ('Pick the file by opening the Documents folder. Don''t use any of the other apps.');
end;
{$ENDIF}
{$IFDEF ANDROID}

// This retrieves the file name the user picked, but reliably only if done per
// opening MyDocuments
function TForm1.HandleIntentAction(const Data: JIntent): Boolean;
var
  P: TJavaObjectArray<Jstring>;
  // in case you want only specific fields... not used here. I have passed nil to get all the columns.
  C: JCursor;
  I: integer;
begin
  P := nil;
  // The following makes no difference
  // P:=TJavaObjectArray<Jstring>.create(1);
  // P.Items[0]:=StringToJstring('_data');

  // this is supposed to give the information back to C ( : JCursor)
  C := MainActivity.getContentResolver.query(Data.getData, nil,
  // when projection is nil... it returns all columns. Ideally, you should only ask for the columns you need
  StringToJString(''),
  // java accepts nil... but you cannot give nil here, you need to give an empty JString
  nil, StringToJString(''));
  // java accepts nil... but you cannot give nil here, you need to give an empty JString

  C.moveToFirst;
  Result := False;
  for I := 0 to C.getColumnCount - 1 do
  begin
    if JStringToString(C.getColumnName(I)) = '_data' then
    // '_data' column contains the path... you can use this to create the filestream to upload or do whatever....
    begin
      // fVideoFilename is a field of the form
      fVideoFilename := JStringToString(C.getString(I));
      Result := true;
      Break;
    end;
  end;
  if not Result then
    fVideoFilename := '';
  // P.Free;
end;
{$ENDIF}

procedure TForm1.FormatComboChange(Sender: TObject);
begin
  UpdateCodecCombo;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateCodecCombo;
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
  CodecID: TAVCodecID;
  FrameRate: integer;
  asp: double;
  aTask: iTask;
begin
  TheProgressbar := ProgressBar1;
  aTask := TTask.Create(
    procedure
    begin
      Height := MovieHeights[HeightCombo.ItemIndex];
      asp := AspectRatios[AspectCombo.ItemIndex];
      Width := Round(asp * Height);
      CodecID := AV_CODEC_ID_NONE;
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
  CodecID: TAVCodecID;
  FrameRate: integer;
  aTask: iTask;
  bm: TBitmap;
  I: integer;
  asp: double;
begin
  TheProgressbar := ProgressBar1;
  aTask := TTask.Create(
    procedure
    begin
      bm := TBitmap.Create;
      try
        Height := MovieHeights[HeightCombo.ItemIndex];
        asp := AspectRatios[AspectCombo.ItemIndex];
        Width := Round(Height * asp);
        if odd(Width) then
          dec(Width);
        CodecID := AV_CODEC_ID_NONE;
        if CodecCombo.ItemIndex >= 0 then
          CodecID := TAVCodecID(TCodecIdWrapper(CodecCombo.Items.Objects
            [CodecCombo.ItemIndex]).CodecID);
        FrameRate := StrToInt(RateCombo.Items.Strings[RateCombo.ItemIndex]);
        bme := TBitmapEncoderM.Create(filename, Width, Height, FrameRate,
          trunc(SpinBox1.Value), CodecID, vsBicubic, BgColors[BgCombo.ItemIndex]
          );
        try
          bme.OnProgress := ShowProgress;
          // 18 sec of movie
          I := 1;
          while I < 4 do
          begin
            bm.Assign(SlideshowPic1.Bitmap);
            bme.AddStillImage(bm, 3000);
            bm.Assign(SlideshowPic2.Bitmap);
            bme.AddStillImage(bm, 3000);
            inc(I);
          end;
          // Hold end frame for 2 seconds
          bme.Freeze(2000);
          bme.CloseFile;
        finally
          bme.Free;
        end;
      finally
        bm.Free;
      end;
    end);
  aTask.Start;
end;

procedure TForm1.ShowProgress(Videotime: int64);
begin
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      TheProgressbar.Value := 0.001 * Videotime;
    end);
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
