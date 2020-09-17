unit UDemoBitmaps2Video;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UBitmaps2Video, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, Vcl.ExtCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  Vcl.Imaging.pngImage,
  Vcl.ExtDlgs, UFormats, FFMPeg;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button2: TButton;
    Label1: TLabel;
    SD: TSaveDialog;
    SpinEdit1: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    HC: TComboBox;
    OPD: TOpenPictureDialog;
    OD: TOpenDialog;
    Label6: TLabel;
    CodecCombo: TComboBox;
    Label7: TLabel;
    RateCombo: TComboBox;
    Label8: TLabel;
    FormatCombo: TComboBox;
    Button5: TButton;
    OVD: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Image1: TImage;
    Button3: TButton;
    rgrpZoom: TRadioGroup;
    Button1: TButton;
    Label4: TLabel;
    ProgressBar1: TProgressBar;
    Label9: TLabel;
    Button7: TButton;
    Button6: TButton;
    Button4: TButton;
    Label5: TLabel;
    Label10: TLabel;
    RadioGroup1: TRadioGroup;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Memo1: TMemo;
    Label15: TLabel;
    Label16: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormatComboChange(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure HCChange(Sender: TObject);
    procedure RateComboChange(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure CodecComboChange(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    procedure UpdateCodecCombo;
    procedure UpdateSizeinMB;
    { Private declarations }
  public
    { Public declarations }
    VideoAspect: double;
    procedure UpdateVideo(Videotime: int64);
  end;

  // Example for a new TCodecSetupClass
  TRawSetup = class(TBaseCodecSetup)
  public
    Constructor Create(CodecID: TAVCodecID); override;
    // for raw video the bitrate is automatic, so we return 0
    function QualityToBitrate(Quality: byte; Width, Height, Rate: integer)
      : int64; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses mmSystem, System.Types, math, UTools, Winapi.ShlObj, Winapi.ActiveX;

const
  Aspects: array [0 .. 2] of double = (16 / 9, 4 / 3, 3 / 2);

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

procedure TForm1.Button1Click(Sender: TObject);
var
  bm: TBitmap;
  bme: TBitmapEncoder;
  h, w, bw, bh: integer;
  t: int64;
  fps: double;
  ZoomTarget, ZoomSource, ZoomTarget2: TRectF;
  ZoomOption: TZoomOption;

begin
  if (not assigned(Image1.Picture.Graphic)) then
  begin
    Vcl.Dialogs.ShowMessage('Load a picture first');
    exit;
  end;
  VideoAspect := Aspects[RadioGroup1.ItemIndex];
  t := TimeGetTime;
  bm := TBitmap.Create;
  try
    bm.Assign(Image1.Picture.Graphic);
    bm.PixelFormat := pf32bit;

    bw := bm.Width;
    bh := bm.Height;
    h := StrToInt(HC.Text);
    w := round(h * VideoAspect);
    if odd(w) then
      w := w - 1;
    ZoomSource := RectF(0, 0, bw, bh);
    ZoomTarget2 := ScaleRect(ZoomSource, 0.5);
    OffsetRect(ZoomTarget2, random(bw div 2), random(bh div 2));
    ZoomTarget := ScaleRect(ZoomSource, 0.3);
    CenterRect(ZoomTarget, ZoomSource);
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

    bme := TBitmapEncoder.Create(Edit1.Text + FormatCombo.Text, w, h,
      StrToInt(RateCombo.Text), SpinEdit1.Value,
      TAVCodecID(CodecCombo.Items.Objects[CodecCombo.ItemIndex]), vsBiCubic);
    // vsBiCubic: if it needs to scale it scales nicely

    try
      bme.OnProgress := UpdateVideo;
      ProgressBar1.Position := 0;
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

procedure TForm1.Button2Click(Sender: TObject);
begin
  if SD.Execute then
  begin
    Edit1.Text := Copy(SD.FileName, 1, length(SD.FileName) -
      length(ExtractFileExt(SD.FileName)));
    FormatCombo.ItemIndex := FormatCombo.Items.IndexOf
      (ExtractFileExt(SD.FileName));
    UpdateCodecCombo;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if OPD.Execute then
    Image1.Picture.LoadFromFile(OPD.FileName);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Tempfile, Videofile, Audiofile: string;
begin
  if not OD.Execute then
    exit;
  Try
    Label5.Caption := 'Writing audio';
    Audiofile := OD.FileName;
    Videofile := Edit1.Text + FormatCombo.Text;
    if not FileExists(Videofile) then
    begin
      ShowMessage('Make a Movie first');
      exit;
    end;
    Tempfile := GetTempFolder + '\_VideoTemp' + ExtractFileExt(Videofile);
    CopyFile(PWideChar(Videofile), PWideChar(Tempfile), false);
    MuxStreams2(Tempfile, Audiofile, Videofile);
    DeleteFile(Tempfile);
    Label5.Caption := 'Audio track now contains ' + ExtractFilename(Audiofile);
  except
    ShowMessage('Audio format not supported by container format ' +
      ExtractFileExt(Videofile));
    // Restore orignal video
    CopyFile(PWideChar(Tempfile), PWideChar(Videofile), false);
  End;
end;

procedure TForm1.Button5Click(Sender: TObject);
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
  bm.Canvas.Font.Color := clWhite;
  bm.Canvas.Brush.style := bsClear;
  bm.Canvas.Font.Size := 32;
  r := Rect(0, 0, bm.Width, bm.Height);
  DrawText(bm.Canvas.Handle, PChar(_text), length(_text), r,
    dt_Center or dt_CalcRect);
  r1 := TRectF(r);
  CenterRect(r1, RectF(0, 0, bm.Width, bm.Height));
  r := r1.round;
  DrawText(bm.Canvas.Handle, PChar(_text), length(_text), r, dt_Center);
end;

procedure BlackBitmap(const bm: TBitmap; w, h: integer);
begin
  bm.PixelFormat := pf32bit;
  bm.SetSize(w, h);
  BitBlt(bm.Canvas.Handle, 0, 0, w, h, 0, 0, 0, BLACKNESS);
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  bme: TBitmapEncoder;
  bm: TBitmap;

begin
  if not OVD.Execute then
    exit;
  Label15.Caption := 'Working';
  Label15.Repaint;
  bme := TBitmapEncoder.CreateFromVideo(OVD.FileName, Edit1.Text, vsBiCubic);
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

procedure TForm1.Button7Click(Sender: TObject);
var
  bm: TBitmap;
  bme: TBitmapEncoder;
  w, h: integer;
begin
  if not OVD.Execute then
    exit;
  VideoAspect := Aspects[RadioGroup1.ItemIndex];
  bm := TBitmap.Create;
  try
    h := StrToInt(HC.Text);
    w := round(VideoAspect * h);
    // video sizes must be even
    if odd(w) then
      dec(w);
    // load the 1st frame from the video
    try
      GrabFrame(bm, OVD.FileName, 1);
    except
      // We catch the exception, since GrabFrame does not
      // yet work reliably with foreign video content
      BlackBitmap(bm, w, h);
    end;
    Label12.Caption := 'Working';
    Label12.Repaint;
    bme := TBitmapEncoder.Create(Edit1.Text + FormatCombo.Text, w, h,
      StrToInt(RateCombo.Text), SpinEdit1.Value,
      TAVCodecID(CodecCombo.Items.Objects[CodecCombo.ItemIndex]), vsBiCubic);
    try
      TextOnBitmap(bm, 'Intro Screen');
      bme.AddStillImage(bm, 5000);
      bme.AddVideo(OVD.FileName);
      // bme.LastVideoFrameCount always contains the frame count of the last
      // added video.
      try
        GrabFrame(bm, OVD.FileName, bme.LastVideoFrameCount - 2);
      except
        BlackBitmap(bm, w, h);
      end;
      TextOnBitmap(bm, 'The End');
      bme.AddStillImage(bm, 5000);
      bme.CloseFile;
    finally
      bme.Free;
    end;
  finally
    bm.Free;
  end;
  Label12.Caption := 'Done';
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
var
  VideoWidth: integer;
begin
  if (Image1.Picture = nil) or (Image1.Picture.Height = 0) then
    exit;
  VideoWidth := round(StrToInt(HC.Text) * Image1.Picture.Width /
    Image1.Picture.Height);
  Label9.Caption := FloatToStrF(VideoSizeinMB(30000,
    TAVCodecID(CodecCombo.Items.Objects[CodecCombo.ItemIndex]), VideoWidth,
    StrToInt(HC.Text), StrToInt(RateCombo.Text), SpinEdit1.Value), ffFixed, 6,
    2) + ' MB';
end;

procedure TForm1.FormatComboChange(Sender: TObject);
begin
  UpdateCodecCombo;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := GetDesktopFolder + '\Example';
  ListSupportedFileFormats(FormatCombo.Items);
  FormatCombo.ItemIndex := 0;
  UpdateCodecCombo;
  if FileExists('GoodTestPicture.png') then
    Image1.Picture.LoadFromFile('GoodTestPicture.png');
  UpdateSizeinMB;
  VideoAspect := 16 / 9;
  Randomize;
end;

procedure TForm1.HCChange(Sender: TObject);
begin
  UpdateSizeinMB;
end;

procedure TForm1.RateComboChange(Sender: TObject);
begin
  UpdateSizeinMB;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  UpdateSizeinMB;
end;

procedure TForm1.UpdateVideo(Videotime: int64);
begin
  ProgressBar1.Position := Videotime;
end;

{ TRawSetup }

constructor TRawSetup.Create(CodecID: TAVCodecID);
begin
  inherited;
  fPreferredOutputPixelFormat := AV_PIX_FMT_YUV420P;
end;

function TRawSetup.QualityToBitrate(Quality: byte;
  Width, Height, Rate: integer): int64;
begin
  Result := 0;
end;

initialization

ReportMemoryLeaksOnShutDown := true;

end.
