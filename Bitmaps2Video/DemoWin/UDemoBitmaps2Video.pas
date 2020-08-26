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
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    SD: TSaveDialog;
    SpinEdit1: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    HC: TComboBox;
    Label4: TLabel;
    rgrpZoom: TRadioGroup;
    ProgressBar1: TProgressBar;
    Button3: TButton;
    OPD: TOpenPictureDialog;
    Button4: TButton;
    OD: TOpenDialog;
    Label5: TLabel;
    Label6: TLabel;
    CodecCombo: TComboBox;
    Label7: TLabel;
    RateCombo: TComboBox;
    Label8: TLabel;
    FormatCombo: TComboBox;
    Button5: TButton;
    Label9: TLabel;
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
  private
    procedure UpdateCodecCombo;
    procedure UpdateSizeinMB;
    { Private declarations }
  public
    { Public declarations }
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
  asp: double;
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
  t := TimeGetTime;
  bm := TBitmap.Create;
  try
    bm.Assign(Image1.Picture.Graphic);
    bm.PixelFormat := pf32bit;

    bw := bm.Width;
    bh := bm.Height;
    asp := bw / bh;
    h := StrToInt(HC.Text);
    w := round(h * asp);
    if odd(w) then
      w := w - 1;
    ZoomSource := RectF(0, 0, bw, bh);
    ZoomTarget2 := ScaleRect(ZoomSource, 0.5);
    OffsetRect(ZoomTarget2, random(w div 2), random(h div 2));
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

      // 30 seconds of movie
      bme.AddStillImage(bm, 2000);

      bme.ZoomPan(bm, ZoomSource, ZoomTarget, 9000, ZoomOption, zeFastSlow);
      bme.Freeze(1000);
      bme.ZoomPan(bm, ZoomTarget, ZoomTarget2, 8000, ZoomOption, zeSlowSlow);
      bme.Freeze(1000);
      bme.ZoomPan(bm, ZoomTarget2, ZoomSource, 7000, ZoomOption, zeSlowFast);
      bme.Freeze(2000);

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
  if (Image1.Picture = nil) or (Image1.Picture.Height=0) then
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
