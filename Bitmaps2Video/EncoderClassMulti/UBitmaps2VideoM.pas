{*****************************************************************************
  This file is licensed to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License. A copy of this licence is found in the root directory of
  this project in the file LICENCE.txt or alternatively at
    http://www.apache.org/licenses/LICENSE-2.0
  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
  Contributors:
  Renate Schaaf (original code)
  Markus Humm (made it compile and run under Android)
*****************************************************************************}
unit UBitmaps2VideoM;

interface

uses
  System.SysUtils,
  FFMPEG,
  FMX.Graphics,
  System.types,
  UFormatsM;

type
  /// <summary> Libav-Scaling used if bitmapsize<>videosize </summary>
  TVideoScaling = (vsFastBilinear, vsBilinear, vsBiCubic, vsLanczos);

  /// <summary> Zoom algorithms, the higher the better and slower
  /// zoAAx2: Antialising factor 2
  /// zoAAx4: Antialising factor 4
  /// zoAAx6: Antialising factor 6
  /// zoResample: "Bicubic" resampling, almost perfect but slow.</summary>
  TZoomOption = (zoAAx2, zoAAx4, zoAAx6, zoResample);

const
  ScaleFunction: array [TVideoScaling] of integer = (SWS_FAST_BILINEAR,
    SWS_BILINEAR, SWS_BICUBIC, SWS_LANCZOS);

  FilterName: array [TVideoScaling] of string = ('Fast Bilinear', 'Bilinear',
    'Bicubic', 'Lanczos');

type

  TVideoProgressEvent = procedure(Videotime: int64) of object;

  //Compatible with windows TRGBTriple
  TBGR=record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
  end;
  PBGR=^TBGR;

  TBitmapEncoderM = class
  private
    fWidth, fHeight, fRate: integer;
    fQuality: byte;
    fFilename: string;
    fFrameCount: integer;
    fVideoScaling: TVideoScaling;
    fCodecId: TAVCodecId;
    fInputVideoTimebase: TAVRational;
    fmt: PAVOutputFormat;
    oc: PAVFormatContext;
    stream: PAVStream;
    codec: PAVCodec;
    c: PAVCodecContext;
    yuvpic: PAVFrame;
    pkt: PAVPacket;
    CodecSetup: TBaseCodecSetup;
    fOnProgress: TVideoProgressEvent;
    function encode(frame: PAVFrame; FromVideo: boolean): boolean;
    procedure BitmapToFrame(const bm: TBitmap);//does nothing right now use BitmapToFrameSlow
    procedure BitmapToFrameSlow(const bm: TBitmap);
    procedure ColorToFrame(Color: TBGR);
  public
    /// <param name="filename"> Output filename. Extension picks the container format (.mp4 .avi .mkv ..)
    /// </param>
    /// <param name="Width"> Video size in pixels (multiples of 2)</param>
    /// <param name="Height"> Video size in pixels (multiples of 2)</param>
    /// <param name="FrameRate"> Frames per second (can be slightly off in the resulting file)</param>
    /// <param name="Quality"> Encoding quality, scale from 0 to 100. 0 gives marginally acceptable quality </param>
    /// <param name="CodecId"> (TAVCodecID see UFormats.pas) Identifier of the codec to use. AV_CODEC_ID_NONE picks the default codec for the file extension. </param>
    /// <param name="VideoScaling">Resample algo used if video size differs from bitmap size </param>
    constructor Create(const filename: string; Width, Height: integer;
      FrameRate: integer; Quality: byte; CodecId: TAVCodecId = AV_CODEC_ID_NONE;
      VideoScaling: TVideoScaling = vsFastBilinear);

    /// <summary> Turn a Bitmap into a movie frame </summary>
    /// <param name="bm"> Bitmap(TBitmap) to be fed to the video. Should have R G and B channels </param>
    procedure AddFrame(const bm: TBitmap);

    ///<summary>Add a uniformly colored Frame to the Video</summary>
    procedure AddColorFrame(Color: TBGR);

    /// <summary> Hold the last frame </summary>
    /// <param name="EffectTime"> Displaytime(integer) in ms </param>
    procedure Freeze(EffectTime: integer);

    /// <summary> Add a picture which is displayed for a certain time </summary>
    /// <param name="bm"> Bitmap(TBitmap) of the picture to be displayed </param>
    /// <param name="ShowTime"> Time(integer) in ms for the display </param>
    procedure AddStillImage(const bm: TBitmap; ShowTime: integer);


    /// <summary> Close the file and make the output file usable. </summary>
    function CloseFile: boolean;


    destructor Destroy; override;

    /// <summary> how many frames have been added to the movie so far </summary>
    property FrameCount: integer read fFrameCount;

    /// <summary> Event which fires every second of video time while writing. Use it to update a progressbar etc.. </summary>
    property OnProgress: TVideoProgressEvent read fOnProgress write fOnProgress;
    // Application.Processmessages can be called safely in this event (I hope).

    
  end;


function VideoSizeInMB(Videotime: int64; CodecId: TAVCodecId;
  Width, Height, Rate: integer; Quality: byte): double;

implementation

uses System.UITypes;


{ TBitmapEncoder }

function VideoSizeInMB(Videotime: int64; CodecId: TAVCodecId;
  Width, Height, Rate: integer; Quality: byte): double;
var
  Setup: TBaseCodecSetup;
  TimeSec: integer;
begin
  TimeSec := Videotime div 1000;
  if CodecSetupClass(CodecId) = nil then
  begin
    Raise Exception.Create('Codec-Id is not supported');
    exit;
  end;
  Setup := CodecSetupClass(CodecId).Create(CodecId);
  try
    result := 0.001 * 1 / 8 * 1 / 1024 * Setup.QualityToBitrate(Quality, Width,
      Height, Rate) * TimeSec;
  finally
    Setup.Free;
  end;
end;

function TBitmapEncoderM.encode(frame: PAVFrame; FromVideo: boolean): boolean;
var
  ret: integer;
begin
  inc(fFrameCount);
  if fFrameCount mod fRate = 0 then // update once per second
    if assigned(fOnProgress) then
      fOnProgress(fFrameCount * 1000 div fRate);
  av_init_packet(pkt);
  pkt.data := nil;
  pkt.size := 0;
  if frame<>nil then
  begin
  if FromVideo then
  begin
    frame.pts :=fFrameCount;
  end
  else
    frame.pts := fFrameCount;
  end;
  ret := avcodec_send_frame(c, frame);
  while ret >= 0 do
  begin
    ret := avcodec_receive_packet(c, pkt);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
    begin
      result := true;
      exit;
    end
    else if ret < 0 then
    begin
      result := False;
      exit;
    end;
    if FromVideo then
      av_packet_rescale_ts(pkt, fInputVideoTimebase, c.time_base);
      av_packet_rescale_ts(pkt, c.time_base, stream.time_base);
    // We set the packet PTS and DTS taking in the account our FPS (second argument)
    // and the time base that our selected format uses (third argument).
    pkt.stream_index := stream.index;
    ret := av_interleaved_write_frame(oc, pkt);
    // Write the encoded frame to the video file.
    // Can fail without causing harm
    av_packet_unref(pkt);
  end;
  result := true;
end;

procedure TBitmapEncoderM.Freeze(EffectTime: integer);
var
  frametime: double;
  time: double;
begin
  frametime := 1000 / fRate;
  time := 0;
  while time < EffectTime do
  begin
    encode(yuvpic, False);
    time := time + frametime;
  end;
end;


 //needs to be implemented with fast access to the pixels of bm
 //research into layout in memory needed
procedure TBitmapEncoderM.BitmapToFrame(const bm: TBitmap);
{
  var rgbpic: PAVFrame;
    convertCtx: PSwsContext;
    x, y: integer;
    row: PByte;
    w, h, bps: integer;
    px, py: PByte;
    jump: integer;
    ret: integer;
    BitData    : TBitmapData;
      AC         : TAlphaColor;
}
begin
    {
    if not (bm.Map(TMapAccess.Read, BitData)) then
      begin
        Raise Exception.Create('Bitmap is not Readable');
        exit;
      end;
      w := bm.Width;
      h := bm.Height;
      // Set up conversion to YUV
      convertCtx := sws_getContext(w, h, AV_PIX_FMT_BGR24, fWidth, fHeight,
        CodecSetup.OutputPixelFormat, ScaleFunction[fVideoScaling], nil, nil, nil);
      // Video ignores the alpha-channel. Size will be scaled if necessary,
      // proportionality might not be preserved
      assert(convertCtx <> nil);

      // Allocate storage for the rgb-frame
      rgbpic := av_frame_alloc();
      assert(rgbpic <> nil);
      try
        rgbpic.Format := Ord(AV_PIX_FMT_BGR24);
        rgbpic.Width := w;
        rgbpic.Height := h;
        av_frame_get_buffer(rgbpic, 0);

        // Store the bm in the frame
        ret := av_frame_make_writable(rgbpic);
        assert(ret >= 0);

        row := bm.ScanLine[0];
        bps := ((w * 32 + 31) and not 31) div 8;
        py := @PByte(rgbpic.data[0])[0];
        // it's faster with pointers instead of array
        jump := rgbpic.linesize[0];
        for y := 0 to h - 1 do
        begin
          ps := PPixel32(row);
          px := py;
          for x := 0 to w - 1 do
          begin
            PPixel24(px)^ := PPixel24(ps)^;
            // works with BGR24 format
            inc(px, 3);
            inc(ps);
          end;
          dec(row, bps);
          inc(py, jump);
        end;

        // Convert the rgb-frame to yuv-frame
        ret := av_frame_make_writable(yuvpic);
        assert(ret >= 0);
        ret := sws_scale(convertCtx, @rgbpic.data, @rgbpic.linesize, 0, h,
          @yuvpic.data, @yuvpic.linesize);
        assert(ret >= 0);
      finally
        sws_freeContext(convertCtx);
        av_frame_free(@rgbpic);
        // needs to be freed and recreated for each frame
        // since the bm's could have different dimensions
      end;
  }
end;

procedure TBitmapEncoderM.BitmapToFrameSlow(const bm: TBitmap);
var
  rgbpic: PAVFrame;
  convertCtx: PSwsContext;
  x, y: integer;
  w, h: integer;
  px, py: PByte;
  jump: integer;
  ret: integer;
  BitData    : TBitmapData;
    AC         : TAlphaColor;
begin
  if not (bm.Map(TMapAccess.Read, BitData)) then
  begin
    Raise Exception.Create('Bitmap is not Readable');
    exit;
  end;
  w := bm.Width;
  h := bm.Height;
  // Set up conversion to YUV
  convertCtx := sws_getContext(w, h, AV_PIX_FMT_BGR24, fWidth, fHeight,
    CodecSetup.OutputPixelFormat, ScaleFunction[fVideoScaling], nil, nil, nil);
  // Video ignores the alpha-channel. Size will be scaled if necessary,
  // proportionality might not be preserved
  assert(convertCtx <> nil);

  // Allocate storage for the rgb-frame
  rgbpic := av_frame_alloc();
  assert(rgbpic <> nil);
  try
    rgbpic.Format := Ord(AV_PIX_FMT_BGR24);
    rgbpic.Width := w;
    rgbpic.Height := h;
    av_frame_get_buffer(rgbpic, 0);

    // Store the bm in the frame
    ret := av_frame_make_writable(rgbpic);
    assert(ret >= 0);

    py := @PByte(rgbpic.data[0])[0];
    jump := rgbpic.linesize[0];
    for y := 0 to h - 1 do
    begin
      px := py;
      for x := 0 to w - 1 do
      begin
        AC:=BitData.GetPixel(x,y);//this is probably very slow
        px^:=TAlphaColorRec(AC).B;
        inc(px);
        px^:=TAlphaColorRec(AC).G;
        inc(px);
        px^:=TAlphaColorRec(AC).R;
        // we have BGR24 format
        inc(px);
      end;
      inc(py, jump);
    end;

    // Convert the rgb-frame to yuv-frame
    ret := av_frame_make_writable(yuvpic);
    assert(ret >= 0);
    ret := sws_scale(convertCtx, @rgbpic.data, @rgbpic.linesize, 0, h,
      @yuvpic.data, @yuvpic.linesize);
    assert(ret >= 0);
  finally
    sws_freeContext(convertCtx);
    av_frame_free(@rgbpic);
    bm.Unmap(BitData);

  end;
end;



procedure TBitmapEncoderM.AddColorFrame(Color: TBGR);
begin
  ColorToFrame(Color);
  Encode(yuvpic, False);
end;

procedure TBitmapEncoderM.AddFrame(const bm: TBitmap);
begin
  // Store the Bitmap in the yuv-frame
  BitmapToFrameSlow(bm);

  // Encode the yuv-frame

  encode(yuvpic, False);

end;


procedure TBitmapEncoderM.AddStillImage(const bm: TBitmap; ShowTime: integer);

begin
  BitmapToFrameSlow(bm);

  // Encode the yuv-frame repeatedly
  Freeze(ShowTime);
end;


function TBitmapEncoderM.CloseFile: boolean;
begin
  // flush the encoder
  assert(encode(nil, False));

  av_write_trailer(oc); // Writing the end of the file.
  if ((fmt.flags and AVFMT_NOFILE) = 0) then
    avio_closep(@oc.pb); // Closing the file.
  result := (avcodec_close(stream.codec) >= 0);
end;

procedure TBitmapEncoderM.ColorToFrame(Color: TBGR);
var
  rgbpic: PAVFrame;
  convertCtx: PSwsContext;
  x, y: integer;

  px, py: PByte;
  jump: integer;
  ret: integer;
begin
  // Set up conversion to YUV
  convertCtx := sws_getContext(fWidth, fHeight, AV_PIX_FMT_BGR24, fWidth, fHeight,
    CodecSetup.OutputPixelFormat, ScaleFunction[fVideoScaling], nil, nil, nil);
  // Video ignores the alpha-channel. Size will be scaled if necessary,
  // proportionality might not be preserved
  assert(convertCtx <> nil);

  // Allocate storage for the rgb-frame
  rgbpic := av_frame_alloc();
  assert(rgbpic <> nil);
  try
    rgbpic.Format := Ord(AV_PIX_FMT_BGR24);
    rgbpic.Width := fWidth;
    rgbpic.Height := fHeight;
    av_frame_get_buffer(rgbpic, 0);

    // Store the color in the frame
    ret := av_frame_make_writable(rgbpic);
    assert(ret >= 0);

    py := @PByte(rgbpic.data[0])[0];
    jump := rgbpic.linesize[0];
    for y := 0 to fHeight - 1 do
    begin
      px := py;
      for x := 0 to fWidth - 1 do
      begin
        PBGR(px)^:=Color;
        inc(px,3);
      end;
      inc(py, jump);
    end;

    // Convert the rgb-frame to yuv-frame
    ret := av_frame_make_writable(yuvpic);
    assert(ret >= 0);
    ret := sws_scale(convertCtx, @rgbpic.data, @rgbpic.linesize, 0, fHeight,
      @yuvpic.data, @yuvpic.linesize);
    assert(ret >= 0);
  finally
    sws_freeContext(convertCtx);
    av_frame_free(@rgbpic);
    
  end;
end;

constructor TBitmapEncoderM.Create(const filename: string;
  Width, Height, FrameRate: integer; Quality: byte;
  CodecId: TAVCodecId = AV_CODEC_ID_NONE;
  VideoScaling: TVideoScaling = vsFastBilinear);
var
  ret: integer;
  bytes: TArray<Byte>;
begin
  fFilename := filename;
  fWidth := Width;
  fHeight := Height;
  fRate := FrameRate;
  fQuality := Quality;
  fVideoScaling := VideoScaling;
  fFrameCount := 0;
  if CodecId = AV_CODEC_ID_NONE then
    fCodecId := PreferredCodec(ExtractFileExt(fFilename))
  else
    fCodecId := CodecId;

  CodecSetup := CodecSetupClass(fCodecId).Create(fCodecId);

  fmt := GetOutputFormat(ExtractFileExt(fFilename));
  assert(fmt <> nil, 'No matching format');
  bytes := TEncoding.UTF8.GetBytes(filename);
  setlength(bytes,length(bytes)+1);
  bytes[length(bytes)-1]:=Byte(#0);
  oc := nil;
  ret := avformat_alloc_output_context2(@oc, nil, nil,
    PAnsiChar(@bytes[0]));
  assert(ret >= 0, 'avformat_alloc.. error' + inttostr(ret));
  stream := avformat_new_stream(oc, nil);
  assert(stream <> nil, 'avformat_new_stream failed');
  codec := CodecSetup.codec;
  assert(codec <> nil, 'codec not found');

  c := avcodec_alloc_context3(codec);
  c.Width := fWidth;
  c.Height := fHeight;

  c.time_base.num := 1;
  c.time_base.den := fRate;
  c.FrameRate.num := fRate;
  c.FrameRate.den := 1;

  c.pix_fmt := CodecSetup.OutputPixelFormat;

  CodecSetup.CodecContextProps(c);
  c.bit_rate := CodecSetup.QualityToBitrate(fQuality, fWidth, fHeight, fRate);
  // c.bit_rate_tolerance := 0; Left at defaults. More research needed :)

  if (oc.oformat.flags and AVFMT_GLOBALHEADER) <> 0 then
    c.flags := c.flags or AV_CODEC_FLAG_GLOBAL_HEADER;

  ret := avcodec_open2(c, codec, @CodecSetup.OptionsDictionary);
  assert(ret >= 0);
  stream.time_base := c.time_base;
  ret := avcodec_parameters_from_context(stream.codecpar, c);
  // replaces avcodec_get_context_defaults3
  assert(ret >= 0);

  ret := avio_open(@oc.pb, PAnsiChar(@bytes[0]), AVIO_FLAG_WRITE);
  assert(ret >= 0);
  ret := avformat_write_header(oc, @CodecSetup.OptionsDictionary);
  assert(ret >= 0);

  // Allocating memory for conversion output YUV frame:
  yuvpic := av_frame_alloc();
  assert(yuvpic <> nil);
  yuvpic.Format := Ord(CodecSetup.OutputPixelFormat);
  yuvpic.Width := fWidth;
  yuvpic.Height := fHeight;
  ret := av_frame_get_buffer(yuvpic, 0);
  assert(ret >= 0);

  // Allocating memory for packet
  pkt := av_packet_alloc();
  assert(pkt <> nil);

end;

function FloatToFrac(x: double; acc: byte): string;
var
  fact: integer;
  // acc not too large
  b: byte;
  num, den: integer;
begin
  fact := 1;
  for b := 1 to acc do
    fact := 10 * fact;
  num := round(x * fact);
  den := fact;
  result := inttostr(num) + '/' + inttostr(den);
end;




destructor TBitmapEncoderM.Destroy;
begin
  CodecSetup.Free;
  av_frame_free(@yuvpic);
  avformat_free_context(oc);
  avcodec_free_context(@c);
  av_packet_free(@pkt);
  inherited;
end;

end.
