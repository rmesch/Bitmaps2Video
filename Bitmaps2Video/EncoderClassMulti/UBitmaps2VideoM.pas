{ *****************************************************************************
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
  Renate Schaaf (original code and extensions)
  Markus Humm (made it compile and run under Android)
  HuichanKIM (routine for faster adding of bitmaps)
  ***************************************************************************** }
unit UBitmaps2VideoM;

interface

uses
  System.SysUtils,
  FFMPEG,
  FMX.Graphics,
  System.types,
  UFormatsM,
  System.Classes,
  UToolsM;

type
  /// <summary> Libav-Scaling used if bitmapsize<>videosize </summary>
  TVideoScaling = (vsFastBilinear, vsBilinear, vsBiCubic, vsLanczos);

  /// <summary> Zoom algorithms, the higher the better and slower not yet implemented
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

  // Compatible with windows TRGBTriple
  TBGR = record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
  end;

  PBGR = ^TBGR;

  TBitmapEncoderM = class
  private
    fWidth, fHeight, fRate: integer;
    fQuality: Byte;
    fFilename: string;
    fFrameCount: integer;
    fVideoTime: int64;
    fVideoScaling: TVideoScaling;
    fCodecId: TAVCodecId;
    fInputVideoTimebase: TAVRational;
    fVideoFrameCount: integer;
    fVideoFrameStart: integer;
    fBackground: Byte;
    fRGB_pix_fmt: TAVPixelFormat;
    VideoPts, VideoDts: int64;
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
    procedure BitmapToFrame(const bm: TBitmap);
    procedure ColorToFrame(Color: TBGR);
    procedure BitmapToRGBFrame(const bm: TBitmap; const rgbFrame: PAVFrame);
    procedure ExpandToAspectRatio(const FrameSrc, FrameDst: PAVFrame;
      AR: TAVRational; Background: Byte);
    procedure ExpandToVideoSize(const FrameSrc, FrameDst: PAVFrame);
    procedure RGBFrameToFrame(const aFrame: PAVFrame);
    procedure Alphablend(fSrc1, fSrc2, fDst: PAVFrame; alpha: Byte);
    procedure ZoomDeleteScans(const Src, Dest: PAVFrame; rs: TRectF);
    procedure ZoomResampleTripleOnly(const Source, Target: PAVFrame;
      SourceRect: TRectF; Radius: single);
  public
    DroppedFrames: integer;
    /// <param name="filename"> Output filename. Extension picks the container format (.mp4 .avi .mkv ..)
    /// </param>
    /// <param name="Width"> Video size in pixels (multiples of 2)</param>
    /// <param name="Height"> Video size in pixels (multiples of 2)</param>
    /// <param name="FrameRate"> Frames per second (can be slightly off in the resulting file)</param>
    /// <param name="Quality"> Encoding quality, scale from 0 to 100. 0 gives marginally acceptable quality </param>
    /// <param name="CodecId"> (TAVCodecID see UFormats.pas) Identifier of the codec to use. AV_CODEC_ID_NONE picks the default codec for the file extension. </param>
    /// <param name="VideoScaling">Resample algo used if video size differs from bitmap size </param>
    constructor Create(const filename: string; Width, Height: integer;
      FrameRate: integer; Quality: Byte; CodecId: TAVCodecId = AV_CODEC_ID_NONE;
      VideoScaling: TVideoScaling = vsFastBilinear; Background: Byte = 0);

    /// <summary> Turn a Bitmap into a movie frame. If its aspect ratio does not match the
    /// one of the movie, a grayscale background will be added (see Background in Create)</summary>
    /// <param name="bm"> Bitmap(TBitmap) to be fed to the video. </param>
    procedure AddFrame(const bm: TBitmap);

    /// <summary>Add a uniformly colored Frame to the Video</summary>
    procedure AddColorFrame(Color: TBGR);

    /// <summary> Hold the last frame </summary>
    /// <param name="EffectTime"> Displaytime(integer) in ms </param>
    procedure Freeze(EffectTime: integer);

    /// <summary> Add a picture which is displayed for a certain time </summary>
    /// <param name="bm"> Bitmap(TBitmap) of the picture to be displayed </param>
    /// <param name="ShowTime"> Time(integer) in ms for the display </param>
    procedure AddStillImage(const bm: TBitmap; ShowTime: integer);

    /// <summary> Add an existing video to the video stream. It will be resized and reencoded with the current settings.
    /// The format of the video can be anything that VLC-player plays.
    /// If its aspect ratio does not match the
    /// one of the movie, a grayscale background will be added (see Background in Create)</summary>
    procedure AddVideo(const VideoInput: string);

    /// <summary> Make a smooth transition between 2 zoom rects within EffectTime.</summary>
    /// <param name="bm"> The Bitmap(TBitmap) to be animated </param>
    /// <param name="zSrc"> Beginning zoom into rect(0,0,bm.width,bm.height) (see TZoom) </param>
    /// <param name="zDst"> End zoom into rect(0,0,bm.width,bm.height) </param>
    /// <param name="EffectTime"> Duration(integer) of the animation in ms </param>
    /// <param name="ZoomOption"> Quality of the zoom (zoAAx2, zoAAx4, zoAAx6, zoResample). </param>
    /// <param name="SpeedEnvelope"> Modifies the speed during EffectTime. (zeFastSlow, zeSlowFast, zeSlowSlow, zeLinear) </param>
    procedure ZoomPan(const bm: TBitmap; zSrc, zDst: TZoom; EffectTime: integer;
      ZoomOption: TZoomOption; SpeedEnvelope: TZoomSpeedEnvelope = zeLinear);

    procedure CrossFade(const am, bm: TBitmap; EffectTime: integer);

    /// <summary> Close the file and make the output file usable. </summary>
    function CloseFile: boolean;

    destructor Destroy; override;

    /// <summary> how many frames have been added to the movie so far </summary>
    property FrameCount: integer read fFrameCount;

    /// <summary> Videotime so far in ms, more accurate if the frame rate varies slightly </summary>
    property Videotime: int64 read fVideoTime;

    /// <summary> Frame count of the last video clip added, use for grabbing thumbnails or timing </summary>
    property LastVideoFrameCount: integer read fVideoFrameCount;

    /// <summary> Event which fires every second of video time while writing. Use it to update a progressbar etc.. </summary>
    property OnProgress: TVideoProgressEvent read fOnProgress write fOnProgress;

  end;

  /// <summary> Estimates the size of the output video in MB for the given settings </summary>
function VideoSizeInMB(Videotime: int64; CodecId: TAVCodecId;
  Width, Height, Rate: integer; Quality: Byte): double;

/// <summary> Combine the 1st video stream from VideoFile with 1st audio stream from Audiofile.  The streams will just be copied, not encoded.
/// Audio is clipped to video length. Raises exception if the format of the audio file is not supported.</summary>
/// <param name="VideoFile"> (string) File which contains a video stream. Any audio stream present will be ignored. </param>
/// <param name="AudioFile"> (string) Genuine audio file (.wav .mp3 .aac) or any file containing an audio stream. This will be added to the video in VideoFile. </param>
/// <param name="OutputFile"> (string) Name of the file containing the newly combined video and audio. </param>
procedure MuxStreams2(const VideoFile, AudioFile: string;
  const OutputFile: string);

implementation

uses System.UITypes, math;

{ TBitmapEncoder }

function VideoSizeInMB(Videotime: int64; CodecId: TAVCodecId;
  Width, Height, Rate: integer; Quality: Byte): double;
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
  pkt: TAvPacket;
begin
  result := true;
  if fFrameCount mod fRate = 0 then // update once per second
  begin
    if assigned(fOnProgress) then
      fOnProgress(fVideoTime);
  end;
  av_init_packet(@pkt);
  pkt.data := nil;
  pkt.size := 0;
  // leave frame.pts at default, otherwise mpeg-1 and mpeg-2 aren't happy
  ret := avcodec_send_frame(c, frame);
  if ret >= 0 then
    while true do
    begin
      ret := avcodec_receive_packet(c, @pkt);
      // Could there be a bug in ffmpeg.pas mixing up AVERROR_EAGAIN and AVERROR_EDEADLK for Android?
      if (ret = AVERROR_EOF) or (ret = AVERROR_EAGAIN) or (ret = AVERROR_EDEADLK)
      then
      begin
        av_packet_unref(@pkt);
        exit;
      end
      else if ret < 0 then
      begin
        Raise Exception.Create('Package read error');
        exit;
      end;

      // Adjust the frame rate, now works
      if FromVideo then
      begin
        pkt.dts := VideoDts;
        pkt.pts := VideoPts;
        av_packet_rescale_ts(@pkt, fInputVideoTimebase, c.time_base);
        pkt.pts := fVideoFrameStart + pkt.pts;
        pkt.dts := fVideoFrameStart + pkt.dts;
        fFrameCount := pkt.dts;
      end
      else
      begin
        pkt.pts := fFrameCount;
        pkt.dts := fFrameCount;
        inc(fFrameCount);
      end;
      pkt.duration := 1;

      av_packet_rescale_ts(@pkt, c.time_base, stream.time_base);
      pkt.stream_index := stream.index;

      // Write the encoded frame to the video file.
      // Does not seem to fail anymore, ret<0 should maybe throw an exception
      ret := av_interleaved_write_frame(oc, @pkt);
      result := (ret >= 0);
      av_packet_unref(@pkt);
      fVideoTime := av_rescale_q(av_stream_get_end_pts(stream),
        stream.time_base, av_make_q(1, 1000));
    end
  else
    result := false;
  if not result then
    inc(DroppedFrames);
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
    encode(yuvpic, false);
    time := time + frametime;
  end;
end;

// we need to make the direct pixel access of the bitmap as short as possible, so
// we copy the bm to an RGBFrame first thing
// fast pixel access as suggested by HuichanKIM
procedure TBitmapEncoderM.BitmapToRGBFrame(const bm: TBitmap;
  const rgbFrame: PAVFrame);
var
  w, h, y: integer;
  stride, bps, jump: integer;
  ret: integer;
  BitData: TBitmapData;
  rowB, rowF: PByte;
const
  Epsilon = 0.05;
begin
  Assert((bm.Width > 0) and (bm.Height > 0));
  w := bm.Width;
  h := bm.Height;
  rgbFrame.Width := w;
  rgbFrame.Height := h;
  rgbFrame.format := Ord(fRGB_pix_fmt);
  ret := av_frame_get_buffer(rgbFrame, 0);
  Assert(ret >= 0);
  Assert(bm.Map(TMapAccess.Read, BitData));
  try
    // Pitch is more reliable than BytesPerLine
    stride := BitData.Pitch;
    bps := 4 * w;
    jump := rgbFrame.linesize[0];
    ret := av_frame_make_writable(rgbFrame);
    Assert(ret >= 0);

    // This is more reliable than a convertCtx
    rowB := BitData.GetScanline(0);
    rowF := rgbFrame.data[0];
    for y := 0 to h - 1 do
    begin
      Move(rowB^, rowF^, bps);
      inc(rowB, stride);
      inc(rowF, jump);
    end;
  finally
    bm.Unmap(BitData);
  end;

end;

procedure TBitmapEncoderM.RGBFrameToFrame(const aFrame: PAVFrame);
var
  convertCtx: PSwsContext;
  ret: integer;
  AspectDiffers: boolean;
  rgbFrame2, rgbFrameSource: PAVFrame;
const
  Epsilon = 0.05;
begin
  Assert((aFrame.Width > 0) and (aFrame.Height > 0));
  AspectDiffers := Abs(aFrame.Width / aFrame.Height - fWidth / fHeight)
    > Epsilon;
  rgbFrame2 := nil;
  if AspectDiffers then
  begin
    rgbFrame2 := av_frame_alloc;
    ExpandToAspectRatio(aFrame, rgbFrame2, av_make_q(fWidth, fHeight),
      fBackground);
    convertCtx := sws_getContext(rgbFrame2.Width, rgbFrame2.Height,
      fRGB_pix_fmt, fWidth, fHeight, CodecSetup.OutputPixelFormat,
      ScaleFunction[fVideoScaling], nil, nil, nil);
    Assert(convertCtx <> nil);
    rgbFrameSource := rgbFrame2;
  end
  else
  begin
    convertCtx := sws_getContext(aFrame.Width, aFrame.Height, fRGB_pix_fmt,
      fWidth, fHeight, CodecSetup.OutputPixelFormat,
      ScaleFunction[fVideoScaling], nil, nil, nil);
    Assert(convertCtx <> nil);
    rgbFrameSource := aFrame;
  end;
  try
    ret := av_frame_make_writable(yuvpic);
    Assert(ret >= 0);
    ret := sws_scale(convertCtx, @rgbFrameSource.data, @rgbFrameSource.linesize,
      0, rgbFrameSource.Height, @yuvpic.data, @yuvpic.linesize);
    Assert(ret >= 0);
  finally
    sws_freeContext(convertCtx);
    if assigned(rgbFrame2) then
      av_frame_free(@rgbFrame2);
  end;
end;

procedure TBitmapEncoderM.BitmapToFrame(const bm: TBitmap);
var
  rgbFrame1: PAVFrame;
begin
  rgbFrame1 := av_frame_alloc;
  try
    BitmapToRGBFrame(bm, rgbFrame1);
    RGBFrameToFrame(rgbFrame1);
  finally
    av_frame_free(@rgbFrame1);
  end;

end;

procedure TBitmapEncoderM.AddColorFrame(Color: TBGR);
begin
  ColorToFrame(Color);
  encode(yuvpic, false);
end;

procedure TBitmapEncoderM.AddFrame(const bm: TBitmap);
begin
  // Store the Bitmap in the yuv-frame
  BitmapToFrame(bm);


  // Encode the yuv-frame

  encode(yuvpic, false);

end;

procedure TBitmapEncoderM.AddStillImage(const bm: TBitmap; ShowTime: integer);

begin
  BitmapToFrame(bm);

  // Encode the yuv-frame repeatedly
  Freeze(ShowTime);
end;

function TBitmapEncoderM.CloseFile: boolean;
begin
  // flush the encoder
  av_write_frame(oc,nil);

  av_write_trailer(oc); // Writing the end of the file.
  if ((fmt.flags and AVFMT_NOFILE) = 0) then
    avio_closep(@oc.pb);
  // Closing the file.
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
  convertCtx := sws_getContext(fWidth, fHeight, AV_PIX_FMT_BGR24, fWidth,
    fHeight, CodecSetup.OutputPixelFormat, ScaleFunction[fVideoScaling], nil,
    nil, nil);
  // Video ignores the alpha-channel. Size will be scaled if necessary,
  // proportionality might not be preserved
  Assert(convertCtx <> nil);

  // Allocate storage for the rgb-frame
  rgbpic := av_frame_alloc();
  Assert(rgbpic <> nil);
  try
    rgbpic.format := Ord(AV_PIX_FMT_BGR24);
    rgbpic.Width := fWidth;
    rgbpic.Height := fHeight;
    av_frame_get_buffer(rgbpic, 0);

    // Store the color in the frame
    ret := av_frame_make_writable(rgbpic);
    Assert(ret >= 0);

    py := @PByte(rgbpic.data[0])[0];
    jump := rgbpic.linesize[0];
    for y := 0 to fHeight - 1 do
    begin
      px := py;
      for x := 0 to fWidth - 1 do
      begin
        PBGR(px)^ := Color;
        inc(px, 3);
      end;
      inc(py, jump);
    end;

    // Convert the rgb-frame to yuv-frame
    ret := av_frame_make_writable(yuvpic);
    Assert(ret >= 0);
    ret := sws_scale(convertCtx, @rgbpic.data, @rgbpic.linesize, 0, fHeight,
      @yuvpic.data, @yuvpic.linesize);
    Assert(ret >= 0);
  finally
    sws_freeContext(convertCtx);
    av_frame_free(@rgbpic);

  end;
end;

constructor TBitmapEncoderM.Create(const filename: string;
  Width, Height, FrameRate: integer; Quality: Byte;
  CodecId: TAVCodecId = AV_CODEC_ID_NONE;
  VideoScaling: TVideoScaling = vsFastBilinear; Background: Byte = 0);
var
  ret: integer;
begin
  fFilename := filename;
  fWidth := Width;
  fHeight := Height;
  fRate := FrameRate;
  fQuality := Quality;
  fVideoScaling := VideoScaling;
  fFrameCount := 0;
  DroppedFrames := 0;
  fBackground := Background;
  // the pixel-format of all rgb-frames matches the one of the operating system
{$IFDEF ANDROID}
  fRGB_pix_fmt := AV_PIX_FMT_RGBA;
{$ELSE}
  fRGB_pix_fmt := AV_PIX_FMT_BGRA;
{$ENDIF}
  if CodecId = AV_CODEC_ID_NONE then
    fCodecId := PreferredCodec(ExtractFileExt(fFilename))
  else
    fCodecId := CodecId;

  CodecSetup := CodecSetupClass(fCodecId).Create(fCodecId);

  fmt := GetOutputFormat(ExtractFileExt(fFilename));
  Assert(fmt <> nil, 'No matching format');
  oc := nil;
  ret := avformat_alloc_output_context2(@oc, nil, nil,
    MarshaledAString(UTF8String(filename)));
  Assert(ret >= 0, 'avformat_alloc.. error' + inttostr(ret));
  stream := avformat_new_stream(oc, nil);
  Assert(stream <> nil, 'avformat_new_stream failed');
  codec := CodecSetup.codec;
  Assert(codec <> nil, 'codec not found');

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
  Assert(ret >= 0);
  stream.time_base := c.time_base;
  ret := avcodec_parameters_from_context(stream.codecpar, c);
  // replaces avcodec_get_context_defaults3
  Assert(ret >= 0);

  ret := avio_open(@oc.pb, MarshaledAString(UTF8String(filename)),
    AVIO_FLAG_WRITE);
  Assert(ret >= 0);
  ret := avformat_write_header(oc, @CodecSetup.OptionsDictionary);
  Assert(ret >= 0);

  // Allocating memory for conversion output YUV frame:
  yuvpic := av_frame_alloc();
  Assert(yuvpic <> nil);
  yuvpic.format := Ord(CodecSetup.OutputPixelFormat);
  yuvpic.Width := fWidth;
  yuvpic.Height := fHeight;
  ret := av_frame_get_buffer(yuvpic, 0);
  Assert(ret >= 0);

  // Allocating memory for packet
  pkt := av_packet_alloc();
  Assert(pkt <> nil);

end;

procedure TBitmapEncoderM.CrossFade(const am, bm: TBitmap; EffectTime: integer);
var
  fSrc1, fSrc2, temp: PAVFrame;
  frametime, time, fact: double;
begin
  fSrc1 := av_frame_alloc;
  try
    temp := av_frame_alloc;
    try
      BitmapToRGBFrame(am, temp);
      ExpandToVideoSize(temp, fSrc1);
    finally
      av_frame_free(@temp);
    end;
    fSrc2 := av_frame_alloc;
    try
      temp := av_frame_alloc;
      try
        BitmapToRGBFrame(bm, temp);
        ExpandToVideoSize(temp, fSrc2);
      finally
        av_frame_free(@temp);
      end;

      frametime := 1000 / fRate;
      time := 0;
      fact := 1 / EffectTime;
      temp := av_frame_alloc;
      try
        temp.Width := fWidth;
        temp.Height := fHeight;
        temp.format := Ord(fRGB_pix_fmt);
        av_frame_get_buffer(temp, 0);
        while time < EffectTime do
        begin
          Alphablend(fSrc1, fSrc2, temp, trunc(time * fact * 255));
          RGBFrameToFrame(temp);
          encode(yuvpic, false);
          time := time + frametime;
        end;
      finally
        av_frame_free(@temp);
      end;
    finally
      av_frame_free(@fSrc2);
    end;
  finally
    av_frame_free(@fSrc1);
  end;
end;

procedure TBitmapEncoderM.AddVideo(const VideoInput: string);
var
  fmt_ctx: PAVFormatContext;
  video_dec_ctx: PAVCodecContext;
  Width, Height: integer;
  pix_fmt, rgb_fmt: TAVPixelFormat;
  video_stream_idx: integer;
  frame: PAVFrame;
  pkt: TAvPacket;
  ret, i: integer;
  got_frame: integer;
  video_dst_data: array [0 .. 3] of PByte;
  video_dst_linesize: array [0 .. 3] of integer;
  video_stream: PAVStream;
  convertCtx: PSwsContext;

  p: PPAVStream;
  sar: TAVRational;
  TrueHeight: integer;
  AspectDiffers: boolean;
  // args: array [0 .. 512 - 1] of AnsiChar;
  // buffersink_ctx: PAVFilterContext;
  // buffersrc_ctx: PAVFilterContext;
  // filter_graph: PAVFilterGraph;
  // f: PAVFrame;
  // inputs, outputs: PAVFilterInOut;
  // FilterInitialized: boolean;
  QuitLoop: boolean;
  // Filterstring: UTF8String;
const
  Epsilon = 0.05;

  function AddVideoFrame(aFrame: PAVFrame): boolean;
  var
    bm: TBitmap;
    BitmapData: TBitmapData;
    row: PByte;
    bps: integer;
  begin
    if AspectDiffers then
    begin
      bm := TBitmap.Create;
      try
        bm.SetSize(aFrame.Width, TrueHeight);
        bm.Map(TMapAccess.Write, BitmapData);
        try
          row := BitmapData.data;
          bps := BitmapData.Pitch;
          ret := sws_scale(convertCtx, @aFrame.data, @aFrame.linesize, 0,
            aFrame.Height, @row, @bps);
          Assert(ret >= 0);
        finally
          bm.Unmap(BitmapData);
        end;
        BitmapToFrame(bm);
      finally
        bm.Free;
      end;
    end
    else
    begin
      // scale the frame and change the pixel format, if necessary
      ret := av_frame_make_writable(yuvpic);
      Assert(ret >= 0);
      ret := sws_scale(convertCtx, @aFrame.data, @aFrame.linesize, 0, Height,
        @yuvpic.data, @yuvpic.linesize);
      Assert(ret >= 0);
    end;

    result := encode(yuvpic, true);
    av_packet_unref(@pkt);
  end;

begin
  Assert(UpperCase(fFilename) <> UpperCase(VideoInput),
    'Output file name must be different from input file name');
  // FilterInitialized := False;
  // filter_graph := nil;
  // inputs := nil;
  // outputs := nil;
  fmt_ctx := nil;
  video_dec_ctx := nil;
  // buffersrc_ctx := nil;
  // buffersink_ctx := nil;
  frame := nil;
  fVideoFrameCount := 0;
  for i := 0 to 3 do
    video_dst_data[i] := nil;
  (* open input file, and allocate format context *)
  ret := avformat_open_input(@fmt_ctx, MarshaledAString(UTF8String(VideoInput)),
    nil, nil);
  Assert(ret >= 0);

  (* retrieve stream information *)
  ret := avformat_find_stream_info(fmt_ctx, nil);
  Assert(ret >= 0);

  open_decoder_context(@video_stream_idx, @video_dec_ctx, fmt_ctx,
    AVMEDIA_TYPE_VIDEO);
  p := fmt_ctx.streams;
  inc(p, video_stream_idx);
  video_stream := p^;
  fInputVideoTimebase := video_stream.time_base;

  (* allocate image where the decoded image will be put *)
  Width := video_dec_ctx.Width;
  Height := video_dec_ctx.Height;
  pix_fmt := video_dec_ctx.pix_fmt;
  ret := av_image_alloc(@video_dst_data[0], @video_dst_linesize[0], Width,
    Height, pix_fmt, 1);
  Assert(ret >= 0);

  // calculate the true aspect ratio based on sample aspect ratio (ar of one pixel in the source)
  sar := video_dec_ctx.sample_aspect_ratio;
  if (sar.num > 0) and (sar.den > 0) then
    TrueHeight := round(Height * sar.den / sar.num)
  else
    TrueHeight := Height;
  AspectDiffers := Abs(fWidth / fHeight - Width / TrueHeight) > Epsilon;
  if AspectDiffers then
  begin
{$IFDEF ANDROID}
    rgb_fmt := AV_PIX_FMT_RGBA;
{$ELSE}
    rgb_fmt := AV_PIX_FMT_BGRA;
{$ENDIF}
    // We convert the frames to bitmaps and then encode the bitmaps
    convertCtx := sws_getContext(Width, Height, pix_fmt, Width, TrueHeight,
      rgb_fmt, ScaleFunction[fVideoScaling], nil, nil, nil);
  end
  else
  begin
    // If the aspect is OK just resize and change the pix-format
    convertCtx := sws_getContext(Width, Height, pix_fmt, fWidth, fHeight,
      c.pix_fmt, ScaleFunction[fVideoScaling], nil, nil, nil);
  end;
  frame := av_frame_alloc();
  Assert(frame <> nil);
  try
    (* initialize packet, set data to nil, let the demuxer fill it *)
    av_init_packet(@pkt);
    pkt.data := nil;
    pkt.size := 0;
    (* read frames from the file *)
    fVideoFrameStart := fFrameCount + 1;
    fVideoFrameCount := 0;
    QuitLoop := false;
    while true do
    begin
      ret := av_read_frame(fmt_ctx, @pkt);
      if ret < 0 then
        break;
      if pkt.stream_index <> video_stream_idx then
      begin
        av_packet_unref(@pkt);
        Continue;
      end;
      (* decode video frame *)
      // !avcodec_decode_video2 is deprecated, but I couldn't get
      // the replacement avcode_send_packet and avcodec_receive_frame to work
      got_frame := 0;
      ret := avcodec_decode_video2(video_dec_ctx, frame, @got_frame, @pkt);
      Assert(ret >= 0);

      if (got_frame <> 0) then
      begin
        inc(fVideoFrameCount);
        // This is needed to give the frames the right decoding- and presentation- timestamps
        // see encode for FromVideo = true
        VideoPts := pkt.pts;
        VideoDts := pkt.dts;
        if VideoPts < VideoDts then
          VideoPts := VideoDts;
        // Deinterlace the frame if necessary
        /// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // Couldn't get deinterlacing to work under Android
        // if frame.interlaced_frame = 0 then
        QuitLoop := QuitLoop or (not AddVideoFrame(frame));
        {
          else
          begin
          If not FilterInitialized then
          begin

          // Configure the de-interlace filter (yadif)
          FillChar(args[0], length(args) * sizeof(AnsiChar), #0);
          Filterstring :=
          UTF8String('buffer=video_size=' + inttostr(video_dec_ctx.Width) +
          'x' + inttostr(video_dec_ctx.Height) + ':pix_fmt=' +
          inttostr(Ord(video_dec_ctx.pix_fmt)) +
          ':time_base=1/1:pixel_aspect=0/1[in];[in]yadif[out];[out]buffersink');
          Move(Filterstring[1], args[0], length(Filterstring) *
          sizeof(AnsiChar));
          filter_graph := avfilter_graph_alloc();
          inputs := nil;
          outputs := nil;
          ret := avfilter_graph_parse2(filter_graph, MarshaledAString(@args[0]),
          @inputs, @outputs);
          Assert(ret >= 0,'Error= '+IntToStr(ret));
          ret := avfilter_graph_config(filter_graph, nil);
          Assert(ret >= 0);
          buffersrc_ctx := avfilter_graph_get_filter(filter_graph,
          'Parsed_buffer_0');
          buffersink_ctx := avfilter_graph_get_filter(filter_graph,
          'Parsed_buffersink_2');
          Assert(buffersrc_ctx <> nil);
          Assert(buffersink_ctx <> nil);

          FilterInitialized := true;
          end;
          f := nil;
          f := av_frame_alloc();
          try
          av_frame_ref(f, frame);
          av_buffersrc_add_frame(buffersrc_ctx, f);
          while true do
          begin
          ret := av_buffersink_get_frame(buffersink_ctx, f);
          // AVError_EAgain means that more data are needed to fill the frame
          if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) or
          (ret = AVERROR_EDEADLK) then
          break;
          Assert(ret >= 0);
          QuitLoop := QuitLoop or (not AddVideoFrame(f));
          end;
          finally
          av_frame_free(@f);
          end;
          end;
        }
      end; // if got_frame
    end; // while true
    Assert(not QuitLoop, 'Some video frames could not be added');
  finally
    {
      if assigned(inputs) then
      avfilter_inout_free(@inputs);
      if assigned(outputs) then
      avfilter_inout_free(@outputs);
      if assigned(filter_graph) then
      avfilter_graph_free(@filter_graph);
    }
    av_freep(@video_dst_data[0]);
    av_frame_free(@frame);
    avcodec_free_context(@video_dec_ctx);
    sws_freeContext(convertCtx);
    avformat_close_input(@fmt_ctx);
  end;
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

procedure TBitmapEncoderM.ExpandToAspectRatio(const FrameSrc,
  FrameDst: PAVFrame; AR: TAVRational; Background: Byte);
var
  xstart, ystart: integer;
  ret: integer;
  bmRow, FrameRow, FrameStart: PByte;
  y, bps, jumpB, jumpF: integer;
begin
  Assert((FrameSrc.Width > 0) and (FrameSrc.Height > 0));
  if FrameSrc.Width / FrameSrc.Height < AR.num / AR.den then
  // Add background right and left
  begin
    FrameDst.Height := FrameSrc.Height;
    FrameDst.Width := round(FrameSrc.Height / AR.den * AR.num);
    xstart := (FrameDst.Width - FrameSrc.Width) div 2;
    ystart := 0;
  end
  else
  begin
    FrameDst.Width := FrameSrc.Width;
    FrameDst.Height := round(FrameSrc.Width / AR.num * AR.den);
    xstart := 0;
    ystart := (FrameDst.Height - FrameSrc.Height) div 2;
  end;
  // Set up the Frame with the right pixelformat

  FrameDst.format := Ord(fRGB_pix_fmt);
  ret := av_frame_get_buffer(FrameDst, 0);
  Assert(ret >= 0);
  av_frame_make_writable(FrameDst);
  // Fill the frame with gray 0:black 255:white
  FillChar(FrameDst.data[0]^, FrameDst.Height * FrameDst.linesize[0],
    Background);

  // Copy FrameSrc to FrameDst
  bmRow := FrameSrc.data[0];
  FrameRow := FrameDst.data[0];
  bps := 4 * FrameSrc.Width;
  jumpF := FrameDst.linesize[0];
  jumpB := FrameSrc.linesize[0];
  inc(FrameRow, ystart * jumpF);
  xstart := 4 * xstart;
  for y := 0 to FrameSrc.Height - 1 do
  begin
    FrameStart := FrameRow;
    inc(FrameStart, xstart);
    Move(bmRow^, FrameStart^, bps);
    inc(bmRow, jumpB);
    inc(FrameRow, jumpF);
  end;
end;

procedure TBitmapEncoderM.ExpandToVideoSize(const FrameSrc, FrameDst: PAVFrame);
var
  temp: PAVFrame;
  convertCtx: PSwsContext;
begin
  temp := av_frame_alloc;
  try
    ExpandToAspectRatio(FrameSrc, temp, av_make_q(fWidth, fHeight),
      fBackground);
    convertCtx := sws_getContext(temp.Width, temp.Height, fRGB_pix_fmt, fWidth,
      fHeight, fRGB_pix_fmt, ScaleFunction[fVideoScaling], nil, nil, nil);
    try
      FrameDst.Width := fWidth;
      FrameDst.Height := fHeight;
      FrameDst.format := Ord(fRGB_pix_fmt);
      av_frame_get_buffer(FrameDst, 0);
      av_frame_make_writable(FrameDst);
      sws_scale(convertCtx, @temp.data, @temp.linesize, 0, temp.Height,
        @FrameDst.data, @FrameDst.linesize);
    finally
      sws_freeContext(convertCtx);
    end;
  finally
    av_frame_free(@temp);
  end;
end;

procedure TBitmapEncoderM.ZoomPan(const bm: TBitmap; zSrc, zDst: TZoom;
  EffectTime: integer; ZoomOption: TZoomOption;
  SpeedEnvelope: TZoomSpeedEnvelope = zeLinear);
var
  asp: double;
  aw, ah: integer; // antialiased width height
  elapsed: integer;
  Src, trg, mid: TRectF;
  targetTime: integer;
  frametime, t: double;
  evf: TEnvelopeFunction;
  bw, bh: integer;
  frRGB, frSrc, frDst: PAVFrame;
  ret: integer;
begin
  evf := EnvelopeFunction[SpeedEnvelope];
  bw := bm.Width;
  bh := bm.Height;
  frSrc := av_frame_alloc; // Antialias frame
  try
    frRGB := av_frame_alloc;
    try
      BitmapToRGBFrame(bm, frRGB);
      ah := frRGB.Height; // suppress compiler warning
      case ZoomOption of
        zoAAx2:
          ah := 2 * fHeight;
        zoAAx4:
          ah := 4 * fHeight;
        zoAAx6:
          ah := 6 * fHeight;
        // 6*fHeight might give an EOutOfResources if there is no
        // suffiently large memory block available, less likely for 64-bit
        zoResample:
          ah := fHeight + 100;
      end;
      asp := bw / bh;
      aw := round(ah * asp);
      frSrc.Width := aw;
      frSrc.Height := ah;
      frSrc.format := Ord(fRGB_pix_fmt);
      ret := av_frame_get_buffer(frSrc, 0);
      Assert(ret >= 0);
      // start with a nice upscale
      ZoomResampleTripleOnly(frRGB, frSrc, Rect(0, 0, bw, bh), 1.8)
    finally
      av_frame_free(@frRGB);
    end;
    Src := zSrc.ToRect(aw, ah);
    trg := zDst.ToRect(aw, ah); // scale zooms to rects for antialias-size
    frametime := 1000 / fRate;
    elapsed := 0;
    targetTime := round(EffectTime - 0.5 * frametime);
    frDst := av_frame_alloc;
    try
      frDst.Height := fHeight;
      frDst.Width := round(fHeight * bw / bh);
      frDst.format := Ord(fRGB_pix_fmt);
      ret := av_frame_get_buffer(frDst, 0);
      Assert(ret >= 0);
      while elapsed < targetTime do
      begin
        t := elapsed / EffectTime;
        mid := Interpolate(Src, trg, evf(t));
        case ZoomOption of
          zoAAx2, zoAAx4, zoAAx6:
            ZoomDeleteScans(frSrc, frDst, mid);
          zoResample:
            ZoomResampleTripleOnly(frSrc, frDst, mid, 1.5);
          // radius 1.5 is good enough
        end;
        // feed frDst to the encoder
        RGBFrameToFrame(frDst);
        encode(yuvpic, false);
        elapsed := round(elapsed + frametime);
      end;
    finally
      av_frame_free(@frDst);
    end;
  finally
    av_frame_free(@frSrc);
  end;
end;

// Alphablend rgb-frames
// Must be of the same size
// fDst = alpha*(fSrc2-fSrc1)/255 + fSrc1
procedure TBitmapEncoderM.Alphablend(fSrc1, fSrc2, fDst: PAVFrame; alpha: Byte);
var
  x, y, jump: integer;
  rowSrc1, rowSrc2, rowDst: PByte;
  bSrc1, bSrc2, bDst: PByte;
begin
  av_frame_make_writable(fDst);
  rowSrc1 := fSrc1.data[0];
  rowSrc2 := fSrc2.data[0];
  rowDst := fDst.data[0];
  jump := fDst.linesize[0];
  for y := 0 to fDst.Height - 1 do
  begin
    bSrc1 := rowSrc1;
    bSrc2 := rowSrc2;
    bDst := rowDst;
    for x := 0 to fDst.linesize[0] - 1 do
    begin
      bDst^ := (alpha * (bSrc2^ - bSrc1^)) shr 8 + bSrc1^;
      inc(bSrc1);
      inc(bSrc2);
      inc(bDst);
    end;
    inc(rowSrc1, jump);
    inc(rowSrc2, jump);
    inc(rowDst, jump);
  end;
end;

type
  TContributor = record
    Min, High: integer;
    // Min: start source pixel
    // High+1: number of source pixels to contribute to the result
    Weights: array of integer; // doubles scaled by $800
  end;

  TContribArray = array of TContributor;

  TIntArray = array of integer;

{$IFDEF ANDROID}

  TQuad = record
    r, g, b, a: Byte;
  end;
{$ELSE}

  TQuad = record
    b, g, r, a: Byte;
  end;
{$ENDIF}

  PQuad = ^TQuad;

const
  beta = 0.54; // f(beta)=0
  beta2 = beta * beta;
  alpha = 105 / (16 - 112 * beta2);
  aa = 1 / 7 * alpha;
  bb = -1 / 5 * alpha * (2 + beta2);
  cc = 1 / 3 * alpha * (1 + 2 * beta2);
  dd = -alpha * beta2;
  // constants computed with maple for polynomial fit

function AntiMyFilter(x: double): double; inline;
// Antiderivative of a filter function similar to bicubic, but I like it better
begin
  if x < -1 then
    result := -0.5
  else if x < 1 then
    result := aa * x * x * x * x * x * x * x + bb * x * x * x * x * x + cc * x *
      x * x + dd * x

  else
    result := 0.5;
end;

procedure MakeContributors(r: single; SourceSize, TargetSize: integer;
  SourceStart, SourceFloatwidth: double; var Contribs: TContribArray);
// r: Filterradius
var
  xCenter, scale, rr: double;
  x, j: integer;
  x1, x2, delta: double;
  TrueMin, TrueMax, Mx: integer;
begin
  if SourceFloatwidth = 0 then
    SourceFloatwidth := SourceSize;
  scale := SourceFloatwidth / TargetSize;
  SetLength(Contribs, TargetSize);

  if scale > 1 then
    // downsampling
    rr := r * scale
  else
    rr := r;
  delta := 1 / rr;

  for x := 0 to TargetSize - 1 do
  begin
    xCenter := (x + 0.5) * scale;
    TrueMin := Ceil(xCenter - rr + SourceStart - 1);
    TrueMax := Floor(xCenter + rr + SourceStart);
    Contribs[x].Min := Min(Max(TrueMin, 0), SourceSize - 1);
    // make sure not to read in negative pixel locations
    Mx := Max(Min(TrueMax, SourceSize - 1), 0);
    // make sure not to read past w1-1 in the source
    Contribs[x].High := Mx - Contribs[x].Min;
    Assert(Contribs[x].High >= 0); // hasn't failed lately:)
    // High=Number of contributing pixels minus 1
    SetLength(Contribs[x].Weights, Contribs[x].High + 1);
    with Contribs[x] do
    begin
      x1 := delta * (Min - SourceStart - xCenter);
      for j := 0 to High do
      begin
        x2 := x1 + delta;
        Weights[j] := round($800 * (AntiMyFilter(x2) - AntiMyFilter(x1)));
        x1 := x2;
      end;
      for j := TrueMin - Min to -1 do
      begin
        // assume the first pixel to be repeated
        x1 := delta * (Min + j - SourceStart - xCenter);
        x2 := x1 + delta;
        Weights[0] := Weights[0] +
          round($800 * (AntiMyFilter(x2) - AntiMyFilter(x1)));
      end;
      for j := High + 1 to TrueMax - Min do
      begin
        // assume the last pixel to be repeated
        x1 := delta * (Min + j - SourceStart - xCenter);
        x2 := x1 + delta;
        Weights[High] := Weights[High] +
          round($800 * (AntiMyFilter(x2) - AntiMyFilter(x1)));
      end;
    end; { with Contribs[x] }
  end; { for x }
end;

// Source and Target must have been allocated, the right size and format and memory must have been allocated
procedure TBitmapEncoderM.ZoomResampleTripleOnly(const Source, Target: PAVFrame;
  SourceRect: TRectF; Radius: single);
var
  ContribsX, ContribsY: TContribArray;

  OldWidth, OldHeight: integer;
  NewWidth, NewHeight: integer;
  // Target needs to be set to correct size

  Sbps, Tbps: integer;
  tr, tg, tb: integer; // total red etc.
  dr, dg, db: integer; // For Subsum
  ps, pT: PQuad;
  rs, rT, rStart, rTStart: PByte; // Row start in Source, Target
  weightx, weighty, weightxStart: PInteger;
  rx, gx, bx: TIntArray;
  x, y, xs, ys, ymin, ymax: integer;
  highx, highy, minx, miny: integer;
  runr, rung, runb: PInteger;
  runrstart, rungstart, runbstart: PInteger;
begin
  NewWidth := Target.Width;
  NewHeight := Target.Height;
  OldWidth := Source.Width;
  OldHeight := Source.Height;

  Tbps := Target.linesize[0];
  // BytesPerScanline Target
  Sbps := Source.linesize[0];
  // BytesPerScanline Source
  MakeContributors(Radius, OldWidth, NewWidth, SourceRect.Left,
    SourceRect.Right - SourceRect.Left, ContribsX);
  MakeContributors(Radius, OldHeight, NewHeight, SourceRect.Top,
    SourceRect.Bottom - SourceRect.Top, ContribsY);
  ymin := ContribsY[0].Min;
  ymax := ContribsY[NewHeight - 1].High + ContribsY[NewHeight - 1].Min;

  SetLength(rx, ymax - ymin + 1);
  SetLength(gx, ymax - ymin + 1);
  SetLength(bx, ymax - ymin + 1); // cache arrays

  av_frame_make_writable(Target);
  rStart := Source.data[0];
  inc(rStart, ymin * Source.linesize[0]);
  rTStart := Target.data[0];

  // Compute color at each target pixel (x,y)

  runrstart := @rx[0];
  rungstart := @gx[0];
  runbstart := @bx[0];

  for x := 0 to NewWidth - 1 do
  begin
    rs := rStart;
    highx := ContribsX[x].High;
    minx := ContribsX[x].Min;
    weightxStart := @ContribsX[x].Weights[0];
    runr := runrstart;
    rung := rungstart;
    runb := runbstart;
    for y := ymin to ymax do
    begin

      // For each source line y
      // Sum up weighted color values at source pixels ContribsX[x].Min+xs
      // 0<=xs<=ContribsX[x].High
      // and store the results in rx[y-ymin], gx[y-ymin] etc.
      ps := PQuad(rs);
      inc(ps, minx);

      weightx := weightxStart;
      db := weightx^ * ps.b;
      dg := weightx^ * ps.g;
      dr := weightx^ * ps.r;
      for xs := 1 to highx do
      begin
        inc(weightx);
        inc(ps);
        db := db + weightx^ * ps.b;
        dg := dg + weightx^ * ps.g;
        dr := dr + weightx^ * ps.r;
      end;
      // store results in rx,gx,bx
      runr^ := dr;
      rung^ := dg;
      runb^ := db;
      inc(runr);
      inc(rung);
      inc(runb);
      inc(rs, Sbps);
    end;
    // Average in y-direction:
    // For each target line y sum up weighted colors
    // rx[ys+ContribsY[y].Min-ymin], 0<=ys<=ContribsY[y].High, (same for gx, bx)
    // Store result in tr,tg,tb ("total red" etc.)
    // round and assign to TargetPixel[x,y]
    rT := rTStart;
    for y := 0 to NewHeight - 1 do
    begin
      pT := PQuad(rT);
      inc(pT, x);
      highy := ContribsY[y].High;
      miny := ContribsY[y].Min - ymin;
      weighty := @ContribsY[y].Weights[0];
      runr := runrstart;
      rung := rungstart;
      runb := runbstart;
      inc(runr, miny);
      inc(rung, miny);
      inc(runb, miny);

      tr := weighty^ * runr^;
      tb := weighty^ * runb^;
      tg := weighty^ * rung^;
      for ys := 1 to highy do
      begin
        inc(weighty);
        inc(runr);
        inc(rung);
        inc(runb);
        tr := tr + weighty^ * runr^;
        tb := tb + weighty^ * runb^;
        tg := tg + weighty^ * rung^;
      end;
      tr := Max(tr, 0);
      tg := Max(tg, 0);
      tb := Max(tb, 0); // results could be negative, filter has negative values
      tr := (tr + $1FFFFF) shr 22; // "round" the result
      tg := (tg + $1FFFFF) shr 22;
      tb := (tb + $1FFFFF) shr 22;
      pT.b := Min(tb, 255);
      pT.g := Min(tg, 255);
      pT.r := Min(tr, 255);
      inc(rT, Tbps);
    end; // for y

  end; // for x
end;

procedure MakeSteps(DestSize, SourceSize: integer;
  SourceLeft, SourceWidth: double; var steps: TIntArray; fact: integer); inline;
var
  shift, delta: double;
  x1, x2, i: integer;
  xx2: double;
begin
  SetLength(steps, DestSize);
  delta := SourceWidth / DestSize;
  shift := SourceLeft + 0.5 * delta;
  x1 := 0;
  xx2 := shift;
  x2 := Floor(xx2);
  if x2 > SourceSize - 1 then
    x2 := SourceSize - 1;
  steps[0] := (x2 - x1) * fact;
  x1 := x2;
  for i := 1 to DestSize - 1 do
  begin
    xx2 := xx2 + delta;
    x2 := Floor(xx2);
    if x2 > SourceSize - 1 then
      x2 := SourceSize - 1;
    steps[i] := (x2 - x1) * fact;
    x1 := x2;
  end;
end;

procedure TBitmapEncoderM.ZoomDeleteScans(const Src, Dest: PAVFrame;
  rs: TRectF);
var
  iwd, ihd, iws, ihs, bs, bd: integer;
  x, y: integer;
  xsteps, ysteps: TIntArray;
  Rows, rowd: PByte;
  Stepsx, Stepsy: PInteger;
  ts, td: PQuad;
begin

  iwd := Dest.Width;
  ihd := Dest.Height;
  Assert((iwd > 1) and (ihd > 1), 'Dest Frame too small');
  iws := Src.Width;
  ihs := Src.Height;

  av_frame_make_writable(Dest);
  bs := Src.linesize[0]; // BytesPerScanline Source
  bd := Dest.linesize[0]; // BytesPerScanline Dest
  MakeSteps(iwd, iws, rs.Left, rs.Right - rs.Left, xsteps, 1);
  MakeSteps(ihd, ihs, rs.Top, rs.Bottom - rs.Top, ysteps, bs);

  Rows := Src.data[0];
  rowd := Dest.data[0];
  Stepsy := @ysteps[0];

  for y := 0 to ihd - 1 do
  begin
    inc(Rows, Stepsy^);
    ts := PQuad(Rows);
    td := PQuad(rowd);
    Stepsx := @xsteps[0];
    for x := 0 to iwd - 1 do
    begin
      inc(ts, Stepsx^);
      td^ := ts^;
      inc(td);
      inc(Stepsx)
    end;
    inc(rowd, bd);
    inc(Stepsy);
  end;
end;

procedure MuxStreams2(const VideoFile, AudioFile: string;
  const OutputFile: string);
var
  ofmt: PAVOutputFormat;
  ifmt_ctx1, ifmt_ctx2, ofmt_ctx: PAVFormatContext;
  pkt: TAvPacket;
  in_filename1, in_filename2, out_filename: PAnsiChar;
  ret: integer;
  out_streamV, out_streamA: PAVStream;
  in_streamV, in_streamA: PAVStream;
  in_codecpar: PAVCodecParameters;
  Videotime, AudioTime: int64;
  p: PPAVStream;
  sn: cardinal;
  VideoStreamIndex, AudioStreamIndex: integer;
begin
  ifmt_ctx1 := nil;
  ifmt_ctx2 := nil;
  ofmt_ctx := nil;

  in_filename1 := MarshaledAString(UTF8String(VideoFile));
  in_filename2 := MarshaledAString(UTF8String(AudioFile));
  out_filename := MarshaledAString(UTF8String(OutputFile));

  ret := avformat_open_input(@ifmt_ctx1, in_filename1, nil, nil);
  Assert(ret = 0, format('Could not open input file ''%s''', [in_filename1]));

  ret := avformat_open_input(@ifmt_ctx2, in_filename2, nil, nil);
  Assert(ret = 0, format('Could not open input file ''%s''', [in_filename2]));

  ret := avformat_find_stream_info(ifmt_ctx1, nil);
  Assert(ret = 0, 'Failed to retrieve input stream information');

  ret := avformat_find_stream_info(ifmt_ctx2, nil);
  Assert(ret = 0, 'Failed to retrieve input stream information');

  avformat_alloc_output_context2(@ofmt_ctx, nil, nil, out_filename);
  Assert(assigned(ofmt_ctx), 'Could not create output context');

  ofmt := ofmt_ctx.oformat;

  // look for the 1st video stream
  p := ifmt_ctx1.streams;
  sn := 0;
  while (p^.codec.codec_type <> AVMEDIA_TYPE_VIDEO) and
    (sn < ifmt_ctx1.nb_streams) do
  begin
    inc(p);
    inc(sn);
  end;
  Assert((sn < ifmt_ctx1.nb_streams) and
    (p^.codec.codec_type = AVMEDIA_TYPE_VIDEO), 'No video stream found in ' +
    VideoFile);
  VideoStreamIndex := sn;
  in_streamV := p^;
  in_codecpar := in_streamV.codecpar;

  out_streamV := avformat_new_stream(ofmt_ctx, nil);
  Assert(assigned(out_streamV));

  ret := avcodec_parameters_copy(out_streamV.codecpar, in_codecpar);
  Assert(ret = 0, 'Failed to copy codec parameters');
  out_streamV.codecpar^.codec_tag.tag := 0;
  out_streamV.time_base.num := in_streamV.time_base.num;
  out_streamV.time_base.den := in_streamV.time_base.den;

  // Handle the audio stream from file 2
  // Locate the 1st audio stream
  p := ifmt_ctx2.streams;
  sn := 0;
  while (p^.codec.codec_type <> AVMEDIA_TYPE_AUDIO) and
    (sn < ifmt_ctx2.nb_streams) do
  begin
    inc(p);
    inc(sn);
  end;
  Assert((sn < ifmt_ctx2.nb_streams) and
    (p^.codec.codec_type = AVMEDIA_TYPE_AUDIO), 'No audio stream found in ' +
    AudioFile);
  AudioStreamIndex := sn;
  in_streamA := p^;
  in_codecpar := in_streamA.codecpar;

  out_streamA := avformat_new_stream(ofmt_ctx, nil);
  Assert(assigned(out_streamA));

  ret := avcodec_parameters_copy(out_streamA.codecpar, in_codecpar);
  Assert(ret = 0, 'Failed to copy codec parameters');
  out_streamA.codecpar^.codec_tag.tag := 0;

  if (ofmt.flags and AVFMT_NOFILE) = 0 then
  begin
    ret := avio_open(@ofmt_ctx.pb, out_filename, AVIO_FLAG_WRITE);
    Assert(ret = 0, format('Could not open output file ''%s''',
      [out_filename]));
  end;

  ret := avformat_write_header(ofmt_ctx, nil);
  Assert(ret = 0, 'Error occurred when opening output file');
  p := ofmt_ctx.streams;
  out_streamV := p^;
  inc(p, 1);
  out_streamA := p^;
  AudioTime := 0;
  while true do
  begin
    ret := av_read_frame(ifmt_ctx1, @pkt);
    if ret < 0 then
      break;

    if (pkt.stream_index <> VideoStreamIndex) then
    begin
      av_packet_unref(@pkt);
      Continue;
    end;

    pkt.stream_index := 0; // PPtrIdx(stream_mapping, pkt.stream_index);

    (* copy packet *)
    pkt.pts := av_rescale_q_rnd(pkt.pts, in_streamV.time_base,
      out_streamV.time_base, Ord(AV_ROUND_NEAR_INF) or
      Ord(AV_ROUND_PASS_MINMAX));
    pkt.dts := av_rescale_q_rnd(pkt.dts, in_streamV.time_base,
      out_streamV.time_base, Ord(AV_ROUND_NEAR_INF) or
      Ord(AV_ROUND_PASS_MINMAX));
    pkt.duration := av_rescale_q(pkt.duration, in_streamV.time_base,
      out_streamV.time_base);
    pkt.pos := -1;

    ret := av_interleaved_write_frame(ofmt_ctx, @pkt);
    if ret < 0 then
      break;
    Videotime := av_stream_get_end_pts(out_streamV);
    while (AudioTime < Videotime) and (ret >= 0) do
    begin
      ret := av_read_frame(ifmt_ctx2, @pkt);
      if ret < 0 then
        break;

      if (pkt.stream_index <> AudioStreamIndex) then
      begin
        av_packet_unref(@pkt);
        Continue;
      end;
      pkt.stream_index := 1;
      (* copy packet *)
      pkt.pts := av_rescale_q_rnd(pkt.pts, in_streamA.time_base,
        out_streamA.time_base, Ord(AV_ROUND_NEAR_INF) or
        Ord(AV_ROUND_PASS_MINMAX));
      pkt.dts := av_rescale_q_rnd(pkt.dts, in_streamA.time_base,
        out_streamA.time_base, Ord(AV_ROUND_NEAR_INF) or
        Ord(AV_ROUND_PASS_MINMAX));
      pkt.duration := av_rescale_q(pkt.duration, in_streamA.time_base,
        out_streamA.time_base);
      pkt.pos := -1;
      ret := av_interleaved_write_frame(ofmt_ctx, @pkt);
      // assert(ret >= 0);
      av_packet_unref(@pkt);
      AudioTime := av_rescale_q(av_stream_get_end_pts(out_streamA),
        out_streamA.time_base, out_streamV.time_base);
    end;
  end;

  av_write_trailer(ofmt_ctx);

  avformat_close_input(@ifmt_ctx1);
  avformat_close_input(@ifmt_ctx2);

  (* close output *)
  if assigned(ofmt_ctx) and ((ofmt.flags and AVFMT_NOFILE) = 0) then
    avio_closep(@ofmt_ctx.pb);
  avformat_free_context(ofmt_ctx);

  Assert((ret >= 0) or (ret = AVERROR_EOF));
end;

end.
