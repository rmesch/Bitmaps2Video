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
  System.SyncObjs;

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
      FrameRate: integer; Quality: Byte; CodecId: TAVCodecId = AV_CODEC_ID_NONE;
      VideoScaling: TVideoScaling = vsFastBilinear; BackGround: Byte = 0);

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

implementation

uses System.UITypes, UToolsM;

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
  inc(fFrameCount);
  if fFrameCount mod fRate = 0 then // update once per second
  begin
    if assigned(fOnProgress) then
      fOnProgress(fVideoTime);
  end;
  av_init_packet(@pkt);
  pkt.data := nil;
  pkt.size := 0;
  if frame <> nil then
  begin
    frame.pts := fFrameCount;
  end;
  ret := avcodec_send_frame(c, frame);
  while ret >= 0 do
  begin
    ret := avcodec_receive_packet(c, @pkt);
    // Could there be a bug in ffmpeg.pas mixing up AVERROR_EAGAIN and AVERROR_EDEADLK for Android?
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) or (ret = AVERROR_EDEADLK)
    then
    begin
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
    end;

    av_packet_rescale_ts(@pkt, c.time_base, stream.time_base);
    pkt.stream_index := stream.index;

    // Write the encoded frame to the video file.
    // Can fail without causing harm, but should be checked in the debugger if frames are missing
    ret := av_interleaved_write_frame(oc, @pkt);
    result := (ret >= 0);
    av_packet_unref(@pkt);
    fVideoTime := round(1000 * av_rescale_q(av_stream_get_end_pts(stream),
      stream.time_base, c.time_base) / fRate);
  end;
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

// Now with fast pixel access thanks to HuichanKIM
procedure TBitmapEncoderM.BitmapToFrame(const bm: TBitmap);
var
  convertCtx: PSwsContext;
  w, h: integer;
  stride: integer; // Add new var
  ret: integer;
  BitData: TBitmapData;
  pix_fmt: TAVPixelFormat;
  AspectDiffers: boolean;
  rgbFrame: PAVFrame;
const
  Epsilon = 0.1;
begin
  Assert((bm.Width > 0) and (bm.Height > 0));
  // Under Android and Windows the pixel formats differ
{$IFDEF ANDROID}
  pix_fmt := AV_PIX_FMT_RGBA;
{$ELSE}
  pix_fmt := AV_PIX_FMT_BGRA;
{$ENDIF}
  AspectDiffers := Abs(bm.Width / bm.Height - fWidth / fHeight) > Epsilon;
  if AspectDiffers then
  begin
    rgbFrame := av_frame_alloc;
    try
      ExpandToAspectRatio(bm, rgbFrame, av_make_q(fWidth, fHeight),
        fBackground);
      convertCtx := sws_getContext(rgbFrame.Width, rgbFrame.Height, pix_fmt,
        fWidth, fHeight, CodecSetup.OutputPixelFormat,
        ScaleFunction[fVideoScaling], nil, nil, nil);
      Assert(convertCtx <> nil);
      try
        ret := av_frame_make_writable(yuvpic);
        Assert(ret >= 0);
        ret := sws_scale(convertCtx, @rgbFrame.data, @rgbFrame.linesize, 0,
          rgbFrame.Height, @yuvpic.data, @yuvpic.linesize);
        Assert(ret >= 0);
      finally
        sws_freeContext(convertCtx);
      end;
    finally
      av_frame_free(@rgbFrame);
    end;
  end
  else
  begin
    Assert(bm.Map(TMapAccess.ReadWrite, BitData));
    w := bm.Width;
    h := bm.Height;
    // Pitch is more reliable than BytesPerLine
    stride := BitData.Pitch;

    convertCtx := sws_getContext(w, h, pix_fmt,
      // replace of AV_PIX_FMT_BGR24
      fWidth, fHeight, CodecSetup.OutputPixelFormat,
      ScaleFunction[fVideoScaling], nil, nil, nil);
    Assert(convertCtx <> nil);

    try
      ret := av_frame_make_writable(yuvpic);
      Assert(ret >= 0);
      ret := sws_scale(convertCtx, @BitData.data, @stride, 0, h, @yuvpic.data,
        @yuvpic.linesize);
      Assert(ret >= 0);
    finally
      sws_freeContext(convertCtx);
      bm.Unmap(BitData);
    end;
  end;

end;

procedure TBitmapEncoderM.AddColorFrame(Color: TBGR);
begin
  ColorToFrame(Color);
  encode(yuvpic, False);
end;

procedure TBitmapEncoderM.AddFrame(const bm: TBitmap);
begin
  // Store the Bitmap in the yuv-frame
  BitmapToFrame(bm);


  // Encode the yuv-frame

  encode(yuvpic, False);

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
  Assert(encode(nil, False));

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
    rgbpic.Format := Ord(AV_PIX_FMT_BGR24);
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
  VideoScaling: TVideoScaling = vsFastBilinear; BackGround: Byte = 0);
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
  fBackground := BackGround;
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
  yuvpic.Format := Ord(CodecSetup.OutputPixelFormat);
  yuvpic.Width := fWidth;
  yuvpic.Height := fHeight;
  ret := av_frame_get_buffer(yuvpic, 0);
  Assert(ret >= 0);

  // Allocating memory for packet
  pkt := av_packet_alloc();
  Assert(pkt <> nil);

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
  Epsilon = 0.1;

  function AddVideoFrame(aFrame: PAVFrame): boolean;
  var
    bm: TBitmap;
    BitmapData: TBitmapData;
    row:PByte;
    bps:Integer;
  begin
    if AspectDiffers then
    begin
      bm := TBitmap.Create;
      try
        bm.SetSize(aFrame.Width, TrueHeight);
        bm.Map(TMapAccess.Write, BitmapData);
        try
          row:=BitmapData.Data;
          bps:=BitmapData.Pitch;
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
    //We convert the frames to bitmaps and then encode the bitmaps
    convertCtx := sws_getContext(Width, Height, pix_fmt, Width, TrueHeight,
      rgb_fmt, ScaleFunction[fVideoScaling], nil, nil, nil);
  end
  else
  begin
    //If the aspect is OK just resize and change the pix-format
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
    QuitLoop := False;
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
      inc(fVideoFrameCount);
      (* decode video frame *)
      // !avcodec_decode_video2 is deprecated, but I couldn't get
      // the replacement avcode_send_packet and avcodec_receive_frame to work
      got_frame := 0;
      ret := avcodec_decode_video2(video_dec_ctx, frame, @got_frame, @pkt);
      Assert(ret >= 0);

      if (got_frame <> 0) then
      begin
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

end.
