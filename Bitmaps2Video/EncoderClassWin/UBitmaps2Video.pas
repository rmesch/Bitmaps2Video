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
  Version 0.2
  Copyright 2020 Renate Schaaf
  ***************************************************************************** }
unit UBitmaps2Video;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  FFMpeg,
  VCL.Graphics,
  System.types,
  UTools,
  UFormats;

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

  TBitmapEncoder = class
  private
    fWidth, fHeight, fRate: integer;
    fQuality: byte;
    fFilename: string;
    fFrameCount: integer;
    fVideoTime: int64;
    fVideoFrameCount, fVideoFrameStart: integer;
    fVideoScaling: TVideoScaling;
    fCodecId: TAVCodecId;
    fInputVideoTimebase: TAVRational;
    fBackground: byte;
    VideoPts, VideoDts: int64;
    oc: PAVFormatContext;
    stream: PAVStream;
    codec: PAVCodec;
    c: PAVCodecContext;
    yuvpic: PAVFrame;
    CodecSetup: TBaseCodecSetup;
    fOnProgress: TVideoProgressEvent;
    function encode(frame: PAVFrame; FromVideo: boolean): boolean;
    procedure BitmapToFrame(const bm: TBitmap);
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
      VideoScaling: TVideoScaling = vsFastBilinear; Background: byte = 0);

    /// <summary> Use to resume work on a partially created video. All encoding, size and format settings will be read off the input file InFile.
    /// There are still some open problems with this routine.</summary>
    constructor CreateFromVideo(const InFilename, OutFilename: string;
      VideoScaling: TVideoScaling = vsFastBilinear; Background: byte = 0);

    /// <summary> Turn a Bitmap into a movie frame. If the aspect ratio does not match the video's one, black borders will be added. </summary>
    /// <param name="bm"> Bitmap(TBitmap) to be fed to the video. Will be converted to pf32bit if not already </param>
    procedure AddFrame(const bm: TBitmap);

    /// <summary> Add an existing video to the video stream. It will be resized and reencoded with the current settings.
    /// The format of the video can be anything that VLC-player plays. </summary>
    procedure AddVideo(const VideoInput: string);

    /// <summary> Hold the last frame </summary>
    /// <param name="EffectTime"> Displaytime(integer) in ms </param>
    procedure Freeze(EffectTime: integer);

    /// <summary> Add a picture which is displayed for a certain time </summary>
    /// <param name="bm"> Bitmap(TBitmap) of the picture to be displayed </param>
    /// <param name="ShowTime"> Time(integer) in ms for the display </param>
    procedure AddStillImage(const bm: TBitmap; ShowTime: integer);

    /// <summary> Make a smooth transition from SourceR to TargetR within EffectTime.</summary>
    /// <param name="bm"> The Bitmap(TBitmap) to be animated </param>
    /// <param name="SourceR"> Beginning rectangle(TRectF) within rect(0,0,bm.width,bm.height) </param>
    /// <param name="TargetR"> End rectangle(TRectF) within rect(0,0,bm.width,bm.height) </param>
    /// <param name="EffectTime"> Duration(integer) of the animation in ms </param>
    /// <param name="ZoomOption"> Quality of the zoom (zoAAx2, zoAAx4, zoAAx6, zoResample). </param>
    /// <param name="SpeedEnvelope"> Modifies the speed during EffectTime. (zeFastSlow, zeSlowFast, zeSlowSlow, zeLinear) </param>
    procedure ZoomPan(const bm: TBitmap; SourceR, TargetR: TRectF;
      EffectTime: integer; ZoomOption: TZoomOption;
      SpeedEnvelope: TZoomSpeedEnvelope = zeLinear);

    /// <summary> Close the file and make the output file usable. </summary>
    procedure CloseFile;

    destructor Destroy; override;

    /// <summary> how many frames have been added to the movie so far </summary>
    property FrameCount: integer read fFrameCount;

    /// <summary> Videotime so far in ms, more accurate if the frame rate varies slightly </summary>
    property Videotime: int64 read fVideoTime;

    property VideoWidth: integer read fWidth;

    property VideoHeight: integer read fHeight;

    property FrameRate: integer read fRate;
    /// <summary> Frame count of the last video added, use for grabbing thumbnails or timing </summary>
    property LastVideoFrameCount: integer read fVideoFrameCount;

    /// <summary> Event which fires every second of video time while writing. Use it to update a progressbar etc.. </summary>
    property OnProgress: TVideoProgressEvent read fOnProgress write fOnProgress;
    // Application.Processmessages can be called safely in this event (I hope).

    // Audio must be added by muxing, see below
    // Threads: Seems to be threadsafe when an instance is not used across threads
    // Needs to be tested further
    // Canvas procedures must be protected by Lock/Unlock.
  end;

  /// <summary> Combine the 1st video stream from VideoFile with 1st audio stream from Audiofile.  The streams will just be copied, not encoded.
  /// Audio is clipped to video length. Raises exception if the format of the audio file is not supported.</summary>
  /// <param name="VideoFile"> (string) File which contains a video stream. Any audio stream present will be ignored. </param>
  /// <param name="AudioFile"> (string) Genuine audio file (.wav .mp3 .aac) or any file containing an audio stream. This will be added to the video in VideoFile. </param>
  /// <param name="OutputFile"> (string) Name of the file containing the newly combined video and audio. </param>
procedure MuxStreams2(const VideoFile, AudioFile: string;
  const OutputFile: string);

/// <summary> Returns the expected video size for the given Codec, Width, Height, Frame-rate and Quality. </summary>
function VideoSizeInMB(Videotime: int64; CodecId: TAVCodecId;
  Width, Height, Rate: integer; Quality: byte): double;

implementation

uses VCL.Forms;

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

function TBitmapEncoder.encode(frame: PAVFrame; FromVideo: boolean): boolean;
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

procedure TBitmapEncoder.Freeze(EffectTime: integer);
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

procedure TBitmapEncoder.ZoomPan(const bm: TBitmap; SourceR, TargetR: TRectF;
  EffectTime: integer; ZoomOption: TZoomOption;
  SpeedEnvelope: TZoomSpeedEnvelope = zeLinear);
var
  am, cm: TBitmap;
  fact, asp: double;
  aw, ah: integer; // antialiased width height
  elapsed: integer;
  src, trg, mid: TRectF;
  targetTime: integer;
  frametime, t: double;
  evf: TEnvelopeFunction;
  bw, bh: integer;
begin
  evf := EnvelopeFunction[SpeedEnvelope];
  bw := bm.Width;
  bh := bm.Height;
  am := TBitmap.Create; // Antialias bitmap
  try
    am.PixelFormat := pf32bit;
    ah := bm.Height; // suppress compiler warning
    case ZoomOption of
      zoAAx2:
        ah := 2 * fHeight;
      zoAAx4:
        ah := 4 * fHeight;
      zoAAx6:
        ah := 6 * fHeight;
      // 6*screen.height might give an EOutOfResources if there is no
      // suffiently large memory block available, less likely under Win64
      zoResample:
        ah := fHeight + 100;
    end;
    asp := bw / bh;
    aw := round(ah * asp);
    am.SetSize(aw, ah);
    // upscale nicely
    ZoomResampleTripleOnly(bm, am, Rect(0, 0, bw, bh), 1.8);
    fact := aw / bw;
    src := ScaleRect(SourceR, fact);
    trg := ScaleRect(TargetR, fact); // scale rects to antialias-size
    frametime := 1000 / fRate;
    elapsed := 0;
    targetTime := round(EffectTime - 0.5 * frametime);
    cm := TBitmap.Create;
    try
      cm.PixelFormat := pf32bit;
      cm.SetSize(round(fHeight*bw/bh), fHeight);
      while elapsed < targetTime do
      begin
        t := elapsed / EffectTime;
        mid := Interpolate(src, trg, evf(t));
        case ZoomOption of
          zoAAx2, zoAAx4, zoAAx6:
            ZoomDeleteScansTripleOnly(am, cm, mid);
          zoResample:
            ZoomResampleTripleOnly(am, cm, mid, 1.5);
          // radius 1.5 is good enough
        end;
        // feed cm to the encoder
        AddFrame(cm);
        elapsed := round(elapsed + frametime);
      end;
    finally
      cm.Free;
    end;

  finally
    am.Free;
  end;
end;

procedure TBitmapEncoder.BitmapToFrame(const bm: TBitmap);
var
  convertCtx: PSwsContext;
  row: PByte;
  w, h, bps: integer;
  ret: integer;
  AspectDiffers: boolean;
  rgbFrame: PAVFrame;
const
  Epsilon = 0.1;
begin
  // Video ignores the alpha-channel. Size will be scaled if necessary,
  // proportionality will now always be preserved
  bm.PixelFormat := pf32bit;
  w := bm.Width;
  h := bm.Height;
  AspectDiffers := abs(w / h - fWidth / fHeight) > Epsilon;
  if AspectDiffers then
  begin
    rgbFrame := av_frame_alloc;
    try
      ExpandToAspectRatio(bm, rgbFrame, av_make_q(fWidth, fHeight),
        fBackground);
      convertCtx := sws_getContext(rgbFrame.Width, rgbFrame.Height,
        TAVPixelFormat(rgbFrame.format), fWidth, fHeight, c.pix_fmt,
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
    // Set up conversion to YUV
    convertCtx := sws_getContext(w, h, AV_PIX_FMT_BGRA, fWidth, fHeight,
      c.pix_fmt, ScaleFunction[fVideoScaling], nil, nil, nil);
    Assert(convertCtx <> nil);
    try
      // Convert the bitmap-data to yuv-frame
      bps := -(((w * 32 + 31) and not 31) div 8);
      ret := av_frame_make_writable(yuvpic);
      Assert(ret >= 0);
      row := bm.ScanLine[0];
      ret := sws_scale(convertCtx, @row, @bps, 0, h, @yuvpic.data,
        @yuvpic.linesize);
      Assert(ret >= 0);
    finally
      sws_freeContext(convertCtx);
      // needs to be freed and recreated for each frame
      // since the bm's could have different dimensions
    end;
  end;
end;

procedure TBitmapEncoder.AddFrame(const bm: TBitmap);
begin
  // Store the Bitmap in the yuv-frame
  BitmapToFrame(bm);

  // Encode the yuv-frame

  encode(yuvpic, False);

end;

procedure TBitmapEncoder.AddStillImage(const bm: TBitmap; ShowTime: integer);

begin
  BitmapToFrame(bm);

  // Encode the yuv-frame repeatedly
  Freeze(ShowTime);
end;

procedure TBitmapEncoder.AddVideo(const VideoInput: string);
var
  fmt_ctx: PAVFormatContext;
  video_dec_ctx: PAVCodecContext;
  Width, Height: integer;
  pix_fmt: TAVPixelFormat;
  video_stream_idx: integer;
  frame: PAVFrame;
  pkt: TAvPacket;
  ret, i: integer;
  got_frame: integer;
  video_dst_data: array [0 .. 3] of PByte;
  video_dst_linesize: array [0 .. 3] of integer;
  video_stream: PAVStream;
  convertCtx: PSwsContext;
  rgbpic: PAVFrame;
  bm: TBitmap;
  p: PPAVStream;
  sar: TAVRational;
  TrueHeight: integer;
  AspectDiffers: boolean;
  args: array [0 .. 512 - 1] of AnsiChar;
  buffersink_ctx: PAVFilterContext;
  buffersrc_ctx: PAVFilterContext;
  filter_graph: PAVFilterGraph;
  f: PAVFrame;
  inputs, outputs: PAVFilterInOut;
  FilterInitialized: boolean;
  QuitLoop: boolean;
const
  Epsilon = 0.1;

  procedure InitFiltergraph(aFrame: PAVFrame);
  begin
    If FilterInitialized then
      exit;

    // Configure the de-interlace filter (yadif)
    snprintf(@args[0], sizeof(args),
      'buffer=video_size=%dx%d:pix_fmt=%d:time_base=1/1:pixel_aspect=0/1[in];[in]yadif[out];[out]buffersink',
      video_dec_ctx.Width, video_dec_ctx.Height, video_dec_ctx.pix_fmt);
    filter_graph := avfilter_graph_alloc();
    inputs := nil;
    outputs := nil;
    ret := avfilter_graph_parse2(filter_graph, PAnsiChar(@args[0]), @inputs,
      @outputs);
    Assert(ret >= 0);
    ret := avfilter_graph_config(filter_graph, nil);
    Assert(ret >= 0);
    buffersrc_ctx := avfilter_graph_get_filter(filter_graph, 'Parsed_buffer_0');
    buffersink_ctx := avfilter_graph_get_filter(filter_graph,
      'Parsed_buffersink_2');
    Assert(buffersrc_ctx <> nil);
    Assert(buffersink_ctx <> nil);

    FilterInitialized := true;
  end;

  function AddVideoFrame(aFrame: PAVFrame): boolean;
  var
    row: PByte;
    bps: integer;
  begin
    if AspectDiffers then
    begin
      bm := TBitmap.Create;
      try
        bm.PixelFormat := pf32bit;
        bm.SetSize(Width, TrueHeight);
        row := bm.ScanLine[0];
        bps := -(((Width * 32 + 31) and not 31) div 8);
        ret := sws_scale(convertCtx, @aFrame.data, @aFrame.linesize, 0, Height,
          @row, @bps);
        Assert(ret >= 0);
        // This takes care of different aspect ratio:
        BitmapToFrame(bm);
      finally
        bm.Free;
      end;
    end
    else
    begin
      // if aspect fits, scale the frame and change the pixel format, if necessary
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
  FilterInitialized := False;
  filter_graph := nil;
  inputs := nil;
  outputs := nil;
  fmt_ctx := nil;
  video_dec_ctx := nil;
  frame := nil;
  rgbpic := nil;
  fVideoFrameCount := 0;
  for i := 0 to 3 do
    video_dst_data[i] := nil;
  (* open input file, and allocate format context *)
  ret := avformat_open_input(@fmt_ctx, PAnsiChar(AnsiString(VideoInput)),
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

  AspectDiffers := abs(fWidth / fHeight - Width / TrueHeight) > Epsilon;
  // If the aspect differs we convert to bitmap and copyrect to bitmap with grayscale borders.
  if AspectDiffers then
  begin
    convertCtx := sws_getContext(Width, Height, pix_fmt, Width, TrueHeight,
      AV_PIX_FMT_BGRA, ScaleFunction[fVideoScaling], nil, nil, nil);
  end
  else
    convertCtx := sws_getContext(Width, Height, pix_fmt, fWidth, fHeight,
      c.pix_fmt, ScaleFunction[fVideoScaling], nil, nil, nil);

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
        if frame.interlaced_frame = 0 then
          QuitLoop := QuitLoop or (not AddVideoFrame(frame))
        else
        begin
          if not FilterInitialized then
            InitFiltergraph(frame);
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
      end; // if got_frame
    end; // while true
    Assert(not QuitLoop, 'Some video frames could not be added');
  finally
    if assigned(rgbpic) then
      av_frame_free(@rgbpic);
    if assigned(inputs) then
      avfilter_inout_free(@inputs);
    if assigned(outputs) then
      avfilter_inout_free(@outputs);
    if assigned(filter_graph) then
      avfilter_graph_free(@filter_graph);
    av_freep(@video_dst_data[0]);
    av_frame_free(@frame);
    avcodec_free_context(@video_dec_ctx);
    sws_freeContext(convertCtx);
    avformat_close_input(@fmt_ctx);
  end;
end;

procedure TBitmapEncoder.CloseFile;
var
  ret: integer;
begin
  // flush the encoder
  encode(nil, False);

  ret := av_write_trailer(oc); // Writing the end of the file.
  Assert(ret >= 0);
  if ((oc.oformat.flags and AVFMT_NOFILE) = 0) then
  begin
    ret := avio_closep(@oc.pb); // Closing the file.
    Assert(ret >= 0);
  end;
  ret := avcodec_close(stream.codec);
  Assert(ret >= 0);
end;

constructor TBitmapEncoder.Create(const filename: string;
  Width, Height, FrameRate: integer; Quality: byte;
  CodecId: TAVCodecId = AV_CODEC_ID_NONE;
  VideoScaling: TVideoScaling = vsFastBilinear; Background: Byte =0);
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
  fBackground := Background;
  if CodecId = AV_CODEC_ID_NONE then
    fCodecId := PreferredCodec(ExtractFileExt(fFilename))
  else
    fCodecId := CodecId;

  CodecSetup := CodecSetupClass(fCodecId).Create(fCodecId);

  oc := nil;
  ret := avformat_alloc_output_context2(@oc, nil, nil,
    PAnsiChar(AnsiString(filename)));
  Assert(ret >= 0, 'avformat_alloc.. error' + Inttostr(ret));
  stream := avformat_new_stream(oc, nil);
  Assert(stream <> nil, 'avformat_new_stream failed');
  codec := CodecSetup.codec;
  Assert(codec <> nil, 'codec not found');

  c := avcodec_alloc_context3(codec);
  c.Width := fWidth;
  c.Height := fHeight;
  // We encode square pixels:
  c.sample_aspect_ratio.num := 1;
  c.sample_aspect_ratio.den := 1;

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

  ret := avio_open(@oc.pb, PAnsiChar(AnsiString(filename)), AVIO_FLAG_WRITE);
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
end;

constructor TBitmapEncoder.CreateFromVideo(const InFilename, OutFilename: string;
      VideoScaling: TVideoScaling = vsFastBilinear; Background: byte = 0);
var
  ret: integer;
  ifmt_ctx: PAVFormatContext;
  in_filename, out_filename: PAnsiChar;
  in_stream: PAVStream;
  in_codecpar: PAVCodecParameters;
  pkt, lastVideoPkt: TAvPacket;
  p: PPAVStream;
  numstreams: integer;
  sn: integer;
begin
  fVideoScaling := VideoScaling;
  fBackground := Background;
  // The extension must match InputFile
  fFilename := ExtractFilePath(OutFilename) + ExtractFileName(OutFilename) +
    ExtractFileExt(InFilename);
  Assert(UpperCase(fFilename) <> UpperCase(InFilename),
    'Output file name must be different from input file name');
  ifmt_ctx := nil;
  oc := nil;
  // Read the input video
  in_filename := PAnsiChar(AnsiString(InFilename));
  ret := avformat_open_input(@ifmt_ctx, in_filename, nil, nil);
  Assert(ret = 0, format('Could not open input file ''%s''', [in_filename]));
  // Read Codec Info
  ret := avformat_find_stream_info(ifmt_ctx, nil);
  Assert(ret = 0, 'Failed to retrieve input stream information');
  numstreams := ifmt_ctx.nb_streams;
  // Create Output file with oc = global output context

  out_filename := PAnsiChar(AnsiString(fFilename));

  avformat_alloc_output_context2(@oc, nil, nil, out_filename);
  Assert(assigned(oc), 'Could not create output context');

  // Copy codec parameters from InFile to OutFile
  // First find the first video-stream
  p := ifmt_ctx.streams;
  sn := 0;
  while (p^.codec.codec_type <> AVMEDIA_TYPE_VIDEO) and (sn < numstreams) do
  begin
    inc(p);
    inc(sn);
  end;
  Assert(sn < numstreams, 'No video stream found in ' + InFilename);
  in_stream := p^;
  in_codecpar := in_stream.codecpar;

  stream := avformat_new_stream(oc, nil);
  Assert(assigned(stream));

  ret := avcodec_parameters_copy(stream.codecpar, in_codecpar);
  Assert(ret = 0, 'Failed to copy codec parameters');
  stream.codecpar^.codec_tag.tag := 0;
  stream.time_base.num := in_stream.time_base.num;
  stream.time_base.den := in_stream.time_base.den;
  // Read properties from InFile
  fWidth := in_codecpar.Width;
  fHeight := in_codecpar.Height;
  fCodecId := in_codecpar.codec_id;
  if CodecSetupClass(fCodecId) <> nil then
    CodecSetup := CodecSetupClass(fCodecId).Create(fCodecId)
  else
    Raise Exception.Create('Input video codec is not supported');
  stream.codec.flags := in_stream.codec.flags;
  if (oc.oformat.flags and AVFMT_GLOBALHEADER) <> 0 then
    stream.codec.flags := stream.codec.flags or AV_CODEC_FLAG_GLOBAL_HEADER;
  if in_stream.avg_frame_rate.den > 0 then
    fRate := round(in_stream.avg_frame_rate.num / in_stream.avg_frame_rate.den)
  else
    fRate := in_stream.r_frame_rate.num;

  // Copy frames from input to output, keep track of framecount
  if (oc.oformat.flags and AVFMT_NOFILE) = 0 then
  begin
    ret := avio_open(@oc.pb, out_filename, AVIO_FLAG_WRITE);
    Assert(ret = 0, format('Could not open output file ''%s''',
      [out_filename]));
  end
  else
    Raise Exception.Create(format('Could not open output file ''%s''',
      [out_filename]));
  ret := avformat_write_header(oc, nil);
  Assert(ret = 0, 'Error occurred when opening output file');
  fFrameCount := 0;
  while true do
  begin
    av_init_packet(@pkt);
    pkt.size := 0;
    pkt.data := nil;
    ret := av_read_frame(ifmt_ctx, @pkt);
    if ret < 0 then
      break;
    // pkt not in video stream (number sn)
    if (pkt.stream_index <> sn) then
    begin
      av_packet_unref(@pkt);
      Continue;
    end;

    pkt.stream_index := 0;

    (* copy packet *)

    // should not be necessary, the time bases are identical
    // rather keep it, avio_open can change the time base
    av_packet_rescale_ts(@pkt, in_stream.time_base, stream.time_base);
    lastVideoPkt := pkt;
    pkt.pos := -1;
    av_write_frame(oc, @pkt);
    // av_interleaved_write_frame(oc, @pkt);
    // inc(fFrameCount);
    av_packet_unref(@pkt);
  end;
  // Create encoding context and fill with codec info using c=global encoding context
  codec := CodecSetup.codec;
  Assert(codec <> nil);
  c := avcodec_alloc_context3(codec);
  CodecSetup.CodecContextProps(c); // needed for the h264-Dictionary
  c.bit_rate := in_codecpar.bit_rate;
  c.codec_id := fCodecId;
  c.codec_type := AVMEDIA_TYPE_VIDEO;

  // c needs to have the real frame rate as time base
  // otherwise the encoding of a single frame fails
  // with error in av_receive_packet
  c.time_base.num := 1;
  c.time_base.den := fRate;

  c.FrameRate.num := fRate;
  c.FrameRate.den := 1;
  c.Width := fWidth;
  c.Height := fHeight;
  c.pix_fmt := in_stream.codec.pix_fmt;
  c.flags := in_stream.codec.flags;
  av_packet_rescale_ts(@lastVideoPkt, stream.time_base, c.time_base);
  fFrameCount := av_rescale_q(av_stream_get_end_pts(stream), stream.time_base,
    c.time_base);
  avformat_close_input(@ifmt_ctx);

  // if we set the global_header flag here, writing fails.
  {
    if (oc.oformat.flags and AVFMT_GLOBALHEADER) <> 0 then
    c.flags := c.flags or AV_CODEC_FLAG_GLOBAL_HEADER;
  }

  // make output file ready to receive new encoded frames
  ret := avcodec_open2(c, codec, @CodecSetup.OptionsDictionary);
  Assert(ret >= 0);

  // Allocating memory for conversion output YUV frame:
  yuvpic := av_frame_alloc();
  Assert(yuvpic <> nil);
  yuvpic.format := Ord(c.pix_fmt);
  yuvpic.Width := fWidth;
  yuvpic.Height := fHeight;
  ret := av_frame_get_buffer(yuvpic, 0);
  Assert(ret >= 0);
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

  in_filename1 := PAnsiChar(AnsiString(VideoFile));
  in_filename2 := PAnsiChar(AnsiString(AudioFile));
  out_filename := PAnsiChar(AnsiString(OutputFile));

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

destructor TBitmapEncoder.Destroy;
begin
  CodecSetup.Free;
  av_frame_free(@yuvpic);
  avformat_free_context(oc);
  avcodec_free_context(@c);
  inherited;
end;

end.
