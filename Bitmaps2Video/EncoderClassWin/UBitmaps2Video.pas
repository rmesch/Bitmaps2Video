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
*****************************************************************************}
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
    fVideoFrameCount: integer;
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
      VideoScaling: TVideoScaling = vsFastBilinear);

    /// <summary> Turn a Bitmap into a movie frame </summary>
    /// <param name="bm"> Bitmap(TBitmap) to be fed to the video. Will be converted to pf32bit if not already </param>
    procedure AddFrame(const bm: TBitmap);

    /// <summary> Add an existing video to the video stream. Right now only works, if the frame rates are the same. </summary>
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
    function CloseFile: boolean;

    destructor Destroy; override;

    /// <summary> how many frames have been added to the movie so far </summary>
    property FrameCount: integer read fFrameCount;

    /// <summary> Event which fires every second of video time while writing. Use it to update a progressbar etc.. </summary>
    property OnProgress: TVideoProgressEvent read fOnProgress write fOnProgress;
    // Application.Processmessages can be called safely in this event (I hope).

    // Audio must be added by muxing, see below
    // Threads: Seems to be threadsafe when an instance is not used across threads
    // Canvas procedures must be protected by Lock/Unlock.
  end;

  /// <summary> Combine the video stream from VideoFile with the audio from Audiofile. The streams will just be copied, not encoded.
  /// Audio is clipped to video length. Raises exception if the format of the audio file is not supported.</summary>
  /// <param name="VideoFile"> (string) File which contains a video stream. Any audio stream present will be ignored. </param>
  /// <param name="AudioFile"> (string) Genuine audio file (.wav .mp3 .aac) the audio of which shall be added to the video in VideoFile. </param>
  /// <param name="OutputFile"> (string) Name of the file containing the newly combined video and audio. </param>
procedure MuxStreams2(const VideoFile, AudioFile: string;
  const OutputFile: string);

///<summary> Returns the expected video size for the given Codec, Width, Height, Frame-rate and Quality. </summary>
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
begin
  evf := EnvelopeFunction[SpeedEnvelope];
  am := TBitmap.Create; // Antialias bitmap
  try
    am.PixelFormat := pf32bit;
    ah := bm.Height; // suppress compiler warning
    case ZoomOption of
      zoAAx2:
        ah := 2 * 1080; // 2* my screen.height  better setting?
      zoAAx4:
        ah := 4 * 1080;
      zoAAx6:
        ah := 6 * 1080;
      // 6*screen.height might give an EOutOfResources depending on hardware
      zoResample:
        ah := bm.Height + 100;
    end;
    asp := bm.Width / bm.Height;
    aw := round(ah * asp);
    am.SetSize(aw, ah);
    ZoomResampleTripleOnly(bm, am, Rect(0, 0, bm.Width, bm.Height), 1.8);
    // upscale nicely
    fact := aw / bm.Width;
    src := ScaleRect(SourceR, fact);
    trg := ScaleRect(TargetR, fact); // scale rects to antialias-size
    frametime := 1000 / fRate;
    elapsed := 0;
    targetTime := round(EffectTime - 0.5 * frametime);
    // Effecttime-half of frame time
    cm := TBitmap.Create;
    try
      cm.PixelFormat := pf32bit;
      cm.SetSize(fWidth, fHeight);
      cm.PixelFormat := pf32bit;
      while elapsed < targetTime do
      begin
        t := elapsed / EffectTime;
        mid := Interpolate(src, trg, evf(t));
        case ZoomOption of
          zoAAx2, zoAAx4, zoAAx6:
            ZoomDeleteScansTripleOnly(am, cm, mid);
          zoResample:
            ZoomResampleTripleOnly(am, cm, mid, 1.6);
          // radius 1.6 is good enough
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
  rgbpic: PAVFrame;
  convertCtx: PSwsContext;
  x, y: integer;
  ps: PRGBQuad;
  row: PByte;
  w, h, bps: integer;
  px, py: PByte;
  jump: integer;
  ret: integer;
begin
  bm.PixelFormat := pf32bit;
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
      ps := PRGBQuad(row);
      px := py;
      for x := 0 to w - 1 do
      begin
        PRGBTriple(px)^ := PRGBTriple(ps)^;
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
  pkt: TAVPacket;
  ret: integer;
  got_frame: integer;
  video_dst_data: array [0 .. 3] of PByte;
  video_dst_linesize: array [0 .. 3] of integer;
  video_stream: PAVStream;
  convertCtx: PSwsContext;
  rgbpic: PAVFrame;
  x, y: integer;
  ps: PRGBQuad;
  row: PByte;
  px, py: PByte;
  jump: integer;
  bps: integer;
  bm: TBitmap;
  p:PPAVStream;
begin
  fmt_ctx := nil;
  video_dec_ctx := nil;
  frame := nil;
  for x := 0 to 3 do
    video_dst_data[x] := nil;
  (* open input file, and allocate format context *)
  ret := avformat_open_input(@fmt_ctx, PAnsiChar(AnsiString(VideoInput)),
    nil, nil);
  assert(ret >= 0);

  (* retrieve stream information *)
  ret := avformat_find_stream_info(fmt_ctx, nil);
  assert(ret >= 0);

  open_decoder_context(@video_stream_idx, @video_dec_ctx, fmt_ctx,
    AVMEDIA_TYPE_VIDEO);
  p:=fmt_ctx.streams;
  inc(p,video_stream_idx);
  video_stream := p^;
  fInputVideoTimebase := video_stream.time_base;

  (* allocate image where the decoded image will be put *)
  Width := video_dec_ctx.Width;
  Height := video_dec_ctx.Height;
  pix_fmt := video_dec_ctx.pix_fmt;
  ret := av_image_alloc(@video_dst_data[0], @video_dst_linesize[0], Width,
    Height, pix_fmt, 1);
  assert(ret >= 0);
  // Conversion Context to BGR
  convertCtx := sws_getContext(Width, Height, pix_fmt, Width, Height,
    AV_PIX_FMT_BGR24, SWS_FAST_BILINEAR, nil, nil, nil);
  // Allocate storage for the rgb-frame
  rgbpic := av_frame_alloc();
  assert(rgbpic <> nil);
  frame := av_frame_alloc();
  assert(frame <> nil);
  try

    rgbpic.Format := Ord(AV_PIX_FMT_BGR24);
    rgbpic.Width := Width;
    rgbpic.Height := Height;
    av_frame_get_buffer(rgbpic, 0);

    (* initialize packet, set data to NULL, let the demuxer fill it *)
    av_init_packet(@pkt);
    pkt.data := nil;
    pkt.size := 0;
    (* read frames from the file *)
    fVideoFrameCount := 0;
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
      assert(pkt.stream_index = video_stream_idx);
      (* decode video frame *)
      got_frame := 0;
      ret := avcodec_decode_video2(video_dec_ctx, frame, @got_frame, @pkt);
      assert(ret >= 0);

      // Convert the yuv-frame to rgb-frame
      ret := av_frame_make_writable(rgbpic);
      assert(ret >= 0);
      ret := sws_scale(convertCtx, @frame.data, @frame.linesize, 0, Height,
        @rgbpic.data, @rgbpic.linesize);
      assert(ret >= 0);
      // Store the frame in a bmp
      bm := TBitmap.Create;
      try
        bm.PixelFormat := pf32bit;
        bm.SetSize(Width, Height);
        row := bm.ScanLine[0];
        bps := ((Width * 32 + 31) and not 31) div 8;
        py := @PByte(rgbpic.data[0])[0];
        // it's faster with pointers instead of array
        jump := rgbpic.linesize[0];
        for y := 0 to Height - 1 do
        begin
          ps := PRGBQuad(row);
          px := py;
          for x := 0 to Width - 1 do
          begin
            PRGBTriple(ps)^ := PRGBTriple(px)^;
            // works with BGR24 format
            inc(px, 3);
            inc(ps);
          end;
          dec(row, bps);
          inc(py, jump);
        end;
        BitmapToFrame(bm);
        inc(fVideoFrameCount);
        encode(yuvpic, true);
        av_packet_unref(@pkt);
      finally
        bm.Free;
      end;
    end; // while ret>=0
  finally
    av_frame_free(@rgbpic);
    av_frame_free(@frame);
    avcodec_free_context(@video_dec_ctx);
    sws_freeContext(convertCtx);
    avformat_close_input(@fmt_ctx);
  end;
end;

function TBitmapEncoder.CloseFile: boolean;
begin
  // flush the encoder
  assert(encode(nil, False));

  av_write_trailer(oc); // Writing the end of the file.
  if ((fmt.flags and AVFMT_NOFILE) = 0) then
    avio_closep(@oc.pb); // Closing the file.
  result := (avcodec_close(stream.codec) >= 0);
end;

constructor TBitmapEncoder.Create(const filename: string;
  Width, Height, FrameRate: integer; Quality: byte;
  CodecId: TAVCodecId = AV_CODEC_ID_NONE;
  VideoScaling: TVideoScaling = vsFastBilinear);
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
  if CodecId = AV_CODEC_ID_NONE then
    fCodecId := PreferredCodec(ExtractFileExt(fFilename))
  else
    fCodecId := CodecId;

  CodecSetup := CodecSetupClass(fCodecId).Create(fCodecId);

  fmt := GetOutputFormat(ExtractFileExt(fFilename));
  assert(fmt <> nil, 'No matching format');
  oc := nil;
  ret := avformat_alloc_output_context2(@oc, nil, nil,
    PAnsiChar(AnsiString(filename)));
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

  ret := avio_open(@oc.pb, PAnsiChar(AnsiString(filename)), AVIO_FLAG_WRITE);
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



procedure MuxStreams2(const VideoFile, AudioFile: string;
  const OutputFile: string);
var
  ofmt: PAVOutputFormat;
  ifmt_ctx1, ifmt_ctx2, ofmt_ctx: PAVFormatContext;
  pkt: TAVPacket;
  in_filename1, in_filename2, out_filename: PAnsiChar;
  ret: integer;
  out_streamV, out_streamA: PAVStream;
  in_streamV, in_streamA: PAVStream;
  in_codecpar: PAVCodecParameters;
  Videotime, AudioTime: int64;
  p:PPAVStream;

begin
  ifmt_ctx1 := nil;
  ifmt_ctx2 := nil;
  ofmt_ctx := nil;

  in_filename1 := PAnsiChar(AnsiString(VideoFile));
  in_filename2 := PAnsiChar(AnsiString(AudioFile));
  out_filename := PAnsiChar(AnsiString(OutputFile));

  ret := avformat_open_input(@ifmt_ctx1, in_filename1, nil, nil);
  assert(ret = 0, Format('Could not open input file ''%s''', [in_filename1]));

  ret := avformat_open_input(@ifmt_ctx2, in_filename2, nil, nil);
  assert(ret = 0, Format('Could not open input file ''%s''', [in_filename2]));

  ret := avformat_find_stream_info(ifmt_ctx1, nil);
  assert(ret = 0, 'Failed to retrieve input stream information');

  ret := avformat_find_stream_info(ifmt_ctx2, nil);
  assert(ret = 0, 'Failed to retrieve input stream information');

  avformat_alloc_output_context2(@ofmt_ctx, nil, nil, out_filename);
  assert(assigned(ofmt_ctx), 'Could not create output context');

  ofmt := ofmt_ctx.oformat;
  in_streamV := ifmt_ctx1.streams^;
  in_codecpar := in_streamV.codecpar;

  out_streamV := avformat_new_stream(ofmt_ctx, nil);
  assert(assigned(out_streamV));

  ret := avcodec_parameters_copy(out_streamV.codecpar, in_codecpar);
  assert(ret = 0, 'Failed to copy codec parameters');
  out_streamV.codecpar^.codec_tag.tag := 0;
  out_streamV.time_base.num := in_streamV.time_base.num;
  out_streamV.time_base.den := in_streamV.time_base.den;

  // Handle the audio stream from file 2
  in_streamA := ifmt_ctx2.streams^;
  in_codecpar := in_streamA.codecpar;

  out_streamA := avformat_new_stream(ofmt_ctx, nil);
  assert(assigned(out_streamA));

  ret := avcodec_parameters_copy(out_streamA.codecpar, in_codecpar);
  assert(ret = 0, 'Failed to copy codec parameters');
  out_streamA.codecpar^.codec_tag.tag := 0;

  if (ofmt.flags and AVFMT_NOFILE) = 0 then
  begin
    ret := avio_open(@ofmt_ctx.pb, out_filename, AVIO_FLAG_WRITE);
    assert(ret = 0, Format('Could not open output file ''%s''',
      [out_filename]));
  end;

  ret := avformat_write_header(ofmt_ctx, nil);
  assert(ret = 0, 'Error occurred when opening output file');
  p:=ofmt_ctx.streams;
  out_streamV := p^;
  inc(p,1);
  out_streamA := p^;
  AudioTime := 0;
  while true do
  begin
    ret := av_read_frame(ifmt_ctx1, @pkt);
    if ret < 0 then
      break;

    if (pkt.stream_index <> 0) then
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
    assert(ret >= 0);
    Videotime := av_stream_get_end_pts(out_streamV);
    while AudioTime < Videotime do
    begin
      ret := av_read_frame(ifmt_ctx2, @pkt);
      if ret < 0 then
        break;

      if (pkt.stream_index <> 0) then
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
      assert(ret >= 0);
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

  assert((ret >= 0) or (ret = AVERROR_EOF));
end;

destructor TBitmapEncoder.Destroy;
begin
  CodecSetup.Free;
  av_frame_free(@yuvpic);
  avformat_free_context(oc);
  avcodec_free_context(@c);
  av_packet_free(@pkt);
  inherited;
end;

end.
