unit UToolsM;

interface

uses
  FFMPEG,
  FMX.Graphics,
  System.types,
  UFormatsM,
  math;

type
  TVideoProps = record
    Width, Height, TrueHeight: integer;
    FrameRate: integer;
    nrVideostreams, nrAudiostreams: integer;
    VideoCodec, AudioCodec: TAVCodecID;
    Duration: int64;
  end;

  TEnvelopeFunction = function(t: double): double;
  TZoomSpeedEnvelope = (zeSlowSlow, zeFastSlow, zeSlowFast, zeLinear,
    zeExperiment);

  /// <summary> (lScale,tScale): LeftTop as fractions of (Width,Height). sScale: Size as Fraction of Width/Height </summary>
  TZoom = record
    lScale, tScale, sScale: double;
    function ToRect(Width, Height: integer): TRectF; inline;
  end;

function RectToZoom(r: TRectF; Width, Height: integer): TZoom; inline;
function MakeZoom(l, t, s: double): TZoom; inline;

procedure open_decoder_context(stream_idx: PInteger; dec_ctx: PPAVCodecContext;
  fmt_ctx: PAVFormatContext; type_: TAVMediaType);

/// <summary> Get video properties. TrueHeight is the height when not square pixels sizes are made square (sar) </summary>
function GetVideoProps(const Videofile: string): TVideoProps;

/// <summary> Store frame number FrameNumber of Videofile in the bitmap bm </summary>
/// fails under Windows for Videofiles encoded with DVvideo
procedure GrabFrame(const bm: TBitmap; const Videofile: string;
  FrameNumber: integer);

// TRectF utility functions

/// <summary> Compute an in between rect of SourceR and TargetR </summary>
/// <param name="SourceR"> TRectF </param>
/// <param name="TargetR"> TRectF </param>
/// <param name="t"> Double between 0 and 1. Weight Source is t, weight Target is 1-t </param>
function Interpolate(SourceR, TargetR: TRectF; t: double): TRectF; inline;

/// <summary> Scale a TRectF by a factor </summary>
/// <param name="SourceR"> TRectF </param>
/// <param name="fact"> Scaling factor (e.g. 2 doubles the size) </param>
function ScaleRect(SourceR: TRectF; fact: double): TRectF; inline;

/// <summary> Center aRect in BigR </summary>
/// <param name="aRect"> (TRectF) Input rect to modify </param>
/// <param name="BigR"> (TRectF) Rect to center aRect in </param>
procedure CenterRect(var aRect: TRectF; BigR: TRectF);
inline

// Zoom-Pan speed modifiers
function SlowSlow(t: double): double;
inline;
function SlowFast(t: double): double; inline;
function FastSlow(t: double): double; inline;
function Linear(t: double): double; inline;
function Experiment(t: double): double; inline;

const
  EnvelopeFunction: array [TZoomSpeedEnvelope] of TEnvelopeFunction = (SlowSlow,
    FastSlow, SlowFast, Linear, Experiment);

Procedure ExpandSizeToAspect(Width, Height: integer; AR: TAVRational;
  var NewWidth, NewHeight: integer);

implementation

uses
  System.SysUtils;

function Experiment(t: double): double; inline;
begin
  result := 0.5 * sin(2 * Pi * t) + t;
end;

function SlowSlow(t: double): double; inline;
begin
  result := 3 * t * t - 2 * t * t * t;
end;

function SlowFast(t: double): double; inline;
begin
  result := t * t;
end;

function FastSlow(t: double): double; inline;
begin
  result := 2 * t - t * t;
end;

function Linear(t: double): double; inline;
begin
  result := t;
end;

function TZoom.ToRect(Width, Height: integer): TRectF;
begin
  Assert((tScale + sScale <= 1) and (lScale + sScale <= 1),
    'Zoom would be outside of picture bounds');
  result.Left := lScale * Width;
  result.Top := tScale * Height;
  result.Right := result.Left + sScale * Width;
  result.Bottom := result.Top + sScale * Height;
end;

function RectToZoom(r: TRectF; Width, Height: integer): TZoom; inline;
begin
  // the result only makes sense if r has the same aspect ratio as Width x Height
  Assert((r.Right - r.Left > 0) and (Width > 0) and (Height > 0));
  result.sScale := (r.Right - r.Left) / Width;
  result.lScale := r.Left / Width;
  result.tScale := r.Top / Height;
end;

function MakeZoom(l, t, s: double): TZoom; inline;
begin
  result.lScale := l;
  result.tScale := t;
  result.sScale := s;
end;

function Interpolate(SourceR, TargetR: TRectF; t: double): TRectF;
begin
  result.Left := SourceR.Left + t * (TargetR.Left - SourceR.Left);
  result.Top := SourceR.Top + t * (TargetR.Top - SourceR.Top);
  result.Right := SourceR.Right + t * (TargetR.Right - SourceR.Right);
  result.Bottom := SourceR.Bottom + t * (TargetR.Bottom - SourceR.Bottom);
end;

function ScaleRect(SourceR: TRectF; fact: double): TRectF;
begin
  result.Left := fact * SourceR.Left;
  result.Top := fact * SourceR.Top;
  result.Right := fact * SourceR.Right;
  result.Bottom := fact * SourceR.Bottom;
end;

procedure CenterRect(var aRect: TRectF; BigR: TRectF);
begin
  offsetRect(aRect, BigR.Left - aRect.Left, BigR.Top - aRect.Top);
  offsetRect(aRect, 0.5 * (BigR.Right - BigR.Left - aRect.Right + aRect.Left),
    0.5 * (BigR.Bottom - BigR.Top - aRect.Bottom + aRect.Top));
end;

function GetVideoProps(const Videofile: string): TVideoProps;
var
  ret: integer;
  fmt_ctx: PAVFormatContext;
  p: PPAVStream;
  vsn, { asn, } sn: Cardinal; // No. of first video/audio stream
  sar: TAVRational;
begin
  fmt_ctx := nil;
  (* open input file, and allocate format context *)
  ret := avformat_open_input(@fmt_ctx, MarshaledAString(UTF8String(Videofile)),
    nil, nil);
  try
    Assert(ret >= 0);
    ret := avformat_find_stream_info(fmt_ctx, nil);
    Assert(ret >= 0);
    p := fmt_ctx.streams;
    sn := 0;
    result.nrVideostreams := 0;
    result.nrAudiostreams := 0;
    vsn := 0;
    // asn := 0;  //future version: info about 1st audio stream
    while (sn < fmt_ctx.nb_streams) do
    begin
      if p^.codec.codec_type = AVMEDIA_TYPE_VIDEO then
      begin
        inc(result.nrVideostreams);
        if result.nrVideostreams = 1 then
        begin
          vsn := sn;
          result.VideoCodec := p^.codec.codec_id;
        end;
      end;
      if p^.codec.codec_type = AVMEDIA_TYPE_AUDIO then
      begin
        inc(result.nrAudiostreams);
        if result.nrAudiostreams = 1 then
        begin
          // asn := sn;
          result.AudioCodec := p^.codec.codec_id;
        end;
      end;
      inc(p);
      inc(sn);
    end;
    Assert(result.nrVideostreams > 0, 'No video stream found in ' + Videofile);
    p := fmt_ctx.streams;
    sn := 0;
    while sn < vsn do
    begin
      inc(p);
      inc(sn);
    end; // p^ now is the 1st video stream
    sar := p^.codecpar.sample_aspect_ratio;
    result.Width := p^.codecpar.Width;
    result.Height := p^.codecpar.Height;
    if (sar.num > 0) and (sar.den > 0) then
      result.TrueHeight := round(p^.codecpar.Height * sar.den / sar.num)
    else
      result.TrueHeight := p^.codecpar.Height;
    if p^.avg_frame_rate.den > 0 then // this should always be true ...
      result.FrameRate := round(p^.avg_frame_rate.num / p^.avg_frame_rate.den)
    else
      result.FrameRate := p^.r_frame_rate.num; // ?
    if p^.Duration <> AV_NOPTS_VALUE then
      result.Duration :=
        round(1000 * (p^.time_base.num / p^.time_base.den * p^.Duration))
    else
      result.Duration := round(1000 * (fmt_ctx.Duration / AV_TIME_BASE));
  finally
    avformat_close_input(@fmt_ctx);
  end;
end;

// dec_ctx is allocated and must be freed. stream_idx^ contains the stream index for the type_
procedure open_decoder_context(stream_idx: PInteger; dec_ctx: PPAVCodecContext;
  fmt_ctx: PAVFormatContext; type_: TAVMediaType);
var
  ret, stream_index: integer;
  st: PAVStream;
  avdec: PAVCodec;
  opts: PAVDictionary;
  p: PPAVStream;
begin
  opts := nil;

  ret := av_find_best_stream(fmt_ctx, type_, -1, -1, nil, 0);
  Assert(ret >= 0);
  stream_index := ret;
  p := fmt_ctx.streams;
  inc(p, stream_index);
  st := PAVStream(p^);

  (* find decoder for the stream *)
  avdec := avcodec_find_decoder(st.codecpar.codec_id);
  Assert(avdec <> nil);

  (* Allocate a codec context for the decoder *)
  dec_ctx^ := avcodec_alloc_context3(avdec);
  Assert(dec_ctx^ <> nil);

  (* Copy codec parameters from input stream to output codec context *)
  ret := avcodec_parameters_to_context(dec_ctx^, st.codecpar);
  Assert(ret >= 0);

  (* Init the decoders, without reference counting *)
  av_dict_set(@opts, 'refcounted_frames', '0', 0);
  ret := avcodec_open2(dec_ctx^, avdec, @opts);
  Assert(ret >= 0);
  stream_idx^ := stream_index;

end;

procedure GrabFrame(const bm: TBitmap; const Videofile: string;
  FrameNumber: integer);
var
  fmt_ctx: PAVFormatContext;
  video_dec_ctx: PAVCodecContext;
  Width, Height: integer;
  pix_fmt, pix_fmt_target: TAVPixelFormat;
  video_stream_idx: integer;
  frame: PAVFrame;
  pkt: TAVPacket;
  video_frame_count: integer;
  ret: integer;
  got_frame: integer;
  video_dst_data: array [0 .. 3] of PByte;
  video_dst_linesize: array [0 .. 3] of integer;
  convertCtx: PSwsContext;
  X: integer;
  row: PByte;
  bps: integer;
  sar: TAVRational;
  nh: integer;
  BitData: TBitmapData;
begin
  fmt_ctx := nil;
  video_dec_ctx := nil;
  frame := nil;
  for X := 0 to 3 do
    video_dst_data[X] := nil;
  (* open input file, and allocate format context *)
  ret := avformat_open_input(@fmt_ctx, MarshaledAString(UTF8String(Videofile)),
    nil, nil);
  Assert(ret >= 0);

  (* retrieve stream information *)
  ret := avformat_find_stream_info(fmt_ctx, nil);
  Assert(ret >= 0);

  open_decoder_context(@video_stream_idx, @video_dec_ctx, fmt_ctx,
    AVMEDIA_TYPE_VIDEO);

  (* allocate image where the decoded image will be put *)
  Width := video_dec_ctx.Width;
  Height := video_dec_ctx.Height;
  pix_fmt := video_dec_ctx.pix_fmt;
  // get the pixel aspect ratio, so we rescale the bitmap to the right size
  sar := video_dec_ctx.sample_aspect_ratio;
  if (sar.num > 0) and (sar.den > 0) then
    nh := round(Height * sar.den / sar.num)
  else
    nh := Height;
  ret := av_image_alloc(@video_dst_data[0], @video_dst_linesize[0], Width,
    Height, pix_fmt, 1);
  Assert(ret >= 0);
  // Conversion Context to BGRA
{$IFDEF ANDROID}
  pix_fmt_target := AV_PIX_FMT_RGBA;
{$ELSE}
  pix_fmt_target := AV_PIX_FMT_BGRA;
{$ENDIF}
  convertCtx := sws_getContext(Width, Height, pix_fmt, Width, nh,
    pix_fmt_target, SWS_Lanczos, nil, nil, nil);

  frame := av_frame_alloc();
  Assert(frame <> nil);
  try
    (* initialize packet, set data to NULL, let the demuxer fill it *)
    av_init_packet(@pkt);
    pkt.data := nil;
    pkt.size := 0;
    video_frame_count := 0;
    (* read frame FrameNumber-1 frames from the file *)
    while (video_frame_count < FrameNumber - 1) do
    begin
      ret := av_read_frame(fmt_ctx, @pkt);
      Assert(ret >= 0);
      if pkt.stream_index <> video_stream_idx then
      begin
        av_packet_unref(@pkt);
        Continue;
      end;
      // without decoding each frame, the grabbing doesn't work
      // except for some formats
      got_frame := 0;
      ret := avcodec_decode_video2(video_dec_ctx, frame, @got_frame, @pkt);
      if got_frame <> 0 then
        inc(video_frame_count);
      Assert(ret >= 0);
      av_packet_unref(@pkt);
    end;
    while (video_frame_count = FrameNumber - 1) do
    begin
      ret := av_read_frame(fmt_ctx, @pkt);
      Assert(ret >= 0);
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
      if got_frame <> 0 then
      begin
        // Convert the frame to 32bit-Bitmap
        bm.SetSize(Width, nh);
        Assert(bm.Map(TMapAccess.ReadWrite, BitData));
        row := BitData.data;
        // Always use Pitch instead of BytesPerLine!
        bps := BitData.Pitch;
        ret := sws_scale(convertCtx, @frame.data, @frame.linesize, 0, Height,
          @row, @bps);
        Assert(ret >= 0);
        bm.Unmap(BitData);
        inc(video_frame_count);
      end;
    end;

  finally
    av_packet_unref(@pkt);
    av_frame_free(@frame);
    avcodec_free_context(@video_dec_ctx);
    sws_freeContext(convertCtx);
    avformat_close_input(@fmt_ctx);
  end;
end;

Procedure ExpandSizeToAspect(Width, Height: integer; AR: TAVRational;
  var NewWidth, NewHeight: integer);
begin
  if Width / Height < AR.num / AR.den then
  // Add background right and left
  begin
    NewHeight := Height;
    NewWidth := round(Height / AR.den * AR.num);
  end
  else
  begin
    NewWidth := Width;
    NewHeight := round(Width * AR.den / AR.num);
  end;
end;

end.
