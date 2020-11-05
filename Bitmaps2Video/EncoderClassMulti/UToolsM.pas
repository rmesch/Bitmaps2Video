unit UToolsM;

interface

uses
  FFMPEG,
  FMX.Graphics,
  System.types,
  UFormatsM;

type
  TVideoProps = record
    Width, Height, TrueHeight: integer;
    FrameRate: integer;
    nrVideostreams, nrAudiostreams: integer;
    VideoCodec, AudioCodec: TAVCodecID;
    Duration: int64;
  end;

procedure open_decoder_context(stream_idx: PInteger; dec_ctx: PPAVCodecContext;
  fmt_ctx: PAVFormatContext; type_: TAVMediaType);

/// <summary> Get video properties. TrueHeight is the height when not square pixels sizes are made square (sar) </summary>
function GetVideoProps(const Videofile: string): TVideoProps;

/// <summary> Store frame number FrameNumber of Videofile in the bitmap bm </summary>
/// fails under Windows for Videofiles encoded with DVvideo
procedure GrabFrame(const bm: TBitmap; const Videofile: string;
  FrameNumber: integer);

procedure ExpandToAspectRatio(const bm: TBitmap; aFrame: PAVFrame;
  AR: TAVRational; Background: byte);

Procedure ExpandSizeToAspect(Width,Height: integer; AR: TAVRational; var NewWidth,NewHeight: integer);

implementation

uses
  System.SysUtils;

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
    assert(ret >= 0);
    ret := avformat_find_stream_info(fmt_ctx, nil);
    assert(ret >= 0);
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
    assert(result.nrVideostreams > 0, 'No video stream found in ' + Videofile);
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
  assert(ret >= 0);
  stream_index := ret;
  p := fmt_ctx.streams;
  inc(p, stream_index);
  st := PAVStream(p^);

  (* find decoder for the stream *)
  avdec := avcodec_find_decoder(st.codecpar.codec_id);
  assert(avdec <> nil);

  (* Allocate a codec context for the decoder *)
  dec_ctx^ := avcodec_alloc_context3(avdec);
  assert(dec_ctx^ <> nil);

  (* Copy codec parameters from input stream to output codec context *)
  ret := avcodec_parameters_to_context(dec_ctx^, st.codecpar);
  assert(ret >= 0);

  (* Init the decoders, without reference counting *)
  av_dict_set(@opts, 'refcounted_frames', '0', 0);
  ret := avcodec_open2(dec_ctx^, avdec, @opts);
  assert(ret >= 0);
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
  assert(ret >= 0);

  (* retrieve stream information *)
  ret := avformat_find_stream_info(fmt_ctx, nil);
  assert(ret >= 0);

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
  assert(ret >= 0);
  // Conversion Context to BGRA
{$IFDEF ANDROID}
  pix_fmt_target := AV_PIX_FMT_RGBA;
{$ELSE}
  pix_fmt_target := AV_PIX_FMT_BGRA;
{$ENDIF}
  convertCtx := sws_getContext(Width, Height, pix_fmt, Width, nh,
    pix_fmt_target, SWS_Lanczos, nil, nil, nil);

  frame := av_frame_alloc();
  assert(frame <> nil);
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
      assert(ret >= 0);
      if pkt.stream_index <> video_stream_idx then
      begin
        av_packet_unref(@pkt);
        Continue;
      end;
      inc(video_frame_count);
      // without decoding each frame, the grabbing doesn't work
      // except for some formats
      got_frame := 0;
      ret := avcodec_decode_video2(video_dec_ctx, frame, @got_frame, @pkt);
      assert(ret >= 0);
      av_packet_unref(@pkt);
    end;
    while (video_frame_count = FrameNumber - 1) do
    begin
      ret := av_read_frame(fmt_ctx, @pkt);
      assert(ret >= 0);
      if pkt.stream_index <> video_stream_idx then
      begin
        av_packet_unref(@pkt);
        Continue;
      end;
      inc(video_frame_count);

    end;
    assert(pkt.stream_index = video_stream_idx);
    (* decode video frame *)
    // !avcodec_decode_video2 is deprecated, but I couldn't get
    // the replacement avcode_send_packet and avcodec_receive_frame to work
    got_frame := 0;
    ret := avcodec_decode_video2(video_dec_ctx, frame, @got_frame, @pkt);
    assert(ret >= 0);
    // Convert the frame to 32bit-Bitmap
    bm.SetSize(Width, nh);
    assert(bm.Map(TMapAccess.ReadWrite, BitData));
    row := BitData.data;
    // Always use Pitch instead of BytesPerLine!
    bps := BitData.Pitch;
    ret := sws_scale(convertCtx, @frame.data, @frame.linesize, 0, Height,
      @row, @bps);
    assert(ret >= 0);
    bm.Unmap(BitData);
  finally
    av_packet_unref(@pkt);
    av_frame_free(@frame);
    avcodec_free_context(@video_dec_ctx);
    sws_freeContext(convertCtx);
    avformat_close_input(@fmt_ctx);
  end;
end;

procedure ExpandToAspectRatio(const bm: TBitmap; aFrame: PAVFrame;
  AR: TAVRational; Background: byte);
var
  xstart, ystart: integer;
  pix_fmt: TAVPixelFormat;
  ret: integer;
  BitmapData: TBitmapData;
  bmRow, FrameRow, FrameStart: PByte;
  y: integer;
begin
  assert((bm.Width > 0) and (bm.Height > 0));
  if bm.Width / bm.Height < AR.num / AR.den then
  // Add background right and left
  begin
    aFrame.Height := bm.Height;
    aFrame.Width := round(bm.Height / AR.den * AR.num);
    xstart := (aFrame.Width - bm.Width) div 2;
    ystart := 0;
  end
  else
  begin
    aFrame.Width := bm.Width;
    aFrame.Height := round(bm.Width / AR.num * AR.den);
    xstart := 0;
    ystart := (aFrame.Height - bm.Height) div 2;
  end;
  // Set up the Frame with the right pixelformat
{$IFDEF ANDROID}
  pix_fmt := AV_PIX_FMT_RGBA;
{$ELSE}
  pix_fmt := AV_PIX_FMT_BGRA;
{$ENDIF}
  aFrame.format := Ord(pix_fmt);
  ret := av_frame_get_buffer(aFrame, 0);
  assert(ret >= 0);
  av_frame_make_writable(aFrame);
  // Fill the frame with gray 0:black 255:white
  FillChar(aFrame.data[0]^, aFrame.Height * aFrame.linesize[0], Background);

  // Copy bm to aFrame
  assert(bm.Map(TMapAccess.Read, BitmapData));
  try
    bmRow := BitmapData.GetScanline(0);
    FrameRow := aFrame.data[0];
    inc(FrameRow, ystart * aFrame.linesize[0]);
    for y := 0 to bm.Height - 1 do
    begin
      FrameStart := FrameRow;
      inc(FrameStart, 4 * xstart);
      Move(bmRow^, FrameStart^, BitmapData.BytesPerLine);
      inc(bmRow, BitmapData.Pitch);
      inc(FrameRow, aFrame.linesize[0]);
    end;
  finally
    bm.Unmap(BitmapData);
  end;
end;

Procedure ExpandSizeToAspect(Width,Height: integer; AR: TAVRational; var NewWidth,NewHeight: integer);
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
