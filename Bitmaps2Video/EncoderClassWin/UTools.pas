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

  Copyright 2020 Renate Schaaf
  ***************************************************************************** }
unit UTools;

interface

uses WinApi.Windows, System.Types, VCL.Graphics, FFMpeg;

type
  TVideoProps = record
    Width, Height, TrueHeight: integer;
    FrameRate: integer;
    nrVideostreams, nrAudiostreams: integer;
    VideoCodec, AudioCodec: TAVCodecID;
    Duration: int64;
  end;

  ///<summary> (lScale,tScale): LeftTop as fractions of (Width,Height). sScale: Size as Fraction of Width/Height </summary>
  TZoom=record
    lScale, tScale, sScale: double;
    function ToRect(Width,Height: integer): TRectF; inline;
  end;

  // All bitmaps should be pf32bit

  // Follows: "bicubic" resampler based on ideas by Anders Melander, Mike Lischke
  // and Eric Grange. It supports float values for filter radii and float-valued
  // zoom rectangles. Source, Target should be pf32bit. Target must be set to the
  // correct size.
procedure ZoomResampleTripleOnly(const Source, Target: TBitmap;
  SourceRect: TRectF; Radius: single);

// DeleteScans Algorithm taking float values for the zoom rectangle into account
// Suitable for making smooth zooms out of an enlarged source, and is almost
// as fast as GDI
procedure ZoomDeleteScansTripleOnly(const Src, Dest: TBitmap; rs: TRectF);

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

/// <summary> Convert frame number FrameNumber of a video file into a bitmap </summary>
procedure GrabFrame(const bm: TBitmap; const Videofile: string;
  FrameNumber: integer);
/// <summary> Get video time in ms of the Videofile </summary>
function GetVideoTime(const Videofile: string): int64;
/// <summary> Get video width, height and frame rate. TrueHeight is the height when not square pixels sizes are made square (sar) </summary>
function GetVideoProps(const Videofile: string): TVideoProps;

procedure open_decoder_context(stream_idx: PInteger; dec_ctx: PPAVCodecContext;
  fmt_ctx: PAVFormatContext; type_: TAVMediaType);

procedure ExpandToAspectRatio(const bm: TBitmap; aFrame: PAVFrame;
  AR: TAVRational; Background: byte);

function RectToZoom(r: TRectF; Width,Height: integer): TZoom; inline;

function MakeZoom(l,t,s: double): TZoom; inline;

// Zoom-Pan speed modifiers
function SlowSlow(t: double): double; inline;
function SlowFast(t: double): double; inline;
function FastSlow(t: double): double; inline;
function Linear(t: double): double; inline;
function Experiment(t: double): double; inline;

function GetTempfolder: string;

/// <summary> Utility useful to create a correctly formatted string to create a filter </summary>
function snprintf(buf: PAnsiChar; size: Cardinal; const fmt: PAnsiChar)
  : integer;
cdecl varargs;
external 'msvcrt' name '_snprintf';

type
  TEnvelopeFunction = function(t: double): double;
  TZoomSpeedEnvelope = (zeSlowSlow, zeFastSlow, zeSlowFast, zeLinear,
    zeExperiment);


const
  EnvelopeFunction: array [TZoomSpeedEnvelope] of TEnvelopeFunction = (SlowSlow,
    FastSlow, SlowFast, Linear, Experiment);

implementation

uses math;

function TZoom.ToRect(Width, Height: integer): TRectF;
 begin
   Assert((tScale+sScale<=1) and (lScale+sScale<=1),'Zoom would be outside of picture bounds');
   Result.Left:=lScale*Width;
   Result.Top:=tScale*Height;
   Result.Right:=Result.Left+sScale*Width;
   Result.Bottom:=Result.Top+sScale*Height;
 end;

  function RectToZoom(r: TRectF; Width,Height: integer): TZoom; inline;
  begin
    //the result only makes sense if r has the same aspect ratio as Width x Height
    Assert((r.Right-r.Left>0) and (Width>0) and (Height>0));
    Result.sScale:=(r.Right-r.Left)/Width;
    Result.lScale:=r.Left/Width;
    Result.tScale:=r.Top/Height;
  end;

  function MakeZoom(l,t,s: double): TZoom; inline;
  begin
    Result.lScale:=l;
    Result.tScale:=t;
    Result.sScale:=s;
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

function GetVideoTime(const Videofile: string): int64;
var
    {
    ret: integer;
      fmt_ctx: PAVFormatContext;
  }
  P: TVideoProps;
begin
  P:=GetVideoProps(Videofile);
  result:=P.Duration;
end;

function GetVideoProps(const Videofile: string): TVideoProps;
var
  ret: integer;
  fmt_ctx: PAVFormatContext;
  p: PPAVStream;
  vsn, {asn,} sn: Cardinal; // No of first video/audio stream
  sar: TAVRational;
begin
  fmt_ctx := nil;
  (* open input file, and allocate format context *)
  ret := avformat_open_input(@fmt_ctx, PAnsiChar(AnsiString(Videofile)),
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
    //asn := 0;  //future version: info about 1st audio stream
    while (sn < fmt_ctx.nb_streams) do
    begin
      if p^.codec.codec_type = AVMEDIA_TYPE_VIDEO then
      begin
        inc(result.nrVideostreams);
        if result.nrVideostreams = 1 then
        begin
          vsn := sn;
          Result.VideoCodec:=p^.codec.codec_id;
        end;
      end;
      if p^.codec.codec_type = AVMEDIA_TYPE_AUDIO then
      begin
        inc(result.nrAudiostreams);
        if result.nrAudiostreams = 1 then
        begin
          //asn := sn;
          Result.AudioCodec:=p^.codec.codec_id;
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
    if p^.duration <> AV_NOPTS_VALUE then
    Result.Duration:=round(1000*(p^.time_base.num/p^.time_base.den*p^.duration))
    else
    Result.Duration:=round(1000*(fmt_ctx.duration/AV_TIME_BASE));
  finally
    avformat_close_input(@fmt_ctx);
  end;
end;

procedure GrabFrame(const bm: TBitmap; const Videofile: string;
  FrameNumber: integer);
var
  fmt_ctx: PAVFormatContext;
  video_dec_ctx: PAVCodecContext;
  Width, Height: integer;
  pix_fmt: TAVPixelFormat;
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
begin
  fmt_ctx := nil;
  video_dec_ctx := nil;
  frame := nil;
  for X := 0 to 3 do
    video_dst_data[X] := nil;
  (* open input file, and allocate format context *)
  ret := avformat_open_input(@fmt_ctx, PAnsiChar(AnsiString(Videofile)),
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
  convertCtx := sws_getContext(Width, Height, pix_fmt, Width, nh,
    AV_PIX_FMT_BGRA, SWS_Lanczos, nil, nil, nil);

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
    // Convert the frame to pf32-Bitmap
    bm.PixelFormat := pf32bit;
    bm.SetSize(Width, nh);
    row := bm.ScanLine[0];
    bps := -((Width * 32 + 31) and not 31) div 8;
    ret:=sws_scale(ConvertCtx,@frame.data, @frame.linesize, 0, Height,
            @row, @bps);
    Assert(ret>=0);
  finally
    av_packet_unref(@pkt);
    av_frame_free(@frame);
    avcodec_free_context(@video_dec_ctx);
    sws_freeContext(convertCtx);
    avformat_close_input(@fmt_ctx);
  end;
end;

function GetTempfolder: string;
var
  l: integer;
begin
  SetLength(result, MAX_PATH + 1);
  l := GetTempPath(MAX_PATH, PChar(result));
  SetLength(result, l);
  if result[Length(result)] = '\' then
    result := copy(result, 1, Length(result) - 1);
end;

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

procedure ExpandToAspectRatio(const bm: TBitmap; aFrame: PAVFrame;
  AR: TAVRational; Background: byte);
var
  xstart, ystart: integer;
  pix_fmt: TAVPixelFormat;
  ret: integer;
  bmRow, FrameRow, FrameStart: PByte;
  y, bps, jump: integer;
begin
  assert((bm.Width > 0) and (bm.Height > 0));
  bm.PixelFormat:=pf32bit;
  if bm.Width / bm.Height < AR.num / AR.den then
  // Add black right and left
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
  pix_fmt := AV_PIX_FMT_BGRA;
  aFrame.format := Ord(pix_fmt);
  ret := av_frame_get_buffer(aFrame, 0);
  assert(ret >= 0);
  av_frame_make_writable(aFrame);
  jump:=aFrame.linesize[0];
  // Fill the frame with gray 0: black 255: white
  FillChar(aFrame.data[0]^, aFrame.Height * jump, Background);

  // Copy bm to aFrame
    bmRow := bm.Scanline[0];
    bps:=((bm.Width * 32 + 31) and not 31) div 8;
    FrameRow := aFrame.data[0];
    inc(FrameRow, ystart * jump);
    for y := 0 to bm.Height - 1 do
    begin
      FrameStart := FrameRow;
      inc(FrameStart, 4 * xstart);
      Move(bmRow^, FrameStart^, bps);
      dec(bmRow, bps);
      inc(FrameRow, jump);
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

const
  beta = 0.54; // f(beta)=0
  beta2 = beta * beta;
  alpha = 105 / (16 - 112 * beta2);
  aa = 1 / 7 * alpha;
  bb = -1 / 5 * alpha * (2 + beta2);
  cc = 1 / 3 * alpha * (1 + 2 * beta2);
  dd = -alpha * beta2;
  // constants computed with maple for polynomial fit

function AntiMyFilter(X: double): double; inline;
// Antiderivative of a filter function similar to bicubic, but I like it better
begin
  if X < -1 then
    result := -0.5
  else if X < 1 then
    result := aa * X * X * X * X * X * X * X + bb * X * X * X * X * X + cc * X *
      X * X + dd * X

  else
    result := 0.5;
end;

procedure MakeContributors(r: single; SourceSize, TargetSize: integer;
  SourceStart, SourceFloatwidth: double; var Contribs: TContribArray);
// r: Filterradius
var
  xCenter, scale, rr: double;
  X, j: integer;
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

  for X := 0 to TargetSize - 1 do
  begin
    xCenter := (X + 0.5) * scale;
    TrueMin := Ceil(xCenter - rr + SourceStart - 1);
    TrueMax := Floor(xCenter + rr + SourceStart);
    Contribs[X].Min := Min(Max(TrueMin, 0), SourceSize - 1);
    // make sure not to read in negative pixel locations
    Mx := Max(Min(TrueMax, SourceSize - 1), 0);
    // make sure not to read past w1-1 in the source
    Contribs[X].High := Mx - Contribs[X].Min;
    assert(Contribs[X].High >= 0); // hasn't failed lately:)
    // High=Number of contributing pixels minus 1
    SetLength(Contribs[X].Weights, Contribs[X].High + 1);
    with Contribs[X] do
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

procedure ZoomResampleTripleOnly(const Source, Target: TBitmap;
  SourceRect: TRectF; Radius: single);
var
  ContribsX, ContribsY: TContribArray;

  OldWidth, OldHeight: integer;
  NewWidth, NewHeight: integer;
  // Target needs to be set to correct size

  Sbps, Tbps: integer;
  tr, tg, tb: integer; // total red etc.
  dr, dg, db: integer; // For Subsum
  ps, pT: PRGBQuad;
  rs, rT, rStart, rTStart: PByte; // Row start in Source, Target
  weightx, weighty, weightxStart: PInteger;
  rx, gx, bx: TIntArray;
  X, Y, xs, ys, ymin, ymax: integer;
  highx, highy, minx, miny: integer;
  runr, rung, runb: PInteger;
  runrstart, rungstart, runbstart: PInteger;
begin
  Source.PixelFormat := pf32bit;
  Target.PixelFormat := pf32bit; // for safety
  NewWidth := Target.Width;
  NewHeight := Target.Height;
  OldWidth := Source.Width;
  OldHeight := Source.Height;

  Tbps := ((NewWidth * 32 + 31) and not 31) div 8;
  // BytesPerScanline Target
  Sbps := ((OldWidth * 32 + 31) and not 31) div 8;
  // BytesPerScanline Source
  // I'm assuming bottom-up bitmaps, could be adjusted, but I've never needed it.

  MakeContributors(Radius, OldWidth, NewWidth, SourceRect.Left,
    SourceRect.Right - SourceRect.Left, ContribsX);
  MakeContributors(Radius, OldHeight, NewHeight, SourceRect.Top,
    SourceRect.Bottom - SourceRect.Top, ContribsY);
  ymin := ContribsY[0].Min;
  ymax := ContribsY[NewHeight - 1].High + ContribsY[NewHeight - 1].Min;

  SetLength(rx, ymax - ymin + 1);
  SetLength(gx, ymax - ymin + 1);
  SetLength(bx, ymax - ymin + 1); // cache arrays

  rStart := Source.ScanLine[ymin];
  rTStart := Target.ScanLine[0];

  // Compute color at each target pixel (x,y)

  runrstart := @rx[0];
  rungstart := @gx[0];
  runbstart := @bx[0];

  for X := 0 to NewWidth - 1 do
  begin
    rs := rStart;
    highx := ContribsX[X].High;
    minx := ContribsX[X].Min;
    weightxStart := @ContribsX[X].Weights[0];
    runr := runrstart;
    rung := rungstart;
    runb := runbstart;
    for Y := ymin to ymax do
    begin

      // For each source line y
      // Sum up weighted color values at source pixels ContribsX[x].Min+xs
      // 0<=xs<=ContribsX[x].High
      // and store the results in rx[y-ymin], gx[y-ymin] etc.
      ps := PRGBQuad(rs);
      inc(ps, minx);

      weightx := weightxStart;
      db := weightx^ * ps.rgbBlue;
      dg := weightx^ * ps.rgbGreen;
      dr := weightx^ * ps.rgbRed;
      for xs := 1 to highx do
      begin
        inc(weightx);
        inc(ps);
        db := db + weightx^ * ps.rgbBlue;
        dg := dg + weightx^ * ps.rgbGreen;
        dr := dr + weightx^ * ps.rgbRed;
      end;
      // store results in rx,gx,bx
      runr^ := dr;
      rung^ := dg;
      runb^ := db;
      inc(runr);
      inc(rung);
      inc(runb);
      dec(rs, Sbps);
    end;
    // Average in y-direction:
    // For each target line y sum up weighted colors
    // rx[ys+ContribsY[y].Min-ymin], 0<=ys<=ContribsY[y].High, (same for gx, bx)
    // Store result in tr,tg,tb ("total red" etc.)
    // round and assign to TargetPixel[x,y]
    rT := rTStart;
    for Y := 0 to NewHeight - 1 do
    begin
      pT := PRGBQuad(rT);
      inc(pT, X);
      highy := ContribsY[Y].High;
      miny := ContribsY[Y].Min - ymin;
      weighty := @ContribsY[Y].Weights[0];
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
      pT.rgbBlue := Min(tb, 255);
      pT.rgbGreen := Min(tg, 255);
      pT.rgbRed := Min(tr, 255);
      dec(rT, Tbps);
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

procedure ZoomDeleteScansTripleOnly(const Src, Dest: TBitmap; rs: TRectF);

var
  iwd, ihd, iws, ihs, bs, bd: integer;
  X, Y: integer;
  xsteps, ysteps: TIntArray;
  Rows, rowd: PByte;
  Stepsx, Stepsy: PInteger;
  ts, td: PRGBQuad;
begin

  iwd := Dest.Width;
  ihd := Dest.Height;
  assert((iwd > 1) and (ihd > 1), 'Dest Bitmap too small');
  iws := Src.Width;
  ihs := Src.Height;
  Src.PixelFormat := pf32bit;
  Dest.PixelFormat := pf32bit;

  bs := (iws * 32 + 31) and not 31;
  bs := bs div 8; // BytesPerScanline Source
  bd := (iwd * 32 + 31) and not 31;
  bd := bd div 8; // BytesPerScanline Dest
  MakeSteps(iwd, iws, rs.Left, rs.Right - rs.Left, xsteps, 1);
  MakeSteps(ihd, ihs, rs.Top, rs.Bottom - rs.Top, ysteps, bs);

  Rows := Src.ScanLine[0];
  rowd := Dest.ScanLine[0];
  Stepsy := @ysteps[0];

  for Y := 0 to ihd - 1 do
  begin
    dec(Rows, Stepsy^); // bottom-up
    ts := PRGBQuad(Rows);
    td := PRGBQuad(rowd);
    Stepsx := @xsteps[0];
    for X := 0 to iwd - 1 do
    begin
      inc(ts, Stepsx^);
      PRGBTriple(td)^ := PRGBTriple(ts)^;
      inc(td);
      inc(Stepsx)
    end;
    dec(rowd, bd);
    inc(Stepsy);
  end;
end;

end.
