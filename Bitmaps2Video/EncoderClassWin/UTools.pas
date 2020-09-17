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

procedure open_decoder_context(stream_idx: PInteger; dec_ctx: PPAVCodecContext;
  fmt_ctx: PAVFormatContext; type_: TAVMediaType);

// Zoom-Pan speed modifiers
function SlowSlow(t: double): double; inline;
function SlowFast(t: double): double; inline;
function FastSlow(t: double): double; inline;
function Linear(t: double): double; inline;
function Experiment(t: double): double; inline;

function GetTempfolder: string;

type
  TEnvelopeFunction = function(t: double): double;
  TZoomSpeedEnvelope = (zeSlowSlow, zeFastSlow, zeSlowFast, zeLinear,
    zeExperiment);

const
  EnvelopeFunction: array [TZoomSpeedEnvelope] of TEnvelopeFunction = (SlowSlow,
    FastSlow, SlowFast, Linear, Experiment);

implementation

uses math;

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
  width, height: integer;
  pix_fmt: TAVPixelFormat;
  // video_stream: PAVStream;
  video_stream_idx: integer;
  frame: PAVFrame;
  pkt: TAVPacket;
  video_frame_count: integer;
  ret: integer;
  got_frame: integer;
  video_dst_data: array [0 .. 3] of PByte;
  video_dst_linesize: array [0 .. 3] of integer;
  convertCtx: PSwsContext;
  rgbPic: PAVFrame;
  x, y: integer;
  ps: PRGBQuad;
  row: PByte;
  px, py: PByte;
  jump: integer;
  bps: integer;
begin
  fmt_ctx := nil;
  video_dec_ctx := nil;
  // video_stream := nil;
  frame := nil;
  rgbPic := nil;
  for x := 0 to 3 do
    video_dst_data[x] := nil;
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
  width := video_dec_ctx.width;
  height := video_dec_ctx.height;
  pix_fmt := video_dec_ctx.pix_fmt;
  ret := av_image_alloc(@video_dst_data[0], @video_dst_linesize[0], width,
    height, pix_fmt, 1);
  assert(ret >= 0);
  // Conversion Context to BGR
  convertCtx := sws_getContext(width, height, pix_fmt, width, height,
    AV_PIX_FMT_BGR24, SWS_FAST_BILINEAR, nil, nil, nil);
  // Allocate storage for the rgb-frame
  rgbPic := av_frame_alloc();
  assert(rgbPic <> nil);
  frame := av_frame_alloc();
  assert(frame <> nil);
  try

    rgbPic.Format := Ord(AV_PIX_FMT_BGR24);
    rgbPic.width := width;
    rgbPic.height := height;
    av_frame_get_buffer(rgbPic, 0);

    (* initialize packet, set data to NULL, let the demuxer fill it *)
    av_init_packet(@pkt);
    pkt.data := nil;
    pkt.size := 0;
    video_frame_count := 0;
    (* read frame FrameNumber from the file *)
    ret := 0;
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

    // Convert the yuv-frame to rgb-frame
    ret := av_frame_make_writable(rgbPic);
    assert(ret >= 0);
    ret := sws_scale(convertCtx, @frame.data, @frame.linesize, 0, height,
      @rgbPic.data, @rgbPic.linesize);
    assert(ret >= 0);
    // Store the frame in a bmp

    bm.PixelFormat := pf32bit;
    bm.SetSize(width, height);
    row := bm.ScanLine[0];
    bps := ((width * 32 + 31) and not 31) div 8;
    py := @PByte(rgbPic.data[0])[0];
    jump := rgbPic.linesize[0];
    for y := 0 to height - 1 do
    begin
      ps := PRGBQuad(row);
      px := py;
      for x := 0 to width - 1 do
      begin
        PRGBTriple(ps)^ := PRGBTriple(px)^;
        inc(px, 3);
        inc(ps);
      end;
      dec(row, bps);
      inc(py, jump);
    end;
  finally
    av_packet_unref(@pkt);
    av_frame_free(@rgbPic);
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
  SetLength(Result, MAX_PATH + 1);
  l := GetTempPath(MAX_PATH, PChar(Result));
  SetLength(Result, l);
  if Result[Length(Result)] = '\' then
    Result := copy(Result, 1, Length(Result) - 1);
end;

function Experiment(t: double): double; inline;
begin
  Result := 0.5 * sin(2 * Pi * t) + t;
end;

function SlowSlow(t: double): double; inline;
begin
  Result := 3 * t * t - 2 * t * t * t;
end;

function SlowFast(t: double): double; inline;
begin
  Result := t * t;
end;

function FastSlow(t: double): double; inline;
begin
  Result := 2 * t - t * t;
end;

function Linear(t: double): double; inline;
begin
  Result := t;
end;

function Interpolate(SourceR, TargetR: TRectF; t: double): TRectF;
begin
  Result.Left := SourceR.Left + t * (TargetR.Left - SourceR.Left);
  Result.Top := SourceR.Top + t * (TargetR.Top - SourceR.Top);
  Result.Right := SourceR.Right + t * (TargetR.Right - SourceR.Right);
  Result.Bottom := SourceR.Bottom + t * (TargetR.Bottom - SourceR.Bottom);
end;

function ScaleRect(SourceR: TRectF; fact: double): TRectF;
begin
  Result.Left := fact * SourceR.Left;
  Result.Top := fact * SourceR.Top;
  Result.Right := fact * SourceR.Right;
  Result.Bottom := fact * SourceR.Bottom;
end;

procedure CenterRect(var aRect: TRectF; BigR: TRectF);
begin
  offsetRect(aRect, BigR.Left - aRect.Left, BigR.Top - aRect.Top);
  offsetRect(aRect, 0.5 * (BigR.Right - BigR.Left - aRect.Right + aRect.Left),
    0.5 * (BigR.Bottom - BigR.Top - aRect.Bottom + aRect.Top));
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

function AntiMyFilter(x: double): double; inline;
// Antiderivative of a filter function similar to bicubic, but I like it better
begin
  if x < -1 then
    Result := -0.5
  else if x < 1 then
    Result := aa * x * x * x * x * x * x * x + bb * x * x * x * x * x + cc * x *
      x * x + dd * x

  else
    Result := 0.5;
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
    assert(Contribs[x].High >= 0); // hasn't failed lately:)
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
  x, y, xs, ys, ymin, ymax: integer;
  highx, highy, minx, miny: integer;
  runr, rung, runb: PInteger;
  runrstart, rungstart, runbstart: PInteger;
begin
  Source.PixelFormat := pf32bit;
  Target.PixelFormat := pf32bit; // for safety
  NewWidth := Target.width;
  NewHeight := Target.height;
  OldWidth := Source.width;
  OldHeight := Source.height;

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
    for y := 0 to NewHeight - 1 do
    begin
      pT := PRGBQuad(rT);
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
  x, y: integer;
  xsteps, ysteps: TIntArray;
  Rows, rowd: PByte;
  Stepsx, Stepsy: PInteger;
  ts, td: PRGBQuad;
begin

  iwd := Dest.width;
  ihd := Dest.height;
  assert((iwd > 1) and (ihd > 1), 'Dest Bitmap too small');
  iws := Src.width;
  ihs := Src.height;
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

  for y := 0 to ihd - 1 do
  begin
    dec(Rows, Stepsy^); // bottom-up
    ts := PRGBQuad(Rows);
    td := PRGBQuad(rowd);
    Stepsx := @xsteps[0];
    for x := 0 to iwd - 1 do
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
