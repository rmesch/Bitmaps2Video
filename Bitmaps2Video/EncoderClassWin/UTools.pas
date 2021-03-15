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

uses WinApi.Windows, System.Types, VCL.Graphics, FFMpeg, System.SysUtils,
  VCL.Controls;

// the rescaling routines benefit substantially from optimization turned on
// whereas overflow checking is stepping on the brakes,
// disable if you don't trust the settings
{$IFOPT O-}
{$DEFINE O_MINUS}
{$O+}
{$ENDIF}
{$IFOPT Q+}
{$DEFINE Q_PLUS}
{$Q-}
{$ENDIF}

type
  TVideoProps = record
    Width, Height, TrueHeight: integer;
    FrameRate: integer;
    nrVideostreams, nrAudiostreams: integer;
    VideoCodec, AudioCodec: TAVCodecID;
    Duration: int64;
    VideoTimeBase: TAVRational;
  end;

  /// <summary> (lScale,tScale): LeftTop as fractions of (Width,Height). sScale: Size as Fraction of Width/Height </summary>
  TZoom = record
    lScale, tScale, sScale: double;
    function ToRect(Width, Height: integer): TRectF; inline;
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

type
  TStartEvent = reference to procedure;
  TStopQuery = reference to procedure(VideoTimeElapsed: int64;
    var quit: boolean);

  /// <summary> Play the video stream of a video file. Any that VLC-player can render may work. </summary>
  /// <param name="aRect"> Rectangle within which to display the video. Will be fit to largest size preserving aspect ratio. </param>
  /// <param name="VideoStartTime" Time in ms into the video at which to start. Not very efficient when >0. </param>
  /// <param name="OnStart" Event called at the start of the video display. Can be used to play a matching audio stream. </param>
  /// <param name="OnStopQuery" Event called at regular intervals. Return quit:=true to stop the video. If not assigned the video plays until finished." </param>
procedure PlayVideoStream(aCanvas: TCanvas; const Videofile: string;
  aRect: TRect; VideoStartTime: int64 = 0; OnStart: TStartEvent = nil;
  OnStopQuery: TStopQuery = nil); overload;

procedure PlayVideoStream(aControl: TCustomControl; const Videofile: string;
  VideoStartTime: int64 = 0; OnStart: TStartEvent = nil;
  OnStopQuery: TStopQuery = nil); overload;

(* **** The following procedures are used internally *********** *)

procedure open_decoder_context(stream_idx: PInteger; dec_ctx: PPAVCodecContext;
  fmt_ctx: PAVFormatContext; type_: TAVMediaType);

procedure FrameToBGRAFrame(FrameSrc, FrameDst: PAVFrame;
  FrameInitialized: boolean; TrueHeight: integer = 0);

procedure BitmapToBGRAFrame(const bm: TBitmap; FrameDst: PAVFrame;
  FrameInitialized: boolean);

procedure ExpandToAspectRatio(const bm: TBitmap; aFrame: PAVFrame;
  AR: TAVRational; Background: byte; FrameInitialized: boolean); overload;

procedure ExpandToAspectRatio(FrameSrc, FrameDst: PAVFrame; AR: TAVRational;
  Background: byte; FrameInitialized: boolean;
  TrueHeight: integer = 0); overload;

Procedure ExpandSizeToAspect(Width, Height: integer; AR: TAVRational;
  var NewWidth, NewHeight: integer); inline;

function RectToZoom(r: TRectF; Width, Height: integer): TZoom; inline;

function MakeZoom(l, t, s: double): TZoom; inline;

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

uses System.Math, WinApi.MMSystem, VCL.Dialogs;

function TZoom.ToRect(Width, Height: integer): TRectF;
begin
  Assert((tScale + sScale <= 1) and (lScale + sScale <= 1),
    'Zoom would be outside of picture bounds');
  Result.Left := lScale * Width;
  Result.Top := tScale * Height;
  Result.Right := Result.Left + sScale * Width;
  Result.Bottom := Result.Top + sScale * Height;
end;

function RectToZoom(r: TRectF; Width, Height: integer): TZoom; inline;
begin
  // the result only makes sense if r has the same aspect ratio as Width x Height
  Assert((r.Right - r.Left > 0) and (Width > 0) and (Height > 0));
  Result.sScale := (r.Right - r.Left) / Width;
  Result.lScale := r.Left / Width;
  Result.tScale := r.Top / Height;
end;

function MakeZoom(l, t, s: double): TZoom; inline;
begin
  Result.lScale := l;
  Result.tScale := t;
  Result.sScale := s;
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

function GetVideoTime(const Videofile: string): int64;
var
  p: TVideoProps;
begin
  p := GetVideoProps(Videofile);
  Result := p.Duration;
end;

function GetVideoProps(const Videofile: string): TVideoProps;
var
  ret: integer;
  fmt_ctx: PAVFormatContext;
  p: PPAVStream;
  vsn, {
    asn,
  } sn: Cardinal; // No. of (last) video/audio stream
  sar: TAVRational;
begin
  fmt_ctx := nil;
  (* open input file, and allocate format context *)
  ret := avformat_open_input(@fmt_ctx, PAnsiChar(AnsiString(Videofile)),
    nil, nil);
  try
    Assert(ret >= 0);
    ret := avformat_find_stream_info(fmt_ctx, nil);
    Assert(ret >= 0);
    p := fmt_ctx.streams;
    sn := 0;
    Result.nrVideostreams := 0;
    Result.nrAudiostreams := 0;
    vsn := 0;
    // asn := 0; // future version: info about 1st audio stream
    while (sn < fmt_ctx.nb_streams) do
    begin
      if p^.codec.codec_type = AVMEDIA_TYPE_VIDEO then
      begin
        inc(Result.nrVideostreams);
        // if Result.nrVideostreams = 1 then
        // begin
        vsn := sn;
        Result.VideoCodec := p^.codec.codec_id;
        // end;
      end;
      if p^.codec.codec_type = AVMEDIA_TYPE_AUDIO then
      begin
        inc(Result.nrAudiostreams);
        if Result.nrAudiostreams = 1 then
        begin
          // asn := sn;
          Result.AudioCodec := p^.codec.codec_id;
        end;
      end;
      inc(p);
      inc(sn);
    end;
    Assert(Result.nrVideostreams > 0, 'No video stream found in ' + Videofile);
    p := fmt_ctx.streams;
    sn := 0;
    while sn < vsn do
    begin
      inc(p);
      inc(sn);
    end; // p^ now is the last video stream
    Result.VideoTimeBase := p^.time_base;
    sar := p^.codecpar.sample_aspect_ratio;
    Result.Width := p^.codecpar.Width;
    Result.Height := p^.codecpar.Height;
    if (sar.num > 0) and (sar.den > 0) then
      Result.TrueHeight := round(p^.codecpar.Height * sar.den / sar.num)
    else
      Result.TrueHeight := p^.codecpar.Height;
    if p^.Duration <> AV_NOPTS_VALUE then
      Result.Duration :=
        round(1000 * (p^.time_base.num / p^.time_base.den * p^.Duration))
    else
      Result.Duration := round(1000 * (fmt_ctx.Duration / AV_TIME_BASE));
    Result.FrameRate := -1;
    if p^.avg_frame_rate.den > 0 then // this should always be true ...
      Result.FrameRate := round(p^.avg_frame_rate.num / p^.avg_frame_rate.den)
    else if p^.r_frame_rate.den > 0 then
      Result.FrameRate := round(p^.r_frame_rate.num / p^.r_frame_rate.den);
    if (Result.FrameRate <= 0) or (Result.FrameRate > 480) then
    begin
      if p^.r_frame_rate.den > 0 then
        Result.FrameRate := round(p^.r_frame_rate.num / p^.r_frame_rate.den)
      else if p^.nb_frames > 0 then
        Result.FrameRate := round(Result.Duration / p^.nb_frames);
    end;
    if (Result.FrameRate <= 0) or (Result.FrameRate > 480) then
      raise exception.create('No meaningful frame rate could be determined');
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
  nh: integer;
  temp: PAVFrame;
  p: TVideoProps;
begin
  fmt_ctx := nil;
  video_dec_ctx := nil;
  frame := nil;
  p := GetVideoProps(Videofile);
  for X := 0 to 3 do
    video_dst_data[X] := nil;
  (* open input file, and allocate format context *)
  ret := avformat_open_input(@fmt_ctx, PAnsiChar(AnsiString(Videofile)),
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
  nh := p.TrueHeight;
  ret := av_image_alloc(@video_dst_data[0], @video_dst_linesize[0], Width,
    Height, pix_fmt, 1);
  Assert(ret >= 0);
  // Conversion Context to BGRA
  convertCtx := sws_getContext(Width, Height, AV_PIX_FMT_BGRA, Width, nh,
    AV_PIX_FMT_BGRA, SWS_Bicubic, nil, nil, nil);

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

    got_frame := 0;
    // Read the next meaningful videoframe
    while (got_frame = 0) do
    begin
      ret := av_read_frame(fmt_ctx, @pkt);
      Assert(ret >= 0);
      if pkt.stream_index <> video_stream_idx then
      begin
        av_packet_unref(@pkt);
        Continue;
      end;
      got_frame := 0;
      ret := avcodec_decode_video2(video_dec_ctx, frame, @got_frame, @pkt);
      Assert(ret >= 0);
      av_packet_unref(@pkt);
    end;
    // Convert the frame to pf32-Bitmap
    // Some video formats don't works nicely with TBitmap,
    // So we convert to BGRA-Frame first
    temp := av_frame_alloc;
    try
      FrameToBGRAFrame(frame, temp, false);
      bm.PixelFormat := pf32bit;
      bm.SetSize(Width, nh);
      row := bm.ScanLine[0];
      bps := -(((Width * 32 + 31) and not 31) div 8);
      ret := sws_scale(convertCtx, @temp.data, @temp.linesize, 0, Height,
        @row, @bps);
      Assert(ret >= 0);
    finally
      av_frame_free(@temp);
    end;
  finally
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

procedure FrameToBGRAFrame(FrameSrc, FrameDst: PAVFrame;
  FrameInitialized: boolean; TrueHeight: integer = 0);
var
  convertCtx: PSwsContext;
begin
  if TrueHeight = 0 then
    TrueHeight := FrameSrc.Height;
  convertCtx := sws_getContext(FrameSrc.Width, FrameSrc.Height,
    TAVPixelFormat(FrameSrc.format), FrameSrc.Width, TrueHeight,
    AV_PIX_FMT_BGRA, SWS_Bicubic, nil, nil, nil);
  try
    if not FrameInitialized then
    begin
      FrameDst.Width := FrameSrc.Width;
      FrameDst.Height := TrueHeight;
      FrameDst.format := Ord(AV_PIX_FMT_BGRA);
      av_frame_get_buffer(FrameDst, 0);
    end;
    av_frame_make_writable(FrameDst);
    sws_scale(convertCtx, @FrameSrc.data, @FrameSrc.linesize, 0,
      FrameSrc.Height, @FrameDst.data, @FrameDst.linesize);
  finally
    sws_freeContext(convertCtx);
  end;
end;

procedure BitmapToBGRAFrame(const bm: TBitmap; FrameDst: PAVFrame;
  FrameInitialized: boolean);
var
  convertCtx: PSwsContext;
  bmData: PByte;
  bmLineSize: integer;
begin
  bm.PixelFormat := pf32bit; // for safety
  convertCtx := sws_getContext(bm.Width, bm.Height, AV_PIX_FMT_BGRA, bm.Width,
    bm.Height, AV_PIX_FMT_BGRA, SWS_FAST_BILINEAR, nil, nil, nil);
  try
    if not FrameInitialized then
    begin
      FrameDst.Width := bm.Width;
      FrameDst.Height := bm.Height;
      FrameDst.format := Ord(AV_PIX_FMT_BGRA);
      av_frame_get_buffer(FrameDst, 0);
    end;
    bmData := bm.ScanLine[0];
    bmLineSize := -(((bm.Width * 32 + 31) and not 31) div 8);
    av_frame_make_writable(FrameDst);
    sws_scale(convertCtx, @bmData, @bmLineSize, 0, bm.Height, @FrameDst.data,
      @FrameDst.linesize);
  finally
    sws_freeContext(convertCtx);
  end;
end;

procedure ExpandToAspectRatio(FrameSrc, FrameDst: PAVFrame; AR: TAVRational;
  Background: byte; FrameInitialized: boolean; TrueHeight: integer = 0);
var
  xstart, ystart: integer;
  ret: integer;
  bmRow, FrameRow, FrameStart: PByte;
  y, bps, jumpB, jumpF: integer;
  temp: PAVFrame;
begin
  Assert((FrameSrc.Width > 0) and (FrameSrc.Height > 0));
  // Convert the frame to BGRA
  if TrueHeight = 0 then
    TrueHeight := FrameSrc.Height;
  temp := av_frame_alloc;
  try
    FrameToBGRAFrame(FrameSrc, temp, false, TrueHeight);
    if FrameSrc.Width / TrueHeight < AR.num / AR.den then
    // Add background right and left
    begin
      if not FrameInitialized then
      begin
        FrameDst.Height := TrueHeight;
        FrameDst.Width := round(TrueHeight / AR.den * AR.num);
      end;
      xstart := (FrameDst.Width - FrameSrc.Width) div 2;
      ystart := 0;
    end
    else
    begin
      if not FrameInitialized then
      begin
        FrameDst.Width := FrameSrc.Width;
        FrameDst.Height := round(FrameSrc.Width / AR.num * AR.den);
      end;
      xstart := 0;
      ystart := (FrameDst.Height - TrueHeight) div 2;
    end;
    // Set up the Frame with the right pixelformat
    if not FrameInitialized then
    begin
      FrameDst.format := Ord(AV_PIX_FMT_BGRA);
      ret := av_frame_get_buffer(FrameDst, 0);
      Assert(ret >= 0);
    end;
    av_frame_make_writable(FrameDst);
    // Fill the frame with gray 0:black 255:white
    FillChar(FrameDst.data[0]^, FrameDst.Height * FrameDst.linesize[0],
      Background);

    // Copy FrameSrc to FrameDst
    bmRow := temp.data[0];
    FrameRow := FrameDst.data[0];
    bps := 4 * temp.Width;
    jumpF := FrameDst.linesize[0];
    jumpB := temp.linesize[0];
    inc(FrameRow, ystart * jumpF);
    xstart := 4 * xstart;
    for y := 0 to temp.Height - 1 do
    begin
      FrameStart := FrameRow;
      inc(FrameStart, xstart);
      Move(bmRow^, FrameStart^, bps);
      inc(bmRow, jumpB);
      inc(FrameRow, jumpF);
    end;
  finally
    av_frame_free(@temp);
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

procedure ExpandToAspectRatio(const bm: TBitmap; aFrame: PAVFrame;
  AR: TAVRational; Background: byte; FrameInitialized: boolean);
var
  xstart, ystart: integer;
  pix_fmt: TAVPixelFormat;
  ret: integer;
  bmRow, FrameRow, FrameStart: PByte;
  y, bps, jump, bmLineSize: integer;
begin
  Assert((bm.Width > 0) and (bm.Height > 0));
  bm.PixelFormat := pf32bit;
  if bm.Width / bm.Height < AR.num / AR.den then
  // Add black right and left
  begin
    if not FrameInitialized then
    begin
      aFrame.Height := bm.Height;
      aFrame.Width := round(bm.Height / AR.den * AR.num);
    end;
    xstart := (aFrame.Width - bm.Width) div 2;
    ystart := 0;
  end
  else
  begin
    if not FrameInitialized then
    begin
      aFrame.Width := bm.Width;
      aFrame.Height := round(bm.Width / AR.num * AR.den);
    end;
    xstart := 0;
    ystart := (aFrame.Height - bm.Height) div 2;
  end;
  // Set up the Frame with the right pixelformat
  if not FrameInitialized then
  begin
    pix_fmt := AV_PIX_FMT_BGRA;
    aFrame.format := Ord(pix_fmt);
    ret := av_frame_get_buffer(aFrame, 0);
    Assert(ret >= 0);
  end;
  av_frame_make_writable(aFrame);
  jump := aFrame.linesize[0];
  // Fill the frame with gray 0: black 255: white
  FillChar(aFrame.data[0]^, aFrame.Height * jump, Background);

  // Copy bm to aFrame
  bmRow := bm.ScanLine[0];
  bps := ((bm.Width * 32 + 31) and not 31) div 8;
  bmLineSize := 4 * bm.Width;
  xstart := 4 * xstart;
  FrameRow := aFrame.data[0];
  inc(FrameRow, ystart * jump);
  for y := 0 to bm.Height - 1 do
  begin
    FrameStart := FrameRow;
    inc(FrameStart, xstart);
    Move(bmRow^, FrameStart^, bmLineSize);
    dec(bmRow, bps);
    inc(FrameRow, jump);
  end;
end;

procedure PlayVideoStream(aCanvas: TCanvas; const Videofile: string;
  aRect: TRect; VideoStartTime: int64 = 0; OnStart: TStartEvent = nil;
  OnStopQuery: TStopQuery = nil); overload;
var
  bm: TBitmap;
  DisplayRect: TRect;
  p: TVideoProps;
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
  convertCtx: PSwsContext;
  X: integer;
  row: PByte;
  bps: integer;
  asp: double;
  elapsed, start, VideoTime: int64;
  FrameCount, DroppedFrames: integer;
  quit: boolean;
  RectW, RectH, bmW, bmH: integer;
  TimeBaseMS: TAVRational;
begin
  RectW := aRect.Right - aRect.Left;
  RectH := aRect.Bottom - aRect.Top;
  Assert((RectW > 0) and (RectH > 0),
    'Video Display must have positive width, height');
  fmt_ctx := nil;
  video_dec_ctx := nil;
  frame := nil;
  for X := 0 to 3 do
    video_dst_data[X] := nil;
  TimeBaseMS := av_make_q(1, 1000);
  FrameCount := 0;
  DroppedFrames := 0;
  bm := TBitmap.create;
  try
    p := GetVideoProps(Videofile);
    // Calculate the display rect
    DisplayRect.Left := 0;
    DisplayRect.Top := 0;
    asp := p.Width / p.TrueHeight;
    if asp < RectW / RectH then
    // display height = RectH
    begin
      DisplayRect.Bottom := RectH;
      DisplayRect.Right := round(RectH * asp);
    end
    else
    // display width = RectW
    begin
      DisplayRect.Right := RectW;
      DisplayRect.Bottom := round(RectW / asp);
    end;
    //This prevents some weird artifacts from showing up more often, like an extra vertical bar to the left
    //Some bug in FFMPEG?
    if (DisplayRect.Right mod 4 = 0) then
      dec(DisplayRect.Right);
    if (DisplayRect.Bottom mod 4 = 0) then
      dec(DisplayRect.Bottom);

    bm.PixelFormat := pf32bit;
    bmH := DisplayRect.Bottom;
    bmW := DisplayRect.Right;
    bm.SetSize(bmW, bmH);
    row := bm.ScanLine[0];
    bps := -(((bmW * 32 + 31) and not 31) div 8);
    // Center the DisplayRect
    offsetRect(DisplayRect, aRect.Left + (RectW - DisplayRect.Right) div 2,
      aRect.Top + (RectH - DisplayRect.Bottom) div 2);
    (* open input file, and allocate format context *)
    ret := avformat_open_input(@fmt_ctx, PAnsiChar(AnsiString(Videofile)),
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
    ret := av_image_alloc(@video_dst_data[0], @video_dst_linesize[0], Width,
      Height, pix_fmt, 1);
    Assert(ret >= 0);
    // Conversion Context to BGRA
    convertCtx := sws_getContext(Width, Height, pix_fmt, bmW, bmH,
      AV_PIX_FMT_BGRA, SWS_FAST_BILINEAR, nil, nil, nil);
    frame := av_frame_alloc();
    Assert(frame <> nil);
    try
      av_init_packet(@pkt);
      pkt.data := nil;
      pkt.size := 0;

      (* read packages until VideoStartTime *)
      VideoTime := -VideoStartTime;
      while VideoTime < 0 do
      begin
        ret := av_read_frame(fmt_ctx, @pkt);
        if ret < 0 then
          break;
        if pkt.stream_index <> video_stream_idx then
        begin
          av_packet_unref(@pkt);
          Continue;
        end;
        VideoTime := av_rescale_q(max(pkt.pts, pkt.dts), p.VideoTimeBase,
          TimeBaseMS) - VideoStartTime;
        av_packet_unref(@pkt);
      end;
      if assigned(OnStart) then
        OnStart;
      start := TimeGetTime;
      (* read frames, decode, convert, display *)
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
        VideoTime := av_rescale_q(max(pkt.pts, pkt.dts), p.VideoTimeBase,
          TimeBaseMS) - VideoStartTime;
        inc(FrameCount);
        got_frame := 0;
        (* decode video frame *)
        // !avcodec_decode_video2 is deprecated, but I couldn't get
        // the replacement avcode_send_packet and avcodec_receive_frame to work
        begin
          ret := avcodec_decode_video2(video_dec_ctx, frame, @got_frame, @pkt);
          Assert(ret >= 0);
          // Convert the frame to pf32-Bitmap
          if got_frame <> 0 then
          begin
            ret := sws_scale(convertCtx, @frame.data, @frame.linesize, 0,
              frame.Height, @row, @bps);
            Assert(ret >= 0);
            elapsed := TimeGetTime - start;
            if elapsed < VideoTime then
              sleep(VideoTime - elapsed);
            BitBlt(aCanvas.Handle, DisplayRect.Left, DisplayRect.Top, bmW, bmH,
              bm.Canvas.Handle, 0, 0, SRCCopy);
          end;
        end;
        if FrameCount mod 20 = 0 then
          if assigned(OnStopQuery) then
          begin
            OnStopQuery(VideoTime + VideoStartTime, quit);
            if quit then
              break;
          end;

        av_packet_unref(@pkt);
      end;
    finally
      av_frame_free(@frame);
      avcodec_free_context(@video_dec_ctx);
      sws_freeContext(convertCtx);
      avformat_close_input(@fmt_ctx);
    end;
  finally
    bm.Free;
  end;
end;

type
  TCrack = class(TCustomControl);

procedure PlayVideoStream(aControl: TCustomControl; const Videofile: string;
  VideoStartTime: int64 = 0; OnStart: TStartEvent = nil;
  OnStopQuery: TStopQuery = nil); overload;
begin
  PlayVideoStream(TCrack(aControl).Canvas, Videofile, aControl.ClientRect,
    VideoStartTime, OnStart, OnStopQuery);
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
    Result := -0.5
  else if X < 1 then
    Result := aa * X * X * X * X * X * X * X + bb * X * X * X * X * X + cc * X *
      X * X + dd * X

  else
    Result := 0.5;
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
    Contribs[X].Min := Min(max(TrueMin, 0), SourceSize - 1);
    // make sure not to read in negative pixel locations
    Mx := max(Min(TrueMax, SourceSize - 1), 0);
    // make sure not to read past w1-1 in the source
    Contribs[X].High := Mx - Contribs[X].Min;
    Assert(Contribs[X].High >= 0); // hasn't failed lately:)
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
  X, y, xs, ys, ymin, ymax: integer;
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
        inc(db, weightx^ * ps.rgbBlue);
        inc(dg, weightx^ * ps.rgbGreen);
        inc(dr, weightx^ * ps.rgbRed);
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
      inc(pT, X);
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
        inc(tr, weighty^ * runr^);
        inc(tb, weighty^ * runb^);
        inc(tg, weighty^ * rung^);
      end;
      tr := max(tr, 0);
      tg := max(tg, 0);
      tb := max(tb, 0);
      // results could be negative, filter has negative values
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
  X, y: integer;
  xsteps, ysteps: TIntArray;
  Rows, rowd: PByte;
  Stepsx, Stepsy: PInteger;
  ts, td: PRGBQuad;
begin

  iwd := Dest.Width;
  ihd := Dest.Height;
  Assert((iwd > 1) and (ihd > 1), 'Dest Bitmap too small');
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

  for y := 0 to ihd - 1 do
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

{$IFDEF O_MINUS}
{$O-}
{$UNDEF O_MINUS}
{$ENDIF}
{$IFDEF Q_PLUS}
{$Q+}
{$UNDEF Q_PLUS}
{$ENDIF}

end.
