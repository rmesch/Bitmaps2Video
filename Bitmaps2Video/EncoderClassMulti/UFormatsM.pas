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
  Renate Schaaf
  Markus Humm
*****************************************************************************}
unit UFormatsM;

interface

uses
  System.SysUtils,
  System.Classes,
  FFMPEG;

const

  VideoCodecHigh = 7;

  /// <summary> The Libav Id's for the codecs I could get to work </summary>
  SupportedVideoCodecs: array [0 .. VideoCodecHigh] of TAVCodecId =
    (AV_CODEC_ID_NONE, AV_CODEC_ID_MPEG1VIDEO, AV_CODEC_ID_MPEG2VIDEO,
    AV_CODEC_ID_MJPEG, AV_CODEC_ID_MPEG4, AV_CODEC_ID_H264,
    // Theora not played by VLC-player but by Windows movie player
    AV_CODEC_ID_THEORA, AV_CODEC_ID_HEVC);

  FileFormatHigh = 7;
  /// <summary> The file extensions of formats I could find a codec for </summary>
  SupportedFileFormats: array [0 .. FileFormatHigh] of string = ('.avi', '.mp4',
    '.mkv', '.f4v', '.mov', '.mpg', '.vob', '.m4v');

type
  EFormatException = class(Exception);
  ECodecException = class(Exception);

  /// <summary> Base class of classes that know how to set up properties for a given codec. </summary>
  TBaseCodecSetup = class
  private
    function GetPixelformat: TAVPixelformat;
    function GetCodec: PAVCodec;
  protected
    fPreferredOutputPixelformat: TAVPixelformat;
    fCodecId: TAVCodecId;
    fpopt: PAVDictionary;
  public
    /// <summary> Override to set the correct pixel format etc. </summary>
    constructor Create(CodecId: TAVCodecId); virtual;
    destructor Destroy; override;
    /// <summary> Override to set up the correct properties of the codec context and fopt </summary>
    procedure CodecContextProps(CodecCtx: PAVCodecContext); virtual;
    /// <summary> Override to translate quality to bitrate for the codec </summary>
    function QualityToBitrate(Quality: byte; Width, Height, Rate: integer)
      : int64; virtual;
    property OutputPixelFormat: TAVPixelformat read GetPixelformat;
    property Codec: PAVCodec read GetCodec;
    property OptionsDictionary: PAVDictionary read fpopt;
  end;

  TH264Setup = class(TBaseCodecSetup)
  public
    procedure CodecContextProps(CodecCtx: PAVCodecContext); override;
    function QualityToBitrate(Quality: byte;Width, Height, Rate: integer): int64; override;
  end;

  /// <summary> H265 is still experimental and dog slow </summary>
  TH265Setup = class(TH264Setup)
  public
    function QualityToBitrate(Quality: byte; Width, Height, Rate: integer)
      : int64; override;
  end;

  TMpegSetup = class(TBaseCodecSetup)
  public
    function QualityToBitrate(Quality: byte; Width, Height, Rate: integer)
      : int64; override;
  end;

  TMJpegSetup = class(TBaseCodecSetup)
  public
    constructor Create(CodecId: TAVCodecId); override;
    function QualityToBitrate(Quality: byte; Width, Height, Rate: integer)
      : int64; override;
  end;

  TCodecSetupClass = class of TBaseCodecSetup;

  /// <summary>
  ///   Required for list Supported codecs on ARC enabled Delphi versions, as
  ///   storing integers/sets typecasted to TObject leads to crashes there
  ///   as the, in such a case non existing, reference counting mechanism is
  ///   to be called.
  /// </summary>
  TCodecIdWrapper = class(TObject)
  strict private
    FCodecId: TAVCodecId;
  public
    constructor Create(CodecId: TAVCodecId);

    /// <summary>
    ///   The "wrapped" CodeId
    /// </summary>
    property CodecId: TAVCodecId
      read   FCodecId;
  end;

function GetOutputFormat(const Ext: string): PAVOutputFormat;

/// <summary> Use to list the short names of the codecs supported by the file format defined by Ext.
/// The corresponding codec-id is stored in strings.objects.
/// IndexOfPreferredCodec is the index in strings that holds the default codec for the file format.
/// Can be -1, if that codec is not in the list of the ones we support.</summary>
procedure ListSupportedCodecs(const Ext: string; const Strings: TStrings;
  var IndexOfPreferredCodec: integer);

/// <summary> List extensions of supported file formats in Strings. </summary>
procedure ListSupportedFileFormats(const Strings: TStrings);

/// <summary> Returns ID of video codec Libav prefers to use for a file with extension Ext.
/// Raises exception if the codec is not supported by us.</summary>
function PreferredCodec(Ext: string): TAVCodecId;

/// <summary> Returns the class suitable to set up properties of the codec defined by CodecId </summary>
function CodecSetupClass(CodecId: TAVCodecId): TCodecSetupClass;

/// <summary> Register your own TCodecSetupClass to be used to set up the codec with ID CodecID. Returns true if successfully registered.
/// CodecID must be a valid ID for a Libav-codec, see libavcodec.pas. </summary>
function RegisterEncoder(CodecId: TAVCodecId;
 CodecSetupClass: TCodecSetupClass; OverwriteIfExists: boolean): boolean;

implementation

uses System.Generics.Collections;

var
  /// <Summary> Used to find the encoder setup class for a given codec-id. Use the RegisterEncoder procedure to support more codecs. </summary>
  EncoderDictionary: TDictionary<TAVCodecId, TCodecSetupClass>;

const
  CodecSetupClasses: array [0 .. VideoCodecHigh] of TCodecSetupClass = (nil,
    TMpegSetup, TMpegSetup, TMJpegSetup, TMpegSetup, TH264Setup,
    TMpegSetup, TH265Setup);

  /// <summary> Makes the basic encoder dictionary on start-up </summary>
procedure MakeEncoderDictionary;
var
  i: integer;
begin
  EncoderDictionary := TDictionary<TAVCodecId, TCodecSetupClass>.Create;
  // Don't add the 0-Id
  for i := 1 to VideoCodecHigh do
    EncoderDictionary.Add(SupportedVideoCodecs[i], CodecSetupClasses[i]);
  EncoderDictionary.TrimExcess;
end;

function CodecSetupClass(CodecId: TAVCodecId): TCodecSetupClass;
begin
  Result := nil;
  if EncoderDictionary.ContainsKey(CodecId) then
    Result := EncoderDictionary.Items[CodecId];
end;

function RegisterEncoder(CodecId: TAVCodecId;
  CodecSetupClass: TCodecSetupClass; OverwriteIfExists: boolean): boolean;
begin
  if OverwriteIfExists then
    EncoderDictionary.AddOrSetValue(CodecId, CodecSetupClass)
  else if EncoderDictionary.ContainsKey(CodecId) then
  begin
    Result := false;
    Exit;
  end
  else
    EncoderDictionary.Add(CodecId, CodecSetupClass);
  Result := true;
end;

function GetOutputFormat(const Ext: string): PAVOutputFormat;
var bytes: TArray<Byte>;
begin
  bytes:=TEncoding.UTF8.GetBytes('Example' + Ext);
  setlength(bytes,length(bytes)+1);
  bytes[length(bytes)-1]:=Byte(#0);
  Result := av_guess_format(nil, PAnsiChar(@bytes[0]), nil);
end;

function PreferredCodec(Ext: string): TAVCodecId;
var bytes: TArray<Byte>;
var
  fmt: PAVOutputFormat;
begin
  fmt := GetOutputFormat(Ext);
  if (fmt = nil) then
    raise EFormatException.Create('No matching format for ' + Ext + ' found');
  bytes:=TEncoding.UTF8.GetBytes('Example' + Ext);
  setlength(bytes,length(bytes)+1);
  bytes[length(bytes)-1]:=Byte(#0);
  Result := av_guess_codec(fmt, nil, PAnsiChar(@bytes[0]),
    nil, AVMEDIA_TYPE_VIDEO);
  if not EncoderDictionary.ContainsKey(Result) then
  begin
    raise EFormatException.Create('Preferred codec for ' + Ext +
      ' not supported');
  end;
end;

procedure ListSupportedCodecs(const Ext: string; const Strings: TStrings;
  var IndexOfPreferredCodec: integer);
var
  CodecId: TAVCodecId;
  Codec: PAVCodec;
  fmt: PAVOutputFormat;
begin
  try
    fmt := GetOutputFormat(Ext);
  except
    On EFormatException do
      Raise;
  end;
  IndexOfPreferredCodec := -1;
  Strings.Clear;
  for CodecId in EncoderDictionary.Keys do
    if (avformat_query_codec(fmt, CodecId, FF_COMPLIANCE_STRICT) = 1) then
    begin
      Codec := avcodec_find_encoder(CodecId);
      if Codec <> nil then
      begin
        Strings.AddObject(String(Codec.name), TCodecIdWrapper.Create(CodecId));
        if PreferredCodec(Ext) = CodecId then
          IndexOfPreferredCodec := Strings.Count - 1;
      end;
    end;
end;

procedure ListSupportedFileFormats(const Strings: TStrings);
var
  i: integer;
begin
  Strings.Clear;
  for i := 0 to FileFormatHigh do
    Strings.Add(SupportedFileFormats[i]);
end;

{ TBaseCodecSetup }

procedure TBaseCodecSetup.CodecContextProps(CodecCtx: PAVCodecContext);
begin
  Assert(assigned(CodecCtx),
    'TBaseCodecSetup.CodecContextProps: Codec Context not allocated.');
  CodecCtx.pix_fmt := fPreferredOutputPixelformat;
end;

constructor TBaseCodecSetup.Create(CodecId: TAVCodecId);
begin
  fCodecId := CodecId;
  fPreferredOutputPixelformat := AV_PIX_FMT_YUV420P;
  fpopt := nil;
end;

destructor TBaseCodecSetup.Destroy;
begin
  av_dict_free(@fpopt);
  inherited;
end;

function TBaseCodecSetup.GetCodec: PAVCodec;
var
  Codec: PAVCodec;
begin
  Codec := avcodec_find_encoder(fCodecId);
  if Codec = nil then
    raise ECodecException.Create('Codec not found');
  Result := Codec;
end;

function TBaseCodecSetup.GetPixelformat: TAVPixelformat;
begin
  Result := fPreferredOutputPixelformat;
end;

function TBaseCodecSetup.QualityToBitrate(Quality: byte;
  Width, Height, Rate: integer): int64;
var
  SquareRoot, factlow, facthigh: double;
  pixels, BitRateLow, BitRateHigh: integer;
begin
  SquareRoot := 1 / sqrt(192 * 108 * 30);
  factlow := 80 * SquareRoot;
  facthigh := 700 * SquareRoot;
  pixels := Width * Height * Rate;
  BitRateLow := round(sqrt(pixels) * factlow);
  // trying to match recommended values
  BitRateHigh := round(sqrt(pixels) * facthigh);
  Result := 1000 * round(BitRateLow + 0.01 * Quality *
    (BitRateHigh - BitRateLow));
end;

{ TMJpegSetup }

constructor TMJpegSetup.Create(CodecId: TAVCodecId);
begin
  inherited;
  fPreferredOutputPixelformat := AV_PIX_FMT_YUVJ422P;
end;

function TMJpegSetup.QualityToBitrate(Quality: byte;
  Width, Height, Rate: integer): int64;
begin
  Result := inherited QualityToBitrate(Quality, Width, Height, Rate);
  Result := Result * 7;
end;

{ TH265Setup }

function TH265Setup.QualityToBitrate(Quality: byte;
  Width, Height, Rate: integer): int64;
begin
  Result := inherited QualityToBitrate(Quality, Width, Height, Rate);
  Result := 3 * Result div 4;
end;

{ TMpegSetup }

function TMpegSetup.QualityToBitrate(Quality: byte;
  Width, Height, Rate: integer): int64;
begin
  Result := inherited QualityToBitrate(Quality, Width, Height, Rate);
  Result := 3 * Result div 2;
end;

{ TH264Setup }

function TH264Setup.QualityToBitrate(Quality: byte;Width, Height, Rate: integer): int64;
begin
  Result:=inherited QualityToBitrate(Quality,Width,Height,rate);

  //from 18 to 48. Lower in Libav is better, scale is log, we make it linear.
  //Anything below 18 is a waste of time and MB
//  qual:=AnsiString(inttostr(round(18-30*(ln(1+Quality)/ln(101)-1))));
//  ret := av_dict_set(@fpopt, 'crf', PAnsiChar(qual), 0);
//  Assert(ret >= 0, 'av_dict_set error ' + inttostr(ret));
end;

procedure TH264Setup.CodecContextProps(CodecCtx: PAVCodecContext);
var
  ret: integer;
begin
  inherited;
  // these options only work for H264 and H265

  ret := av_dict_set(@fpopt, 'preset', 'slow', 0);
  Assert(ret >= 0, 'av_dict_set error ' + inttostr(ret));
  ret := av_dict_set(@fpopt, 'tune', 'zerolatency', 0);
  Assert(ret >= 0, 'av_dict_set error ' + inttostr(ret));
//   ret := av_dict_set(@fpopt, 'tune', 'animation', 0);
//  Assert(ret >= 0, 'av_dict_set error ' + inttostr(ret));
end;

{ TCodecIdWrapper }

constructor TCodecIdWrapper.Create(CodecId: TAVCodecId);
begin
  inherited Create;

  FCodecId := COdecId;
end;

initialization

av_register_all();
MakeEncoderDictionary;

finalization

EncoderDictionary.Free;

end.
