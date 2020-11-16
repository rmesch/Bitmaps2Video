unit UAndroidTools;

interface

uses
  System.Messaging,
  AndroidAPI.Helpers,
  AndroidAPI.Jni.Os,
  AndroidAPI.Jni.GraphicsContentViewText,
  AndroidAPI.Jni.Net,
  FMX.Platform.Android,
  AndroidAPI.JNIBridge,
  AndroidAPI.Jni.JavaTypes,
  AndroidAPI.Jni.App;

type
  TContentEnum = (ctVideo, ctImages, ctAudio, ctText, ctAll);

  TFileBrowseEvent = reference to procedure(var Filename: string;
    var Success: boolean);

  TFileBrowser = class
  private
    fOnExecute: TFileBrowseEvent;
    fFilename: string;
    class var FInstance: TFileBrowser;
    class function Instance: TFileBrowser;
  protected
    fMessageSubscriptionID: integer;
    procedure DoExecute(Success: boolean);
    function HandleIntentAction(const Data: JIntent): boolean;
    procedure HandleActivityMessage(const sender: TObject; const M: TMessage);
    procedure Initialize(Content: TContentEnum; OnExecute: TFileBrowseEvent);
  public
    class constructor Create;
    class destructor Destroy;
    /// <summary> Use Android API to have the user pick a file with a certain content type </summary>
    /// <param OnExecute> Event occuring when the user has picked a file. Write a (anonymous) procedure(var Filename: string; var Success: boolean) to handle the event. <param>
    class procedure BrowseForFile(Content: TContentEnum;
      OnExecute: TFileBrowseEvent);
  end;

procedure PlayVideo(VideoFilename: string);

implementation

uses System.Sysutils;

const
  ContentStrings: array [TContentEnum] of string = ('video/*', 'image/*',
    'audio/*', 'text/*', '*/*');

  { TFileBrowser }

class function TFileBrowser.Instance: TFileBrowser;
begin
  if not assigned(FInstance) then
    FInstance := TFileBrowser.Create;
  Result := FInstance;
end;

class constructor TFileBrowser.Create;
begin
  FInstance := nil;
end;

class destructor TFileBrowser.Destroy;
begin
  FInstance.free;
end;

class procedure TFileBrowser.BrowseForFile(Content: TContentEnum;
  OnExecute: TFileBrowseEvent);
begin
  Instance.Initialize(Content, OnExecute);
end;

procedure TFileBrowser.Initialize(Content: TContentEnum;
  OnExecute: TFileBrowseEvent);
var
  Intent: JIntent;
begin
  fOnExecute := OnExecute;
  fMessageSubscriptionID := TMessageManager.DefaultManager.SubscribeToMessage
    (TMessageResultNotification, HandleActivityMessage);
  Intent := TJIntent.Create;
  Intent.setType(StringToJString(ContentStrings[Content]));
  Intent.setAction(TJIntent.JavaClass.ACTION_GET_CONTENT);
  MainActivity.startActivityForResult(Intent, 0);
end;

procedure TFileBrowser.DoExecute(Success: boolean);
begin
  if assigned(fOnExecute) then
    fOnExecute(fFilename, Success);
end;

procedure TFileBrowser.HandleActivityMessage(const sender: TObject;
  const M: TMessage);
begin
  if M is TMessageResultNotification then
  begin
    DoExecute(HandleIntentAction(TMessageReceivedNotification(M).Value));
  end;
end;

function TFileBrowser.HandleIntentAction(const Data: JIntent): boolean;
var
  C: JCursor;
  I: integer;
begin
  C := MainActivity.getContentResolver.query(Data.getData, nil,
    StringToJString(''), nil, StringToJString(''));

  C.moveToFirst;
  Result := false;
  for I := 0 to C.getColumnCount - 1 do
  begin
    if JStringToString(C.getColumnName(I)) = '_data' then
    // '_data' column contains the path
    begin
      fFilename := JStringToString(C.getString(I));
      Result := true;
      Break;
    end;
  end;
  if not Result then
    fFilename := '';
end;

procedure PlayVideo(VideoFilename: string);
var
  Intent: JIntent;
  uri: Jnet_URI;
  ext: string;
begin
  ext := ExtractFileExt(VideoFilename);
  ext := Copy(ext, 2, length(ext) - 1);
  uri := TAndroidHelper.StrToJURI(VideoFilename);
  Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setDataAndType(uri, StringToJString('video/' + ext));
  TAndroidHelper.Activity.startActivity(Intent);
end;

end.
