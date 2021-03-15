object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Demo for TBitmapEncoder (VCL-based, Windows only)'
  ClientHeight = 494
  ClientWidth = 631
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  StyleElements = [seFont, seClient]
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 189
    Width = 631
    Height = 305
    ActivePage = TabSheet6
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 609
    object TabSheet6: TTabSheet
      Caption = 'Simple Animation'
      ImageIndex = 5
      ExplicitWidth = 601
      object ImageAni: TImage
        Left = 356
        Top = 21
        Width = 143
        Height = 133
        Proportional = True
        Stretch = True
      end
      object Label21: TLabel
        Left = 356
        Top = 160
        Width = 38
        Height = 13
        Caption = 'Preview'
      end
      object Label24: TLabel
        Left = 0
        Top = 238
        Width = 623
        Height = 39
        Align = alBottom
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'This is a demo for the method TBitmapEncoder.AddFrame. A video i' +
          's created from a series of drawings on a bitmap'#39's canvas. Frames' +
          ' are matched to the chosen aspect ratio by adding borders colore' +
          'd as chosen for Background.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 2052118
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitLeft = -3
        ExplicitWidth = 601
      end
      object ButtonAnimation: TButton
        Left = 20
        Top = 36
        Width = 165
        Height = 25
        Caption = 'Animate a Canvas-Drawing'
        TabOrder = 0
        OnClick = ButtonAnimationClick
      end
      object ProgressBar3: TProgressBar
        Left = 20
        Top = 80
        Width = 289
        Height = 17
        TabOrder = 1
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Simple Slideshow'
      ImageIndex = 6
      ExplicitWidth = 601
      DesignSize = (
        623
        277)
      object Label22: TLabel
        Left = 318
        Top = 216
        Width = 30
        Height = 13
        Caption = '          '
      end
      object Label23: TLabel
        Left = 8
        Top = 8
        Width = 217
        Height = 13
        Caption = 'Click on the images to load different pictures.'
      end
      object Label25: TLabel
        Left = 0
        Top = 238
        Width = 623
        Height = 39
        Align = alBottom
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'A series of images is turned into a slideshow video using the me' +
          'thods AddStillImage and CrossFade of TBitmapEncoder.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 2052118
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitTop = 237
      end
      object ScrollboxSlideshow: TScrollBox
        Left = 0
        Top = 32
        Width = 622
        Height = 145
        HorzScrollBar.Tracking = True
        VertScrollBar.Visible = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        ExplicitWidth = 600
        object Image1: TImage
          Left = 11
          Top = 3
          Width = 116
          Height = 116
          Proportional = True
          Stretch = True
          OnClick = Image1Click
        end
        object Image2: TImage
          Left = 134
          Top = 3
          Width = 116
          Height = 116
          Proportional = True
          Stretch = True
          OnClick = Image1Click
        end
        object Image3: TImage
          Left = 257
          Top = 3
          Width = 116
          Height = 116
          Proportional = True
          Stretch = True
          OnClick = Image1Click
        end
        object Image4: TImage
          Left = 380
          Top = 3
          Width = 116
          Height = 116
          Proportional = True
          Stretch = True
          OnClick = Image1Click
        end
        object Image5: TImage
          Left = 503
          Top = 3
          Width = 116
          Height = 116
          Proportional = True
          Stretch = True
          OnClick = Image1Click
        end
        object Image6: TImage
          Left = 626
          Top = 3
          Width = 116
          Height = 116
          Proportional = True
          Stretch = True
          OnClick = Image1Click
        end
        object Image7: TImage
          Left = 749
          Top = 3
          Width = 116
          Height = 116
          Proportional = True
          Stretch = True
          OnClick = Image1Click
        end
      end
      object ButtonMakeSlideshow: TButton
        Left = 14
        Top = 183
        Width = 247
        Height = 25
        Caption = 'Make a slideshow video (background thread)'
        TabOrder = 1
        OnClick = ButtonMakeSlideshowClick
      end
      object ProgressBar4: TProgressBar
        Left = 14
        Top = 214
        Width = 285
        Height = 17
        TabOrder = 2
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Zooming'
      ExplicitWidth = 601
      object ImageZoomPicture: TImage
        Left = 9
        Top = 10
        Width = 165
        Height = 111
        Proportional = True
        Stretch = True
      end
      object Label4: TLabel
        Left = 340
        Top = 81
        Width = 21
        Height = 13
        Caption = '       '
      end
      object Label9: TLabel
        Left = 340
        Top = 37
        Width = 21
        Height = 13
        Caption = '       '
      end
      object Label10: TLabel
        Left = 0
        Top = 208
        Width = 623
        Height = 69
        Align = alBottom
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'Make a movie by zooming into the picture displayed. The result w' +
          'ill be stored in the output file chosen and the encoder properti' +
          'es chosen will be used. This is a demo for the methods AddFrame,' +
          ' ZoomPan, AddStillImage and Freeze of TBitmapEncoder. If the pic' +
          'ture does not have the aspect ratio chosen, borders will be adde' +
          'd as chosen by Background.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 2052118
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitTop = 127
        ExplicitWidth = 497
      end
      object ButtonLoadZoomPicture: TButton
        Left = 188
        Top = 97
        Width = 141
        Height = 25
        Caption = 'Load picture'
        TabOrder = 0
        OnClick = ButtonLoadZoomPictureClick
      end
      object rgrpZoom: TRadioGroup
        Left = 186
        Top = 3
        Width = 143
        Height = 88
        Caption = 'Zooming done by'
        ItemIndex = 2
        Items.Strings = (
          'Antialias x2'
          'Antialias x4'
          'Antialias x6'
          '"Bicubic" Resample')
        TabOrder = 1
      end
      object ButtonMakeZoomMovie: TButton
        Left = 351
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Make Movie'
        TabOrder = 2
        OnClick = ButtonMakeZoomMovieClick
      end
      object ProgressBar1: TProgressBar
        Left = 351
        Top = 56
        Width = 234
        Height = 19
        Max = 17000
        MarqueeInterval = 100
        TabOrder = 3
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Inserting an existing video'
      ImageIndex = 1
      ExplicitWidth = 601
      object Label11: TLabel
        Left = 0
        Top = 204
        Width = 623
        Height = 73
        Align = alBottom
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'This demo shows how you can insert an existing video into your s' +
          'lideshow via TBitmapEncoder.AddVideo. Load a video from your har' +
          'd disk (any format that VLC-player can render should be Ok). Int' +
          'ro- and End- Slides will be added to the video and the result is' +
          ' stored in the output file. The video will be resized (proportio' +
          'nality preserved) and reencoded according to the settings. Prope' +
          'rties of the video and a picture of a frame are shown as a demo ' +
          'of the utilitis GetVideoProps and GrabFrame in UTools.pas.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 2052118
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitTop = 124
        ExplicitWidth = 601
      end
      object Label12: TLabel
        Left = 167
        Top = 134
        Width = 98
        Height = 43
        AutoSize = False
        Caption = '     '
        WordWrap = True
      end
      object ImageShowFrame: TImage
        Left = 3
        Top = 82
        Width = 153
        Height = 110
        Proportional = True
        Stretch = True
      end
      object Label17: TLabel
        Left = 166
        Top = 82
        Width = 50
        Height = 13
        Caption = 'Frame No.'
      end
      object Label19: TLabel
        Left = 3
        Top = 34
        Width = 104
        Height = 13
        Caption = 'Video clip to process: '
      end
      object ButtonLoadVideoClip: TButton
        Left = 3
        Top = 3
        Width = 157
        Height = 25
        Caption = 'Load a video clip'
        TabOrder = 0
        OnClick = ButtonLoadVideoClipClick
      end
      object ProgressBar2: TProgressBar
        Left = 277
        Top = 163
        Width = 308
        Height = 17
        TabOrder = 1
      end
      object MemoProps: TMemo
        Left = 331
        Top = 2
        Width = 267
        Height = 111
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 2
      end
      object FrameSpin: TSpinEdit
        Left = 166
        Top = 106
        Width = 82
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 15
        OnChange = FrameSpinChange
      end
      object EditVideoFile: TEdit
        Left = 3
        Top = 48
        Width = 315
        Height = 21
        Alignment = taRightJustify
        ReadOnly = True
        TabOrder = 4
      end
      object ButtonInsertClipMainThread: TButton
        Left = 277
        Top = 132
        Width = 143
        Height = 25
        Caption = 'Insert Clip (main thread)'
        TabOrder = 5
        OnClick = ButtonInsertClipMainThreadClick
      end
      object ButtonInsertClipBackgroundThread: TButton
        Left = 434
        Top = 132
        Width = 164
        Height = 25
        Caption = 'Insert Clip (background thread)'
        TabOrder = 6
        OnClick = ButtonInsertClipMainThreadClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Create from Video'
      Enabled = False
      ImageIndex = 2
      TabVisible = False
      ExplicitWidth = 601
      DesignSize = (
        623
        277)
      object Label13: TLabel
        Left = 0
        Top = 34
        Width = 620
        Height = 83
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'This is a demo for the constructor TBitmapEncoder.CreateFromVide' +
          'o. The main use of this constructor is to resume work on a video' +
          ' that has been previously interrupted with the unfinished result' +
          ' stored. This constructor loads the unfinished video, stores its' +
          ' frames in a new file without quality loss and reads all the enc' +
          'oder and format setting off the unfinished video. Once created, ' +
          'work can be resumed on the new video. As a demo, here some text ' +
          'frames are added to the video you load.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 5671452
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 537
      end
      object Label14: TLabel
        Left = 3
        Top = 123
        Width = 212
        Height = 98
        AutoSize = False
        Caption = 
          'This is the theory. In practice, it does not yet always work as ' +
          'intended. For .avi and its codecs it seems to work OK. Otherwise' +
          ' issues are listet in the memo to the right.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 100
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object Label15: TLabel
        Left = 190
        Top = 10
        Width = 21
        Height = 13
        Caption = '       '
      end
      object Button6: TButton
        Left = 5
        Top = 3
        Width = 179
        Height = 25
        Caption = 'Load video and process further'
        TabOrder = 0
      end
      object Memo1: TMemo
        Left = 243
        Top = 117
        Width = 316
        Height = 110
        Lines.Strings = (
          'If the unfinished video contains a reencoded video, '
          'there might be a problem with the proper timing,'
          'though I haven'#39't seen it lately.'
          ''
          'For .mkv and .mp4 the encoder sets the VFR '
          '(variable frame rate) tag in the joined video '
          'with the result that Windows Films and TV doesn'#39't '
          'play the appended part. VLC-player plays .mp4 OK, '
          'in .mkv frames can be are corrupted. '
          'So far all attempts to fix this have failed.'
          ''
          'There might be problems with other file formats,'
          'very little tested.')
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Add Audio'
      ImageIndex = 3
      ExplicitWidth = 601
      object Label5: TLabel
        Left = 20
        Top = 59
        Width = 21
        Height = 13
        Caption = '       '
      end
      object Label16: TLabel
        Left = 0
        Top = 194
        Width = 623
        Height = 83
        Align = alBottom
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'Demo for the utiliy procedure MuxStreams2. An audio file is adde' +
          'd as an audio stream to the output file video, which must have b' +
          'een created. Streams are just copied, no reencoding happens. Loa' +
          'd an audio file from your hard disk, the audio will be clipped t' +
          'o the video duration. Not all audio formats are compatible with ' +
          'all file formats. On a mismatch you get an exception. Also, some' +
          ' players might not be able to play the result, even if the file ' +
          'has been written successfully. Usually .mp3 files don'#39't pose pro' +
          'blems.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 2052118
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitTop = 114
        ExplicitWidth = 601
      end
      object ButtonAddAudio: TButton
        Left = 14
        Top = 28
        Width = 175
        Height = 25
        Caption = 'Add audio to the output file'
        TabOrder = 0
        OnClick = ButtonAddAudioClick
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Simple Video Player'
      ImageIndex = 4
      ExplicitWidth = 601
      object Panel1: TPanel
        Left = 0
        Top = 222
        Width = 623
        Height = 55
        Align = alBottom
        TabOrder = 0
        ExplicitWidth = 601
        DesignSize = (
          623
          55)
        object Label20: TLabel
          Left = 437
          Top = 6
          Width = 27
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '         '
          ExplicitLeft = 415
        end
        object ButtonStartPlaying: TButton
          Left = 153
          Top = 20
          Width = 176
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Start Playing at Trackbar Position'
          Enabled = False
          TabOrder = 0
          OnClick = ButtonStartPlayingClick
        end
        object ButtonStopPlaying: TButton
          Left = 335
          Top = 20
          Width = 134
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Stop Playing'
          Enabled = False
          TabOrder = 1
          OnClick = ButtonStopPlayingClick
        end
        object ButtonLoadVideoForPlaying: TButton
          Left = 8
          Top = 20
          Width = 139
          Height = 25
          Caption = 'Load Video File'
          TabOrder = 2
          OnClick = ButtonLoadVideoForPlayingClick
        end
        object TrackBar1: TTrackBar
          Left = 0
          Top = 0
          Width = 438
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          LineSize = 0
          TabOrder = 3
          TickStyle = tsNone
          ExplicitWidth = 416
        end
      end
      object Panel2: TPanel
        Left = 402
        Top = 0
        Width = 221
        Height = 222
        Align = alRight
        TabOrder = 1
        ExplicitLeft = 380
        object Label26: TLabel
          Left = 1
          Top = 1
          Width = 219
          Height = 220
          Align = alClient
          Alignment = taCenter
          AutoSize = False
          Caption = 
            'Demo for the utility procedure PlayVideoStream in UTools.pas. Vi' +
            'deo files can be loaded and displayed with very little delay whe' +
            'n started at time 0, which makes this procedure suitable for pre' +
            'views. The way the player works is however not very efficient CP' +
            'U-wise, use it for small clips, even though it can play movies. ' +
            'If you have bass.dll, you can use it to also have the audio-trac' +
            'k playing. Enable {$Define UseBass} at the top of this demo. You' +
            ' need to have bass.pas in your path and bass.dll in the exe-fold' +
            'er.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 2052118
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
          WordWrap = True
          ExplicitTop = 2
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 402
        Height = 222
        Align = alClient
        BevelEdges = []
        BevelOuter = bvNone
        Color = clBlack
        Ctl3D = False
        ParentBackground = False
        ParentCtl3D = False
        TabOrder = 2
        ExplicitLeft = -5
        ExplicitTop = -6
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 631
    Height = 189
    Align = alTop
    Color = 14216665
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = -6
    ExplicitWidth = 609
    object Label1: TLabel
      Left = 18
      Top = 4
      Width = 84
      Height = 13
      Caption = 'Output file name:'
    end
    object Label2: TLabel
      Left = 223
      Top = 51
      Width = 78
      Height = 13
      Caption = 'Encoding quality'
    end
    object Label3: TLabel
      Left = 18
      Top = 51
      Width = 59
      Height = 13
      Caption = 'Video height'
    end
    object Label6: TLabel
      Left = 150
      Top = 102
      Width = 39
      Height = 13
      Caption = 'Encoder'
    end
    object Label7: TLabel
      Left = 120
      Top = 51
      Width = 82
      Height = 13
      Caption = 'Frame rate [FPS]'
    end
    object Label8: TLabel
      Left = 18
      Top = 102
      Width = 69
      Height = 13
      Caption = 'Output format'
    end
    object Label18: TLabel
      Left = 322
      Top = 51
      Width = 56
      Height = 13
      Caption = 'Background'
    end
    object EditOutput: TEdit
      Left = 18
      Top = 24
      Width = 350
      Height = 21
      TabOrder = 0
      Text = 'Output.avi'
    end
    object ButtonOpenOutput: TButton
      Left = 374
      Top = 20
      Width = 27
      Height = 25
      Caption = '...'
      TabOrder = 1
      OnClick = ButtonOpenOutputClick
    end
    object SpinEditQuality: TSpinEdit
      Left = 223
      Top = 70
      Width = 71
      Height = 22
      MaxValue = 120
      MinValue = 0
      TabOrder = 2
      Value = 70
      OnChange = SpinEditQualityChange
    end
    object HC: TComboBox
      Left = 17
      Top = 70
      Width = 85
      Height = 21
      Style = csDropDownList
      ItemIndex = 3
      TabOrder = 3
      Text = '720'
      OnChange = HCChange
      Items.Strings = (
        '360'
        '540'
        '600'
        '720'
        '1080')
    end
    object CodecCombo: TComboBox
      Left = 150
      Top = 121
      Width = 173
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      OnChange = CodecComboChange
    end
    object RateCombo: TComboBox
      Left = 120
      Top = 70
      Width = 53
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 5
      Text = '30'
      OnChange = RateComboChange
      Items.Strings = (
        '1'
        '25'
        '30'
        '40'
        '50'
        '60')
    end
    object FormatCombo: TComboBox
      Left = 18
      Top = 121
      Width = 83
      Height = 21
      Style = csDropDownList
      TabOrder = 6
      OnChange = FormatComboChange
      Items.Strings = (
        '.avi'
        '.mp4'
        '.mkv'
        '.flv'
        '.f4v'
        '.mov'
        '.mpg'
        '.webm'
        '.m4v')
    end
    object ButtonRegisterNewCodec: TButton
      Left = 17
      Top = 148
      Width = 223
      Height = 25
      Caption = 'Register raw video as new codec (.avi)'
      TabOrder = 7
      OnClick = ButtonRegisterNewCodecClick
    end
    object RadioGroupAspect: TRadioGroup
      Left = 442
      Top = 54
      Width = 103
      Height = 78
      Caption = 'Output aspect ratio'
      ItemIndex = 1
      Items.Strings = (
        '16:9'
        '4:3'
        '3:2')
      TabOrder = 8
    end
    object BgCombo: TComboBox
      Left = 322
      Top = 70
      Width = 95
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 9
      Text = 'Black'
      Items.Strings = (
        'Black'
        'White'
        'Dark gray'
        'Light gray')
    end
    object ButtonPlayOutput: TButton
      Left = 407
      Top = 20
      Width = 138
      Height = 25
      Caption = 'Play Output Video'
      TabOrder = 10
      OnClick = ButtonPlayOutputClick
    end
  end
  object SD: TSaveDialog
    Filter = 
      'Videos (*.avi, *.mp4, *.mkv,*.f4v,*.mov,*.mpg,*.m4v)|*.avi;*.mp4' +
      ';*.mkv;*.f4v;*.mov;*.mpg;*.m4v'
    Left = 376
    Top = 154
  end
  object OPD: TOpenPictureDialog
    Left = 442
    Top = 154
  end
  object OD: TOpenDialog
    Filter = 'Audio files (*.wav, *.mp3, *.aac)|*.wav;*.mp3;.aac|Any file|*.*'
    Left = 412
    Top = 152
  end
  object OVD: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Video Files (*.avi *.mp4 *.mkv*.mpg*.wmv)'
        FileMask = '*.avi;*.mp4;*.mkv;*.mpg;*.wmv'
      end
      item
        DisplayName = 'Any File'
        FileMask = '*.*'
      end>
    Options = [fdoPathMustExist, fdoFileMustExist]
    Left = 496
    Top = 156
  end
end
