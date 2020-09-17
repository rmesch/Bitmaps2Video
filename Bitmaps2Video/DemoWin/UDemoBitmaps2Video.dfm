object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Demo for TBitmapEncoder (Windows only)'
  ClientHeight = 403
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
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
  object Edit1: TEdit
    Left = 18
    Top = 23
    Width = 350
    Height = 21
    TabOrder = 0
    Text = 'Output.avi'
  end
  object Button2: TButton
    Left = 374
    Top = 20
    Width = 27
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = Button2Click
  end
  object SpinEdit1: TSpinEdit
    Left = 223
    Top = 70
    Width = 71
    Height = 22
    MaxValue = 120
    MinValue = 0
    TabOrder = 2
    Value = 70
    OnChange = SpinEdit1Change
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
      '480'
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
    Left = 17
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
  object Button5: TButton
    Left = 17
    Top = 148
    Width = 223
    Height = 25
    Caption = 'Register raw video as new codec (.avi)'
    TabOrder = 7
    OnClick = Button5Click
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 178
    Width = 548
    Height = 225
    ActivePage = TabSheet1
    Align = alBottom
    TabOrder = 8
    object TabSheet1: TTabSheet
      Caption = 'Zooming'
      object Image1: TImage
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
        Top = 128
        Width = 540
        Height = 69
        Align = alBottom
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'Make a movie by zooming into the picture displayed. The result w' +
          'ill be stored in the output file chosen and the encoder properti' +
          'es chosen will be used. This is a demo for the methods AddFrame,' +
          ' ZoomPan, AddStillImage and Freeze of TBitmapEncoder. If the pic' +
          'ture does not have the aspect ratio chosen, black borders will b' +
          'e added.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 5671452
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitTop = 127
        ExplicitWidth = 497
      end
      object Button3: TButton
        Left = 186
        Top = 97
        Width = 141
        Height = 25
        Caption = 'Load picture'
        TabOrder = 0
        OnClick = Button3Click
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
      object Button1: TButton
        Left = 340
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Make Movie'
        TabOrder = 2
        OnClick = Button1Click
      end
      object ProgressBar1: TProgressBar
        Left = 340
        Top = 56
        Width = 139
        Height = 19
        Max = 20000
        MarqueeInterval = 100
        TabOrder = 3
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Inserting an existing video'
      ImageIndex = 1
      object Label11: TLabel
        Left = 0
        Top = 98
        Width = 540
        Height = 99
        Align = alBottom
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'This demo shows how you can insert an existing video into your s' +
          'lideshow. The method to be used is TBitmapEncoder.AddVideo. When' +
          ' you push the button, load a video from your hard disk (any form' +
          'at that VLC-player can render should be Ok). Intro- and End- Sli' +
          'des will be added to the video and the result is stored in the o' +
          'utput file. The video will be resized (proportionality preserved' +
          ') and reencoded according to the settings. The frame rate will b' +
          'e adjusted, too, without (hopefully) introducing any speedup or ' +
          'slowdown.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 5671452
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 497
      end
      object Label12: TLabel
        Left = 194
        Top = 46
        Width = 15
        Height = 13
        Caption = '     '
      end
      object Button7: TButton
        Left = 14
        Top = 40
        Width = 157
        Height = 25
        Caption = 'Load and insert video'
        TabOrder = 0
        OnClick = Button7Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Create from Video'
      ImageIndex = 2
      DesignSize = (
        540
        197)
      object Label13: TLabel
        Left = 0
        Top = 34
        Width = 537
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
      end
      object Label14: TLabel
        Left = 3
        Top = 123
        Width = 212
        Height = 66
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
        Left = 0
        Top = 3
        Width = 179
        Height = 25
        Caption = 'Load video and process further'
        TabOrder = 0
        OnClick = Button6Click
      end
      object Memo1: TMemo
        Left = 225
        Top = 119
        Width = 316
        Height = 77
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
      object Label5: TLabel
        Left = 20
        Top = 59
        Width = 21
        Height = 13
        Caption = '       '
      end
      object Label16: TLabel
        Left = 0
        Top = 98
        Width = 540
        Height = 99
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
        Font.Color = 5671452
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 497
      end
      object Button4: TButton
        Left = 14
        Top = 23
        Width = 175
        Height = 25
        Caption = 'Add audio to the output file'
        TabOrder = 0
        OnClick = Button4Click
      end
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 342
    Top = 57
    Width = 103
    Height = 78
    Caption = 'Output aspect ratio'
    ItemIndex = 1
    Items.Strings = (
      '16:9'
      '4:3'
      '3:2')
    TabOrder = 9
  end
  object SD: TSaveDialog
    Filter = 
      'Videos (*.avi, *.mp4, *.mkv,*.f4v,*.mov,*.mpg,*.m4v)|*.avi;*.mp4' +
      ';*.mkv;*.f4v;*.mov;*.mpg;*.m4v'
    Left = 408
    Top = 18
  end
  object OPD: TOpenPictureDialog
    Left = 442
    Top = 162
  end
  object OD: TOpenDialog
    Filter = 'Audio files (*.wav, *.mp3, *.aac)|*.wav;*.mp3;.aac'
    Left = 410
    Top = 162
  end
  object OVD: TOpenDialog
    Filter = 
      'Video Files (*.avi *.mp4 *.mkv*.mpg)|*.avi;*.mp4;*.mkv;*.mpg| An' +
      'y File|*.*'
    Left = 474
    Top = 162
  end
end
