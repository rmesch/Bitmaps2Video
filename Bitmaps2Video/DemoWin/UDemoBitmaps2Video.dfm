object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Make a movie by zooming into a picture'
  ClientHeight = 297
  ClientWidth = 505
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
  object Image1: TImage
    Left = 18
    Top = 73
    Width = 165
    Height = 111
    Proportional = True
    Stretch = True
  end
  object Label1: TLabel
    Left = 18
    Top = 4
    Width = 84
    Height = 13
    Caption = 'Output file name:'
  end
  object Label2: TLabel
    Left = 412
    Top = 51
    Width = 78
    Height = 13
    Caption = 'Encoding quality'
  end
  object Label3: TLabel
    Left = 230
    Top = 52
    Width = 59
    Height = 13
    Caption = 'Video height'
  end
  object Label4: TLabel
    Left = 18
    Top = 228
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 18
    Top = 278
    Width = 31
    Height = 13
    Caption = 'Label5'
  end
  object Label6: TLabel
    Left = 324
    Top = 102
    Width = 39
    Height = 13
    Caption = 'Encoder'
  end
  object Label7: TLabel
    Left = 324
    Top = 52
    Width = 82
    Height = 13
    Caption = 'Frame rate [FPS]'
  end
  object Label8: TLabel
    Left = 232
    Top = 102
    Width = 69
    Height = 13
    Caption = 'Output format'
  end
  object Label9: TLabel
    Left = 212
    Top = 221
    Width = 31
    Height = 13
    Caption = 'Label9'
  end
  object Edit1: TEdit
    Left = 18
    Top = 18
    Width = 350
    Height = 21
    TabOrder = 0
    Text = 'Output.avi'
  end
  object Button1: TButton
    Left = 18
    Top = 190
    Width = 75
    Height = 25
    Caption = 'Make Movie'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 370
    Top = 16
    Width = 27
    Height = 25
    Caption = '...'
    TabOrder = 2
    OnClick = Button2Click
  end
  object SpinEdit1: TSpinEdit
    Left = 412
    Top = 70
    Width = 71
    Height = 22
    MaxValue = 120
    MinValue = 0
    TabOrder = 3
    Value = 70
    OnChange = SpinEdit1Change
  end
  object HC: TComboBox
    Left = 228
    Top = 71
    Width = 85
    Height = 21
    Style = csDropDownList
    ItemIndex = 3
    TabOrder = 4
    Text = '720'
    OnChange = HCChange
    Items.Strings = (
      '360'
      '480'
      '600'
      '720'
      '1080')
  end
  object rgrpZoom: TRadioGroup
    Left = 274
    Top = 148
    Width = 223
    Height = 99
    Caption = 'Zooming done by'
    ItemIndex = 2
    Items.Strings = (
      'Antialias x2'
      'Antialias x4'
      'Antialias x6'
      '"Bicubic" Resample')
    TabOrder = 5
  end
  object ProgressBar1: TProgressBar
    Left = 104
    Top = 196
    Width = 139
    Height = 19
    Max = 29000
    MarqueeInterval = 100
    TabOrder = 6
  end
  object Button3: TButton
    Left = 18
    Top = 42
    Width = 141
    Height = 25
    Caption = 'Load picture'
    TabOrder = 7
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 18
    Top = 247
    Width = 103
    Height = 25
    Caption = 'Add audio'
    TabOrder = 8
    OnClick = Button4Click
  end
  object CodecCombo: TComboBox
    Left = 324
    Top = 121
    Width = 173
    Height = 21
    Style = csDropDownList
    TabOrder = 9
    OnChange = CodecComboChange
  end
  object RateCombo: TComboBox
    Left = 324
    Top = 71
    Width = 53
    Height = 21
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 10
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
    Left = 230
    Top = 121
    Width = 83
    Height = 21
    Style = csDropDownList
    TabOrder = 11
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
    Left = 274
    Top = 256
    Width = 223
    Height = 25
    Caption = 'Register raw video as new codec (.avi)'
    TabOrder = 12
    OnClick = Button5Click
  end
  object SD: TSaveDialog
    Filter = 
      'Videos (*.avi, *.mp4, *.mkv,*.f4v,*.mov,*.mpg,*.m4v)|*.avi;*.mp4' +
      ';*.mkv;*.f4v;*.mov;*.mpg;*.m4v'
    Left = 408
    Top = 18
  end
  object OPD: TOpenPictureDialog
    Left = 174
    Top = 38
  end
  object OD: TOpenDialog
    Filter = 'Audio files (*.wav, *.mp3, *.aac)|*.wav;*.mp3;.aac'
    Left = 66
    Top = 272
  end
end
