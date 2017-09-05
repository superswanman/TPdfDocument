object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 606
  ClientWidth = 883
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS UI Gothic'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 883
    Height = 41
    Align = alTop
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 608
    object Label1: TLabel
      Left = 86
      Top = 13
      Width = 26
      Height = 12
      Caption = '0 / 0'
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 33
      Height = 25
      Caption = #8593
      Enabled = False
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 47
      Top = 8
      Width = 33
      Height = 25
      Caption = #8595
      Enabled = False
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 883
    Height = 565
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    ExplicitLeft = 256
    ExplicitTop = 216
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 883
      Height = 565
      Align = alClient
      Center = True
      Proportional = True
      ExplicitLeft = 124
      ExplicitTop = 168
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
  end
  object MainMenu1: TMainMenu
    Left = 92
    Top = 80
    object F1: TMenuItem
      Caption = #12501#12449#12452#12523'(&F)'
      object O1: TMenuItem
        Caption = #38283#12367'(&O)...'
        OnClick = O1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object A1: TMenuItem
        Caption = #30011#20687#12392#12375#12390#20445#23384'(&A)...'
        OnClick = A1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'pdf'
    Filter = 'Adobe PDF '#12501#12449#12452#12523'|*.pdf'
    Left = 192
    Top = 44
  end
  object SaveDialog1: TSaveDialog
    Filter = 'PNG '#12501#12449#12452#12523'|*.png|JPEG '#12501#12449#12452#12523'|*.jpg'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 192
    Top = 108
  end
end
