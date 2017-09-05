object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS UI Gothic'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 12
  object PopupMenu1: TPopupMenu
    Left = 24
    Top = 32
    object A1: TMenuItem
      Caption = #21517#21069#12434#20184#12369#12390#20445#23384'(&A)'
      OnClick = A1Click
    end
  end
  object FileSaveDialog1: TFileSaveDialog
    DefaultExtension = 'png'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'PNG '#12501#12449#12452#12523
        FileMask = '*.png'
      end>
    Options = [fdoOverWritePrompt]
    Left = 108
    Top = 32
  end
end
