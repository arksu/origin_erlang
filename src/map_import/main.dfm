object main_form: Tmain_form
  Left = 0
  Top = 0
  Caption = 'a1 map importer v 0.1'
  ClientHeight = 634
  ClientWidth = 930
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    930
    634)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 9
    Top = 8
    Width = 105
    Height = 25
    Caption = 'open tiles png'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 8
    Width = 105
    Height = 25
    Caption = 'open obj png'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 304
    Top = 8
    Width = 105
    Height = 25
    Caption = 'start'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 914
    Height = 587
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
  object od: TOpenDialog
    Left = 248
    Top = 8
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 200
    OnTimer = Timer1Timer
    Left = 432
    Top = 8
  end
end
