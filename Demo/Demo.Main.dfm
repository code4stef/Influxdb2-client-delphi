object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'InfluxDB2 Client test'
  ClientHeight = 404
  ClientWidth = 648
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 16
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 22
    Top = 43
    Width = 29
    Height = 13
    Caption = 'Token'
  end
  object Button1: TButton
    Left = 22
    Top = 80
    Width = 147
    Height = 25
    Caption = 'Client functions'
    TabOrder = 0
    OnClick = Button1Click
  end
  object edtServerURL: TEdit
    Left = 57
    Top = 13
    Width = 248
    Height = 21
    TabOrder = 1
    Text = 'http://192.168.1.245:8086'
  end
  object edtToken: TEdit
    Left = 57
    Top = 40
    Width = 248
    Height = 21
    TabOrder = 2
    Text = 
      'cFLcJSyJof8EQ7s-3ifR0NM6u3NsvSO9Q8m8vAgQwSxl4smePMRK2hgElShJ0_V2' +
      'AXLumvdf90U_3-FZ3jPdUw=='
  end
  object Memo1: TMemo
    Left = 22
    Top = 144
    Width = 603
    Height = 252
    Lines.Strings = (
      'Memo1')
    TabOrder = 3
  end
  object Button2: TButton
    Left = 256
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 4
  end
end
