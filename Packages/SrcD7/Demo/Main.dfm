object Form1: TForm1
  Left = 381
  Top = 148
  Width = 531
  Height = 341
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    515
    302)
  PixelsPerInch = 96
  TextHeight = 13
  object mDBGrid_1: TmDBGrid_
    Left = 8
    Top = 48
    Width = 497
    Height = 209
    EditSearcFound_Color = 9563793
    EditSearcNotFound_Color = 12366079
    EditSearcStartEmpty_Color = 15179959
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = ds_Test
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Pnl_StatusBar: TPanel
    Left = 0
    Top = 272
    Width = 515
    Height = 30
    Align = alBottom
    TabOrder = 1
  end
  object Btn_Connect: TButton
    Left = 8
    Top = 0
    Width = 137
    Height = 41
    Cursor = crHandPoint
    Caption = 'Connect'
    TabOrder = 3
    OnClick = Btn_ConnectClick
  end
  object DBNav_Test: TDBNavigator
    Left = 160
    Top = 8
    Width = 330
    Height = 33
    DataSource = ds_Test
    TabOrder = 2
  end
  object ds_Test: TDataSource
    DataSet = Qry_User
    Left = 456
    Top = 128
  end
  object Qry_User: TADOQuery
    Connection = ADOCon_1
    Parameters = <>
    SQL.Strings = (
      'SELECT * FROM TUser '
      'ORDER BY ID')
    Left = 400
    Top = 128
  end
  object ADOCon_1: TADOConnection
    LoginPrompt = False
    Left = 424
    Top = 72
  end
end
