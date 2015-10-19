object ProgressForm: TProgressForm
  Left = 0
  Top = 0
  Caption = 'Progress'
  ClientHeight = 342
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressLabel: TLabel
    Left = 8
    Top = 8
    Width = 608
    Height = 13
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Progress message'
  end
  object DetailsMemo: TMemo
    Left = 8
    Top = 58
    Width = 608
    Height = 245
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object DetailsButton: TButton
    Left = 428
    Top = 309
    Width = 91
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Hide details'
    TabOrder = 1
    OnClick = ToggleDetails
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 27
    Width = 608
    Height = 25
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 525
    Top = 309
    Width = 91
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = CancelButtonClick
  end
end
