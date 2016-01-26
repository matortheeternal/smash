object ChangeLogForm: TChangeLogForm
  Left = 0
  Top = 0
  Caption = 'Update Available'
  ClientHeight = 342
  ClientWidth = 366
  Color = clBtnFace
  Constraints.MaxHeight = 1000
  Constraints.MaxWidth = 382
  Constraints.MinHeight = 300
  Constraints.MinWidth = 382
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelPrompt: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Changelog'
    WordWrap = True
  end
  object ScrollBox: TScrollBox
    Left = 8
    Top = 27
    Width = 350
    Height = 276
    HorzScrollBar.Visible = False
    VertScrollBar.Tracking = True
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object ButtonInstall: TButton
    Left = 202
    Top = 309
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Install'
    ModalResult = 1
    TabOrder = 1
  end
  object ButtonSkip: TButton
    Left = 283
    Top = 309
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Skip'
    ModalResult = 2
    TabOrder = 2
  end
end
