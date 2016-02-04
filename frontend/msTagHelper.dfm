object TagHelper: TTagHelper
  Left = 0
  Top = 0
  Caption = 'Tag Helper'
  ClientHeight = 412
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblPrompt: TLabel
    Left = 8
    Top = 8
    Width = 185
    Height = 13
    Caption = 'Check the tags you want to {ACTION}'
  end
  object CheckList: TCheckListBox
    Left = 8
    Top = 27
    Width = 318
    Height = 346
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 251
    Top = 379
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 170
    Top = 379
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
end
