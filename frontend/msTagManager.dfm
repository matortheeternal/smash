object TagManager: TTagManager
  Left = 0
  Top = 0
  Caption = 'Manage Tags'
  ClientHeight = 312
  ClientWidth = 434
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
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 434
    Height = 273
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object lblDescription: TLabel
      Left = 8
      Top = 8
      Width = 53
      Height = 13
      Align = alCustom
      Caption = 'Description'
    end
    object meDescription: TMemo
      Left = 8
      Top = 27
      Width = 321
      Height = 238
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
    end
    object btnClear: TButton
      Left = 335
      Top = 87
      Width = 90
      Height = 25
      Align = alCustom
      Anchors = [akTop, akRight]
      Caption = 'Clear Tags'
      TabOrder = 1
      OnClick = btnClearClick
    end
    object btnRemove: TButton
      Left = 335
      Top = 56
      Width = 90
      Height = 25
      Align = alCustom
      Anchors = [akTop, akRight]
      Caption = 'Remove Tags'
      TabOrder = 2
      OnClick = btnRemoveClick
    end
    object btnAdd: TButton
      Left = 335
      Top = 25
      Width = 90
      Height = 25
      Align = alCustom
      Anchors = [akTop, akRight]
      Caption = 'Add Tags'
      TabOrder = 3
      OnClick = btnAddClick
    end
    object btnReset: TButton
      Left = 335
      Top = 118
      Width = 90
      Height = 25
      Align = alCustom
      Anchors = [akTop, akRight]
      Caption = 'Reset Tags'
      TabOrder = 4
      OnClick = btnResetClick
    end
  end
  object btnCancel: TButton
    Left = 346
    Top = 279
    Width = 80
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnApply: TButton
    Left = 260
    Top = 279
    Width = 80
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    ModalResult = 1
    TabOrder = 2
  end
  object kbCombine: TCheckBox
    Left = 8
    Top = 283
    Width = 201
    Height = 17
    Align = alCustom
    Anchors = [akLeft, akBottom]
    Caption = 'Apply combined setting'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
