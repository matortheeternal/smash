object MiniPluginSelectionForm: TMiniPluginSelectionForm
  Left = 0
  Top = 0
  Caption = 'Plugin selection'
  ClientHeight = 424
  ClientWidth = 334
  Color = clBtnFace
  Constraints.MaxHeight = 1500
  Constraints.MaxWidth = 700
  Constraints.MinHeight = 400
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblPrompt: TLabel
    Left = 8
    Top = 8
    Width = 311
    Height = 26
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Select the plugins you want to base the setting tree off of.  Th' +
      'is  should be the plugins you plan to use this setting with. '
    WordWrap = True
  end
  object CheckListBox: TCheckListBox
    Left = 8
    Top = 45
    Width = 318
    Height = 335
    Margins.Top = 8
    Margins.Bottom = 8
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    PopupMenu = CheckListPopupMenu
    TabOrder = 0
    OnClick = CheckListBoxClick
    OnKeyUp = CheckListBoxKeyUp
  end
  object btnOK: TButton
    Left = 170
    Top = 391
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 251
    Top = 391
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object CheckListPopupMenu: TPopupMenu
    Left = 64
    Top = 64
    object SelectAllItem: TMenuItem
      Caption = 'Select all'
      OnClick = SelectAllItemClick
    end
    object SelectNoneItem: TMenuItem
      Caption = 'Select none'
      OnClick = SelectNoneItemClick
    end
    object InvertSelectionItem: TMenuItem
      Caption = 'Invert selection'
      OnClick = InvertSelectionItemClick
    end
  end
end
