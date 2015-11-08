object PluginSelectionForm: TPluginSelectionForm
  Left = 0
  Top = 0
  Caption = 'Plugin selection'
  ClientHeight = 453
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
    Height = 258
    Margins.Top = 8
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
    Top = 420
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = btnOKClick
    ExplicitTop = 352
  end
  object btnCancel: TButton
    Left = 251
    Top = 420
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitTop = 352
  end
  object gbOptions: TGroupBox
    Left = 8
    Top = 309
    Width = 318
    Height = 100
    Caption = 'Options'
    TabOrder = 3
    object lblRecords: TLabel
      Left = 12
      Top = 72
      Width = 39
      Height = 13
      Margins.Left = 8
      Margins.Top = 8
      Align = alCustom
      Caption = 'Records'
    end
    object kbOverridesOnly: TCheckBox
      Left = 12
      Top = 21
      Width = 298
      Height = 17
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alCustom
      Anchors = [akLeft, akRight]
      Caption = 'Build tree from override records only'
      TabOrder = 0
    end
    object rbSkip: TRadioButton
      Left = 162
      Top = 46
      Width = 144
      Height = 17
      Margins.Right = 12
      Align = alCustom
      Caption = 'Skip records'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = rbSkipClick
    end
    object rbTarget: TRadioButton
      Left = 12
      Top = 44
      Width = 144
      Height = 17
      Margins.Left = 12
      Align = alCustom
      Caption = 'Target records'
      TabOrder = 2
      OnClick = rbTargetClick
    end
    object edRecords: TEdit
      Left = 104
      Top = 69
      Width = 206
      Height = 21
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
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
