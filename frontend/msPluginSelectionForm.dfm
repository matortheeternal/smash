object PluginSelectionForm: TPluginSelectionForm
  Left = 0
  Top = 0
  Caption = 'Plugin selection'
  ClientHeight = 385
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
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
    Top = 40
    Width = 318
    Height = 280
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnClick = CheckListBoxClick
    ExplicitHeight = 257
  end
  object btnOK: TButton
    Left = 170
    Top = 352
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = btnOKClick
    ExplicitTop = 329
  end
  object btnCancel: TButton
    Left = 251
    Top = 352
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitTop = 329
  end
  object kbOverridesOnly: TCheckBox
    Left = 8
    Top = 326
    Width = 318
    Height = 17
    Caption = 'Build tree from override records only'
    TabOrder = 3
  end
end
