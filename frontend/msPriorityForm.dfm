object PriorityForm: TPriorityForm
  Left = 0
  Top = 0
  Caption = 'Change node priority'
  ClientHeight = 134
  ClientWidth = 300
  Color = clBtnFace
  Constraints.MaxHeight = 172
  Constraints.MaxWidth = 316
  Constraints.MinHeight = 172
  Constraints.MinWidth = 316
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblPriority: TLabel
    Left = 24
    Top = 67
    Width = 34
    Height = 13
    Caption = 'Priority'
  end
  object lblPrompt: TLabel
    Left = 8
    Top = 8
    Width = 284
    Height = 39
    Caption = 
      'Change a node'#39's priority to adjust how it conflict resolves with' +
      ' other nodes.  Values from higher priority nodes will be favored' +
      ' over values from lower priority nodes.'
    WordWrap = True
  end
  object btnCancel: TButton
    Left = 217
    Top = 101
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 136
    Top = 101
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = btnOKClick
  end
  object edPriority: TEdit
    Left = 88
    Top = 64
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 2
    Text = '0'
  end
end
