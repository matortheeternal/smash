object OptionsForm: TOptionsForm
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 446
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MaxHeight = 485
  Constraints.MaxWidth = 600
  Constraints.MinHeight = 485
  Constraints.MinWidth = 600
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
  object SettingsPageControl: TPageControl
    Left = 8
    Top = 8
    Width = 568
    Height = 401
    ActivePage = GeneralTabSheet
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    TabWidth = 80
    object GeneralTabSheet: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbStyle: TGroupBox
        Left = 6
        Top = 69
        Width = 548
        Height = 71
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alCustom
        Caption = 'Style'
        TabOrder = 0
        object kbSimpleDictionary: TCheckBox
          Left = 12
          Top = 20
          Width = 205
          Height = 17
          Align = alCustom
          Caption = 'Simple dictionary view'
          TabOrder = 0
        end
        object kbSimplePlugins: TCheckBox
          Left = 12
          Top = 43
          Width = 205
          Height = 17
          Align = alCustom
          Caption = 'Simple plugins list'
          TabOrder = 1
        end
      end
      object gbLanguage: TGroupBox
        Left = 6
        Top = 6
        Width = 548
        Height = 51
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alCustom
        Caption = 'Language'
        TabOrder = 1
        object lblLanguage: TLabel
          Left = 12
          Top = 20
          Width = 78
          Height = 13
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Align = alCustom
          Caption = 'Current languge'
        end
        object cbLanguage: TComboBox
          Left = 265
          Top = 17
          Width = 274
          Height = 21
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Align = alCustom
          Style = csDropDownList
          Anchors = [akTop, akRight]
          TabOrder = 0
        end
      end
    end
    object PatchingTabSheet: TTabSheet
      Caption = 'Patching'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 28
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbGeneral: TGroupBox
        Left = 3
        Top = 3
        Width = 554
        Height = 54
        Caption = 'General'
        TabOrder = 0
        object lblDestinationDirectory: TLabel
          Left = 12
          Top = 20
          Width = 129
          Height = 13
          Caption = 'Patch destination directory'
        end
        object btnBrowsePatchDirectory: TSpeedButton
          Left = 519
          Top = 17
          Width = 23
          Height = 22
          Hint = 'Browse'
          Margins.Right = 6
          ParentShowHint = False
          ShowHint = True
          OnClick = btnBrowsePatchDirectoryClick
        end
        object edPatchDirectory: TEdit
          Left = 168
          Top = 17
          Width = 345
          Height = 21
          TabOrder = 0
        end
      end
      object gbDebug: TGroupBox
        Left = 3
        Top = 63
        Width = 554
        Height = 146
        Caption = 'Debug'
        TabOrder = 1
        object kbDebugPatchStatus: TCheckBox
          Left = 12
          Top = 24
          Width = 200
          Height = 17
          Align = alCustom
          Caption = 'Debug patch status'
          TabOrder = 0
        end
        object kbDebugMasters: TCheckBox
          Left = 12
          Top = 47
          Width = 200
          Height = 17
          Align = alCustom
          Caption = 'Debug masters'
          TabOrder = 1
        end
        object kbDebugArrays: TCheckBox
          Left = 12
          Top = 70
          Width = 200
          Height = 17
          Align = alCustom
          Caption = 'Debug arrays'
          TabOrder = 2
        end
        object kbDebugSkips: TCheckBox
          Left = 12
          Top = 93
          Width = 200
          Height = 17
          Align = alCustom
          Caption = 'Debug skips'
          TabOrder = 3
        end
        object kbDebugTraversal: TCheckBox
          Left = 12
          Top = 116
          Width = 200
          Height = 17
          Align = alCustom
          Caption = 'Debug traversal'
          TabOrder = 4
        end
        object kbDebugTypes: TCheckBox
          Left = 273
          Top = 24
          Width = 200
          Height = 17
          Align = alCustom
          Caption = 'Debug types'
          TabOrder = 5
        end
        object kbDebugChanges: TCheckBox
          Left = 272
          Top = 47
          Width = 200
          Height = 17
          Align = alCustom
          Caption = 'Debug changes'
          TabOrder = 6
        end
        object kbDebugSingle: TCheckBox
          Left = 272
          Top = 72
          Width = 200
          Height = 17
          Align = alCustom
          Caption = 'Debug treat as single'
          TabOrder = 7
        end
        object kbDebugLinks: TCheckBox
          Left = 272
          Top = 95
          Width = 200
          Height = 17
          Align = alCustom
          Caption = 'Debug links'
          TabOrder = 8
        end
      end
      object gbOther: TGroupBox
        Left = 3
        Top = 215
        Width = 554
        Height = 58
        Caption = 'Other Options'
        TabOrder = 2
        object kbBuildRefs: TCheckBox
          Left = 12
          Top = 22
          Width = 201
          Height = 17
          Align = alCustom
          Caption = 'Build references'
          TabOrder = 0
        end
        object kbPreserveITPOs: TCheckBox
          Left = 273
          Top = 22
          Width = 201
          Height = 17
          Align = alCustom
          Caption = 'Preserve ITPOs'
          TabOrder = 1
        end
      end
    end
    object AdvancedTabSheet: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbLogging: TGroupBox
        Left = 6
        Top = 72
        Width = 548
        Height = 190
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Logging'
        TabOrder = 0
        object lblClientColor: TLabel
          Left = 12
          Top = 20
          Width = 27
          Height = 13
          Caption = 'Client'
        end
        object lblGeneralColor: TLabel
          Left = 12
          Top = 48
          Width = 37
          Height = 13
          Margins.Left = 12
          Margins.Top = 11
          Caption = 'General'
        end
        object lblLoadColor: TLabel
          Left = 12
          Top = 76
          Width = 23
          Height = 13
          Margins.Left = 12
          Margins.Top = 11
          Caption = 'Load'
        end
        object lblPatchColor: TLabel
          Left = 274
          Top = 20
          Width = 27
          Height = 13
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = 'Patch'
        end
        object lblPluginColor: TLabel
          Left = 274
          Top = 48
          Width = 28
          Height = 13
          Margins.Top = 11
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = 'Plugin'
        end
        object lblErrorColor: TLabel
          Left = 274
          Top = 76
          Width = 29
          Height = 13
          Margins.Top = 11
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = 'Errors'
        end
        object lblSample: TLabel
          Left = 12
          Top = 155
          Width = 34
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'Sample'
        end
        object lblSampleValue: TLabel
          Left = 88
          Top = 155
          Width = 232
          Height = 13
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = '[12:34] (GENERAL) Test: This is a test message.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblTemplate: TLabel
          Left = 12
          Top = 109
          Width = 44
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'Template'
        end
        object cbClientColor: TColorBox
          Left = 88
          Top = 17
          Width = 145
          Height = 22
          Selected = clBlue
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 0
        end
        object cbGeneralColor: TColorBox
          Left = 88
          Top = 45
          Width = 145
          Height = 22
          Selected = clGreen
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 1
        end
        object cbLoadColor: TColorBox
          Left = 88
          Top = 73
          Width = 145
          Height = 22
          Selected = clPurple
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 2
        end
        object cbPatchColor: TColorBox
          Left = 392
          Top = 17
          Width = 145
          Height = 22
          Align = alCustom
          Selected = 33023
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          Anchors = [akTop, akRight]
          TabOrder = 3
        end
        object cbPluginColor: TColorBox
          Left = 392
          Top = 45
          Width = 145
          Height = 22
          Align = alCustom
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          Anchors = [akTop, akRight]
          TabOrder = 4
        end
        object cbErrorColor: TColorBox
          Left = 392
          Top = 73
          Width = 145
          Height = 22
          Align = alCustom
          Selected = clRed
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          Anchors = [akTop, akRight]
          TabOrder = 5
        end
        object meTemplate: TMemo
          Left = 88
          Top = 109
          Width = 449
          Height = 37
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          Lines.Strings = (
            '[{{Time}}] ({{Group}}) {{Label}}: {{Text}}')
          ScrollBars = ssVertical
          TabOrder = 6
          OnChange = meTemplateChange
        end
      end
      object gbSmashProfile: TGroupBox
        Left = 6
        Top = 6
        Width = 548
        Height = 54
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Smash Profile'
        TabOrder = 1
        object lblCurrentProfile: TLabel
          Left = 12
          Top = 24
          Width = 70
          Height = 13
          Caption = 'Current profile'
        end
        object lblCurrentProfileName: TLabel
          Left = 151
          Top = 24
          Width = 89
          Height = 13
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = '<PROFILE NAME>'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object btnChangeSmashProfile: TButton
          Left = 392
          Top = 18
          Width = 150
          Height = 25
          Caption = 'Switch smash profiles'
          TabOrder = 0
          OnClick = btnChangeSmashProfileClick
        end
      end
    end
  end
  object btnCancel: TButton
    Left = 501
    Top = 415
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 420
    Top = 415
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object IconList: TImageList
    Left = 16
    Top = 400
    Bitmap = {
      494C010101000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000075848FFF66808FFF607987FF576E
      7BFF4E626FFF445661FF394852FF2E3A43FF252E35FF1B2229FF14191EFF0E12
      16FF0E1318FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000778792FF89A1ABFF6AB2D4FF008F
      CDFF008FCDFF008FCDFF048CC7FF0888BEFF0F82B4FF157CA9FF1B779FFF1F72
      96FF214A5BFEBDC2C44A00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007A8A95FF7EBED3FF8AA4AEFF7EDC
      FFFF5FCFFFFF55CBFFFF4CC4FAFF41BCF5FF37B3F0FF2EAAEBFF24A0E5FF138C
      D4FF236780FF5E686CB400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007D8E98FF79D2ECFF8BA4ADFF89C2
      CEFF71D8FFFF65D3FFFF5CCEFFFF51C9FEFF49C1FAFF3FB9F5FF34B0EEFF29A8
      E9FF1085CDFF224B5BFFDADDDF27000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080919CFF81D7EFFF7DC5E0FF8CA6
      B0FF80DDFEFF68D3FFFF67D4FFFF62D1FFFF58CDFFFF4EC7FCFF46BEF7FF3BB6
      F2FF31ACECFF256981FF7A95A190000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000083959FFF89DCF1FF8CE2FFFF8DA8
      B1FF8CBAC7FF74D8FFFF67D4FFFF67D4FFFF67D4FFFF5FD0FFFF54CDFFFF4BC5
      FCFF41BBF7FF2EA2DBFF516674F1E1E4E62B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000869AA3FF92E1F2FF98E8FDFF80C4
      DEFF8EA7B0FF81DEFDFF84E0FFFF84E0FFFF84E0FFFF84E0FFFF81DFFFFF7BDD
      FFFF74D8FFFF6BD6FFFF56A9D1FF8E9BA3A20000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000889CA5FF9AE6F3FF9FEBFBFF98E8
      FEFF8BACB9FF8BACB9FF8AAAB7FF88A6B3FF86A3AFFF839FAAFF819AA6FF7F95
      A1FF7C919DFF7A8E99FF798B95FF778893FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008BA0A8FFA0EAF6FFA6EEF9FF9FEB
      FBFF98E8FEFF7ADAFFFF67D4FFFF67D4FFFF67D4FFFF67D4FFFF67D4FFFF67D4
      FFFF778893FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008EA2ABFFA7EEF6FFABF0F7FFA6EE
      F9FF9FEBFBFF98E8FDFF71D4FBFF899EA7FF8699A3FF82949FFF7E909AFF7A8C
      97FF778893FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008FA4ACFFA0D2DAFFABF0F7FFABF0
      F7FFA6EEF9FF9FEBFBFF8DA1AAFFC0D0D6820000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D8DFE2578FA4ACFF8FA4ACFF8FA4
      ACFF8FA4ACFF8FA4ACFFBDCFD68D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000FFFF000000000000
      0007000000000000000300000000000000030000000000000001000000000000
      0001000000000000000000000000000000000000000000000000000000000000
      0007000000000000000700000000000000FF00000000000001FF000000000000
      FFFF000000000000FFFF00000000000000000000000000000000000000000000
      000000000000}
  end
end
