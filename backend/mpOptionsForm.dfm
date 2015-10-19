object OptionsForm: TOptionsForm
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 447
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MaxHeight = 600
  Constraints.MaxWidth = 800
  Constraints.MinHeight = 480
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
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
    TabWidth = 70
    object GeneralTabSheet: TTabSheet
      Caption = 'General'
      object gbStatus: TGroupBox
        Left = 6
        Top = 6
        Width = 548
        Height = 107
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Status'
        TabOrder = 0
        object lblVersion: TLabel
          Left = 12
          Top = 20
          Width = 108
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'Client Program Version'
        end
        object lblTES5Hash: TLabel
          Left = 12
          Top = 53
          Width = 102
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'TES5 Dictionary Hash'
        end
        object lblTES4Hash: TLabel
          Left = 12
          Top = 78
          Width = 102
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'TES4 Dictionary Hash'
        end
        object lblTES4HashValue: TLabel
          Left = 190
          Top = 78
          Width = 63
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = '$01234567'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblFNVHash: TLabel
          Left = 295
          Top = 53
          Width = 97
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = 'FNV Dictionary Hash'
        end
        object lblFNVHashValue: TLabel
          Left = 473
          Top = 53
          Width = 63
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 12
          Margins.Bottom = 6
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = '$01234567'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblFO3Hash: TLabel
          Left = 295
          Top = 78
          Width = 98
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = 'FO3 Dictionary Hash'
        end
        object lblFO3HashValue: TLabel
          Left = 473
          Top = 78
          Width = 63
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 12
          Margins.Bottom = 6
          Caption = '$01234567'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblVersionValue: TLabel
          Left = 190
          Top = 20
          Width = 37
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = '2.0.0.0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblTES5HashValue: TLabel
          Left = 190
          Top = 53
          Width = 63
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = '$01234567'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object btnUpdateStatus: TButton
          Left = 436
          Top = 16
          Width = 106
          Height = 25
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = 'Update Status'
          TabOrder = 0
          OnClick = btnUpdateStatusClick
        end
      end
      object gbColoring: TGroupBox
        Left = 6
        Top = 235
        Width = 548
        Height = 107
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Log Coloring'
        TabOrder = 1
        object lblServerColor: TLabel
          Left = 12
          Top = 20
          Width = 32
          Height = 13
          Caption = 'Server'
        end
        object lblInitColor: TLabel
          Left = 12
          Top = 47
          Width = 16
          Height = 13
          Margins.Left = 12
          Margins.Top = 11
          Caption = 'Init'
        end
        object lblSQLColor: TLabel
          Left = 12
          Top = 74
          Width = 19
          Height = 13
          Margins.Left = 12
          Margins.Top = 11
          Caption = 'SQL'
        end
        object lblDataColor: TLabel
          Left = 274
          Top = 20
          Width = 23
          Height = 13
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = 'Data'
        end
        object lblTaskColor: TLabel
          Left = 274
          Top = 47
          Width = 27
          Height = 13
          Margins.Top = 11
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = 'Tasks'
        end
        object lblErrorColor: TLabel
          Left = 274
          Top = 74
          Width = 29
          Height = 13
          Margins.Top = 11
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = 'Errors'
        end
        object cbServerColor: TColorBox
          Left = 88
          Top = 17
          Width = 145
          Height = 22
          Selected = clBlue
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 0
        end
        object cbInitColor: TColorBox
          Left = 88
          Top = 45
          Width = 145
          Height = 22
          Selected = clGreen
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 1
        end
        object cbSQLColor: TColorBox
          Left = 88
          Top = 73
          Width = 145
          Height = 22
          Selected = clPurple
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
          TabOrder = 2
        end
        object cbDataColor: TColorBox
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
        object cbTaskColor: TColorBox
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
      end
      object gbStyle: TGroupBox
        Left = 6
        Top = 125
        Width = 548
        Height = 98
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Style'
        TabOrder = 2
        object kbSimpleReports: TCheckBox
          Left = 12
          Top = 20
          Width = 117
          Height = 17
          Caption = 'Simple reports list'
          TabOrder = 0
        end
        object kbSimpleLog: TCheckBox
          Left = 12
          Top = 43
          Width = 97
          Height = 17
          Caption = 'Simple log view'
          TabOrder = 1
        end
        object kbSimpleDictionary: TCheckBox
          Left = 12
          Top = 66
          Width = 125
          Height = 17
          Caption = 'Simple dictionary view'
          TabOrder = 2
        end
      end
    end
    object Dictionary: TTabSheet
      Caption = 'Dictionary'
      ImageIndex = 1
      object gbConsolidation: TGroupBox
        Left = 6
        Top = 6
        Width = 548
        Height = 99
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Report consolidation'
        TabOrder = 0
        object kbSeparateHashes: TCheckBox
          Left = 12
          Top = 20
          Width = 197
          Height = 17
          Caption = 'Separate entries based on hashes'
          TabOrder = 0
        end
        object kbSeparateVersions: TCheckBox
          Left = 12
          Top = 43
          Width = 197
          Height = 17
          Caption = 'Separate entries based on versions'
          TabOrder = 1
        end
        object kbSeparateRecords: TCheckBox
          Left = 12
          Top = 66
          Width = 253
          Height = 17
          Caption = 'Separate entries based on number of records'
          TabOrder = 2
        end
      end
      object gbNotesformat: TGroupBox
        Left = 6
        Top = 117
        Width = 548
        Height = 180
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Entry notes'
        TabOrder = 1
        object lblSampleValueHash: TLabel
          Left = 114
          Top = 66
          Width = 243
          Height = 13
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = '[Username: Test] [Hash: 01234567] [Version: 2.0]'
        end
        object lblTemplateHash: TLabel
          Left = 12
          Top = 20
          Width = 93
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'Template with hash'
        end
        object lblSample: TLabel
          Left = 12
          Top = 66
          Width = 34
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'Sample'
        end
        object lblSampleValueNoHash: TLabel
          Left = 114
          Top = 146
          Width = 153
          Height = 13
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = '[Username: Test] [Version: 2.0]'
        end
        object lblSampleNoHash: TLabel
          Left = 12
          Top = 146
          Width = 34
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'Sample'
        end
        object lblTemplateNoHash: TLabel
          Left = 12
          Top = 100
          Width = 85
          Height = 13
          Margins.Left = 12
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'Template no hash'
        end
        object meTemplateHash: TMemo
          Left = 114
          Top = 20
          Width = 423
          Height = 37
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          Lines.Strings = (
            '[Username: {{user}}] [Hash: {{hash}}] [Version: {{version}}]')
          ScrollBars = ssVertical
          TabOrder = 0
          OnChange = meTemplateHashChange
        end
        object meTemplateNoHash: TMemo
          Left = 114
          Top = 100
          Width = 423
          Height = 37
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          Lines.Strings = (
            '[Username: {{user}}] [Version: {{version}}]')
          ScrollBars = ssVertical
          TabOrder = 1
          OnChange = meTemplateNoHashChange
        end
      end
    end
    object UsersTabsheet: TTabSheet
      Caption = 'Users'
      ImageIndex = 2
      object gbBlacklist: TGroupBox
        Left = 6
        Top = 6
        Width = 548
        Height = 169
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Blacklist'
        TabOrder = 0
        object lvBlacklist: TListView
          Left = 12
          Top = 20
          Width = 524
          Height = 133
          Margins.Left = 12
          Margins.Top = 20
          Margins.Right = 12
          Margins.Bottom = 6
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          Columns = <
            item
              Caption = 'IP Address'
              Width = 120
            end
            item
              AutoSize = True
              Caption = 'Username'
            end
            item
              Caption = 'Created'
              Width = 120
            end
            item
              Caption = 'Expires'
              Width = 120
            end>
          DoubleBuffered = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MultiSelect = True
          OwnerData = True
          ReadOnly = True
          RowSelect = True
          ParentDoubleBuffered = False
          ParentFont = False
          PopupMenu = pmBlacklist
          TabOrder = 0
          ViewStyle = vsReport
          OnData = lvBlacklistData
        end
      end
      object gbUsers: TGroupBox
        Left = 6
        Top = 187
        Width = 548
        Height = 167
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Users'
        TabOrder = 1
        object lvUsers: TListView
          Left = 12
          Top = 20
          Width = 524
          Height = 133
          Margins.Left = 12
          Margins.Top = 20
          Margins.Right = 12
          Margins.Bottom = 6
          Align = alCustom
          Anchors = [akLeft, akTop, akRight, akBottom]
          Columns = <
            item
              Caption = 'IP Address'
              Width = 120
            end
            item
              AutoSize = True
              Caption = 'Username'
            end
            item
              Caption = 'First seen'
              Width = 120
            end
            item
              Caption = 'Last seen'
              Width = 120
            end>
          DoubleBuffered = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MultiSelect = True
          OwnerData = True
          ReadOnly = True
          RowSelect = True
          ParentDoubleBuffered = False
          ParentFont = False
          PopupMenu = pmUsers
          TabOrder = 0
          ViewStyle = vsReport
          OnData = lvUsersData
          OnDblClick = lvUsersDblClick
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
      494C010101000800880010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
  object pmBlacklist: TPopupMenu
    OnPopup = pmBlacklistPopup
    Left = 72
    Top = 400
    object UnblacklistItem: TMenuItem
      Caption = 'Unblacklist'
      OnClick = UnblacklistItemClick
    end
    object ChangeExpirationItem: TMenuItem
      Caption = 'Change expiration date'
    end
  end
  object pmUsers: TPopupMenu
    OnPopup = pmUsersPopup
    Left = 136
    Top = 400
    object ViewUserItem: TMenuItem
      Caption = 'View user details'
      OnClick = ViewUserItemClick
    end
    object BlacklistUserItem: TMenuItem
      Caption = 'Blacklist user'
      OnClick = BlacklistUserItemClick
    end
    object DeleteUserItem: TMenuItem
      Caption = 'Delete user'
      OnClick = DeleteUserItemClick
    end
  end
end
