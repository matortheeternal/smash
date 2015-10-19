object UserForm: TUserForm
  Left = 0
  Top = 0
  Caption = 'User'
  ClientHeight = 450
  ClientWidth = 382
  Color = clBtnFace
  Constraints.MaxHeight = 600
  Constraints.MaxWidth = 600
  Constraints.MinHeight = 480
  Constraints.MinWidth = 250
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
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 366
    Height = 402
    ActivePage = TabSheet
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 369
    ExplicitHeight = 329
    object TabSheet: TTabSheet
      Caption = 'View User'
      ExplicitLeft = 0
      ExplicitTop = 28
      ExplicitWidth = 276
      ExplicitHeight = 476
      object lblIP: TLabel
        Left = 8
        Top = 13
        Width = 10
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'IP'
      end
      object lblUsername: TLabel
        Left = 8
        Top = 40
        Width = 48
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'Username'
      end
      object lblAuth: TLabel
        Left = 8
        Top = 67
        Width = 23
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'Auth'
      end
      object lblFirstSeen: TLabel
        Left = 8
        Top = 94
        Width = 47
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'First seen'
      end
      object lblLastSeen: TLabel
        Left = 8
        Top = 121
        Width = 46
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'Last seen'
      end
      object lblTimesSeen: TLabel
        Left = 8
        Top = 148
        Width = 53
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'Times seen'
      end
      object lblDownload: TLabel
        Left = 8
        Top = 175
        Width = 47
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'Download'
      end
      object lblUpload: TLabel
        Left = 8
        Top = 202
        Width = 33
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'Upload'
      end
      object lblTimesRun: TLabel
        Left = 8
        Top = 229
        Width = 46
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'Times run'
      end
      object lblMergesBuilt: TLabel
        Left = 8
        Top = 256
        Width = 58
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'Merges built'
      end
      object lblPluginsChecked: TLabel
        Left = 8
        Top = 283
        Width = 75
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'Plugins checked'
      end
      object lblPluginsMerged: TLabel
        Left = 8
        Top = 310
        Width = 72
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'Plugins merged'
      end
      object lblReportsSubmitted: TLabel
        Left = 8
        Top = 337
        Width = 88
        Height = 13
        Margins.Left = 8
        Margins.Top = 7
        Margins.Right = 8
        Margins.Bottom = 7
        Caption = 'Reports submitted'
      end
      object edIP: TEdit
        Left = 139
        Top = 10
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edUsername: TEdit
        Left = 139
        Top = 37
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edAuth: TEdit
        Left = 139
        Top = 64
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edFirstSeen: TEdit
        Left = 139
        Top = 91
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edLastSeen: TEdit
        Left = 139
        Top = 118
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edTimesSeen: TEdit
        Left = 139
        Top = 145
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        NumbersOnly = True
        TabOrder = 5
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edDownload: TEdit
        Left = 139
        Top = 172
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        NumbersOnly = True
        TabOrder = 6
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edUpload: TEdit
        Left = 139
        Top = 199
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        NumbersOnly = True
        TabOrder = 7
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edTimesRun: TEdit
        Left = 139
        Top = 226
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        NumbersOnly = True
        TabOrder = 8
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edMergesBuilt: TEdit
        Left = 139
        Top = 253
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        NumbersOnly = True
        TabOrder = 9
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edPluginsChecked: TEdit
        Left = 139
        Top = 280
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        NumbersOnly = True
        TabOrder = 10
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edPluginsMerged: TEdit
        Left = 139
        Top = 307
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        NumbersOnly = True
        TabOrder = 11
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
      object edReportsSubmitted: TEdit
        Left = 139
        Top = 334
        Width = 211
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        NumbersOnly = True
        TabOrder = 12
        ExplicitLeft = 147
        ExplicitWidth = 121
      end
    end
  end
  object btnOK: TButton
    Left = 218
    Top = 416
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = btnOKClick
    ExplicitLeft = 140
    ExplicitTop = 421
  end
  object btnCancel: TButton
    Left = 299
    Top = 416
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 221
    ExplicitTop = 420
  end
end
