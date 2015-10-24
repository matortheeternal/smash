object SettingsManager: TSettingsManager
  Left = 0
  Top = 0
  Caption = 'Settings Manager'
  ClientHeight = 682
  ClientWidth = 1264
  Color = clBtnFace
  Constraints.MinHeight = 540
  Constraints.MinWidth = 966
  DoubleBuffered = True
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
  object Splitter: TSplitter
    Left = 860
    Top = 0
    Height = 682
    Beveled = True
    Color = clSilver
    ParentColor = False
    OnMoved = SplitterMoved
    ExplicitLeft = 857
  end
  object pnlEntries: TPanel
    Left = 0
    Top = 0
    Width = 860
    Height = 682
    Align = alLeft
    Caption = 'pnlEntries'
    Constraints.MinWidth = 650
    TabOrder = 0
    object lvSettings: TListView
      Left = 3
      Top = 58
      Width = 852
      Height = 621
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          AutoSize = True
          Caption = 'Name'
        end
        item
          Caption = 'Records'
          Width = 400
        end>
      DoubleBuffered = True
      OwnerData = True
      OwnerDraw = True
      ReadOnly = True
      RowSelect = True
      ParentDoubleBuffered = False
      PopupMenu = SettingsPopupMenu
      SortType = stData
      TabOrder = 1
      TabStop = False
      ViewStyle = vsReport
      OnChange = lvSettingsChange
      OnColumnClick = lvSettingsColumnClick
      OnData = lvSettingsData
      OnDrawItem = lvSettingsDrawItem
    end
    object gbFiltering: TGroupBox
      Left = 3
      Top = 3
      Width = 852
      Height = 49
      Margins.Left = 2
      Margins.Right = 2
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Filtering options'
      TabOrder = 0
      DesignSize = (
        852
        49)
      object lblName: TLabel
        Left = 8
        Top = 20
        Width = 27
        Height = 13
        Margins.Left = 8
        Caption = 'Name'
      end
      object lblRecords: TLabel
        Left = 316
        Top = 20
        Width = 42
        Height = 13
        Margins.Left = 12
        Anchors = [akTop, akRight]
        Caption = 'Records '
        ExplicitLeft = 310
      end
      object edName: TEdit
        Left = 41
        Top = 17
        Width = 260
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnExit = edExit
        OnKeyPress = edKeyPress
      end
      object edRecords: TEdit
        Left = 364
        Top = 17
        Width = 477
        Height = 21
        Anchors = [akTop, akRight]
        NumbersOnly = True
        TabOrder = 1
        OnExit = edExit
        OnKeyPress = edKeyPress
      end
    end
  end
  object pnlDetails: TPanel
    Left = 863
    Top = 0
    Width = 401
    Height = 682
    Align = alClient
    Constraints.MinWidth = 300
    TabOrder = 1
    object pnlDictionaryInfo: TPanel
      Left = 1
      Top = 1
      Width = 399
      Height = 203
      Align = alTop
      TabOrder = 0
      object lblDictionary: TLabel
        Left = 8
        Top = 8
        Width = 100
        Height = 13
        Caption = 'Settings information:'
      end
      object vl: TValueListEditor
        Left = 3
        Top = 27
        Width = 391
        Height = 171
        TabStop = False
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        DefaultDrawing = False
        DisplayOptions = [doAutoColResize, doKeyColFixed]
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goThumbTracking]
        ScrollBars = ssVertical
        Strings.Strings = (
          '')
        TabOrder = 0
        OnDrawCell = vlDrawCell
        ColWidths = (
          150
          235)
      end
    end
    object pnlReportNotes: TPanel
      Left = 1
      Top = 204
      Width = 399
      Height = 477
      Align = alClient
      TabOrder = 1
      object lblNotes: TLabel
        Left = 5
        Top = 6
        Width = 57
        Height = 13
        Caption = 'Description:'
      end
      object meNotes: TMemo
        Left = 3
        Top = 25
        Width = 391
        Height = 448
        TabStop = False
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object SettingsPopupMenu: TPopupMenu
    Left = 48
    Top = 104
    object NewSettingItem: TMenuItem
      Caption = 'New setting'
      OnClick = NewSettingItemClick
    end
    object EditSettingItem: TMenuItem
      Caption = 'Edit setting'
      OnClick = EditSettingItemClick
    end
    object DeleteSettingItem: TMenuItem
      Caption = 'Delete setting'
      OnClick = DeleteSettingItemClick
    end
  end
end
