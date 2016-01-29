object DictionaryForm: TDictionaryForm
  Left = 0
  Top = 0
  Caption = 'Dictionary Viewer'
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
    object lvEntries: TListView
      Left = 3
      Top = 58
      Width = 852
      Height = 621
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Filename'
          Width = 450
        end
        item
          Caption = 'Records'
          Width = 100
        end
        item
          Caption = 'Version'
          Width = 100
        end
        item
          Caption = 'Rating'
          Width = 100
        end
        item
          Caption = 'Reports'
          Width = 100
        end>
      DoubleBuffered = True
      OwnerData = True
      OwnerDraw = True
      ReadOnly = True
      RowSelect = True
      ParentDoubleBuffered = False
      SortType = stData
      TabOrder = 1
      TabStop = False
      ViewStyle = vsReport
      OnChange = lvEntriesChange
      OnColumnClick = lvEntriesColumnClick
      OnData = lvEntriesData
      OnDrawItem = lvEntriesDrawItem
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
      object lblFilename: TLabel
        Left = 8
        Top = 20
        Width = 42
        Height = 13
        Margins.Left = 8
        Caption = 'Filename'
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
      object lblVersion: TLabel
        Left = 457
        Top = 20
        Width = 35
        Height = 13
        Margins.Left = 12
        Anchors = [akTop, akRight]
        Caption = 'Version'
        ExplicitLeft = 451
      end
      object lblRating: TLabel
        Left = 592
        Top = 20
        Width = 31
        Height = 13
        Margins.Left = 12
        Anchors = [akTop, akRight]
        Caption = 'Rating'
        ExplicitLeft = 586
      end
      object lblReports: TLabel
        Left = 722
        Top = 20
        Width = 38
        Height = 13
        Margins.Left = 12
        Anchors = [akTop, akRight]
        Caption = 'Reports'
        ExplicitLeft = 716
      end
      object edFilename: TEdit
        Left = 56
        Top = 17
        Width = 245
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnExit = edExit
        OnKeyPress = edKeyPress
      end
      object cbVersion: TComboBox
        Left = 498
        Top = 17
        Width = 33
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemIndex = 0
        TabOrder = 3
        Text = '>'
        OnChange = cbChange
        Items.Strings = (
          '>'
          '<'
          '=')
      end
      object edRating: TEdit
        Left = 667
        Top = 17
        Width = 40
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 6
        OnExit = edExit
        OnKeyPress = edKeyPress
      end
      object cbReports: TComboBox
        Left = 766
        Top = 17
        Width = 32
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemIndex = 0
        TabOrder = 7
        Text = '>'
        OnChange = cbChange
        Items.Strings = (
          '>'
          '<'
          '=')
      end
      object edRecords: TEdit
        Left = 402
        Top = 17
        Width = 40
        Height = 21
        Anchors = [akTop, akRight]
        NumbersOnly = True
        TabOrder = 2
        OnExit = edExit
        OnKeyPress = edKeyPress
      end
      object cbRecords: TComboBox
        Left = 364
        Top = 17
        Width = 32
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemIndex = 0
        TabOrder = 1
        Text = '>'
        OnChange = cbChange
        Items.Strings = (
          '>'
          '<'
          '=')
      end
      object edVersion: TEdit
        Left = 537
        Top = 17
        Width = 40
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 4
        OnExit = edExit
        OnKeyPress = edKeyPress
      end
      object cbRating: TComboBox
        Left = 629
        Top = 17
        Width = 32
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemIndex = 0
        TabOrder = 5
        Text = '>'
        OnChange = cbChange
        Items.Strings = (
          '>'
          '<'
          '=')
      end
      object edReports: TEdit
        Left = 804
        Top = 17
        Width = 40
        Height = 21
        Margins.Right = 8
        Anchors = [akTop, akRight]
        NumbersOnly = True
        TabOrder = 8
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
    object pnlSelectGame: TPanel
      Left = 1
      Top = 1
      Width = 399
      Height = 39
      Align = alTop
      TabOrder = 2
      object lblGame: TLabel
        Left = 8
        Top = 12
        Width = 34
        Height = 13
        Align = alCustom
        Caption = 'Game: '
      end
      object cbGame: TComboBox
        Left = 192
        Top = 9
        Width = 201
        Height = 21
        Align = alCustom
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemIndex = 0
        TabOrder = 0
        Text = 'TES5'
        OnChange = cbGameChange
        Items.Strings = (
          'TES5'
          'TES4'
          'FNV'
          'FO3')
      end
    end
    object pnlDictionaryInfo: TPanel
      Left = 1
      Top = 40
      Width = 399
      Height = 203
      Align = alTop
      TabOrder = 0
      object lblDictionary: TLabel
        Left = 8
        Top = 8
        Width = 109
        Height = 13
        Caption = 'Dictionary information:'
      end
      object vlInfo: TValueListEditor
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
        OnDrawCell = vlInfoDrawCell
        ColWidths = (
          150
          235)
      end
    end
    object pnlReportNotes: TPanel
      Left = 1
      Top = 243
      Width = 399
      Height = 438
      Align = alClient
      TabOrder = 1
      object lblNotes: TLabel
        Left = 5
        Top = 6
        Width = 67
        Height = 13
        Caption = 'Report notes:'
      end
      object meNotes: TMemo
        Left = 3
        Top = 25
        Width = 391
        Height = 409
        TabStop = False
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
end
