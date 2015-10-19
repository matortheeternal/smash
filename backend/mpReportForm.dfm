object ReportForm: TReportForm
  Left = 0
  Top = 0
  Caption = 'Submit report'
  ClientHeight = 342
  ClientWidth = 328
  Color = clWindow
  Constraints.MinHeight = 380
  Constraints.MinWidth = 344
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    328
    342)
  PixelsPerInch = 96
  TextHeight = 13
  object gbYourReport: TGroupBox
    Left = 8
    Top = 141
    Width = 312
    Height = 162
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Your report'
    TabOrder = 0
    object lblRating: TLabel
      Left = 8
      Top = 20
      Width = 34
      Height = 13
      Align = alCustom
      Caption = 'Rating '
    end
    object lblNotes: TLabel
      Left = 8
      Top = 45
      Width = 28
      Height = 13
      Align = alCustom
      Caption = 'Notes'
    end
    object cbRating: TComboBox
      Left = 136
      Top = 17
      Width = 168
      Height = 21
      Hint = 'Rating hint'
      Align = alCustom
      AutoComplete = False
      Style = csDropDownList
      ItemIndex = 5
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = '4: Perfect'
      OnChange = cbRatingChange
      Items.Strings = (
        '-1: Blacklist'
        '0: Failure'
        '1: Dysfunctional'
        '2: Partially functional'
        '3: Tweaking required'
        '4: Perfect')
    end
    object meNotes: TMemo
      Left = 8
      Top = 64
      Width = 296
      Height = 90
      Margins.Left = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        '<Notes>')
      ParentShowHint = False
      ScrollBars = ssVertical
      ShowHint = True
      TabOrder = 1
      OnChange = meNotesChange
    end
  end
  object pnlTitle: TPanel
    Left = 8
    Top = 8
    Width = 312
    Height = 45
    Anchors = [akLeft, akTop, akRight]
    BevelEdges = []
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      312
      45)
    object lblFilename: TLabel
      Left = 6
      Top = 3
      Width = 298
      Height = 21
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '{Filename}'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object lblHash: TLabel
      Left = 6
      Top = 28
      Width = 100
      Height = 12
      AutoSize = False
      Caption = 'HASH: 00000000'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblRecords: TLabel
      Left = 106
      Top = 28
      Width = 104
      Height = 12
      Alignment = taCenter
      Anchors = [akTop]
      AutoSize = False
      Caption = 'RECORDS: 0'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Font.Quality = fqDraft
      ParentColor = False
      ParentFont = False
    end
    object lblFlags: TLabel
      Left = 208
      Top = 29
      Width = 96
      Height = 12
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'FLAGS: NAGVIF'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
  end
  object btnNext: TButton
    Left = 245
    Top = 309
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Next'
    Enabled = False
    TabOrder = 2
    OnClick = btnNextClick
    ExplicitTop = 387
  end
  object btnPrev: TButton
    Left = 8
    Top = 309
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Previous'
    Enabled = False
    TabOrder = 3
    OnClick = btnPrevClick
    ExplicitTop = 387
  end
  object gbUserReports: TGroupBox
    Left = 8
    Top = 59
    Width = 312
    Height = 79
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Existing reports'
    TabOrder = 4
    object lblExRating: TLabel
      Left = 8
      Top = 20
      Width = 72
      Height = 13
      Align = alCustom
      Caption = 'Average rating'
    end
    object lblExReports: TLabel
      Left = 8
      Top = 39
      Width = 88
      Height = 13
      Align = alCustom
      Caption = 'Number of reports'
    end
    object lblViewDetails: TLabel
      Left = 8
      Top = 58
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Margins.Left = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alCustom
      Caption = 'View details'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHotLight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblViewDetailsClick
      OnMouseEnter = lblViewDetailsMouseEnter
      OnMouseLeave = lblViewDetailsMouseLeave
    end
    object lblExRatingValue: TLabel
      Left = 136
      Top = 20
      Width = 44
      Height = 13
      Align = alCustom
      Caption = 'No rating'
    end
    object lblExReportsvalue: TLabel
      Left = 136
      Top = 39
      Width = 6
      Height = 13
      Align = alCustom
      Caption = '0'
    end
  end
end
