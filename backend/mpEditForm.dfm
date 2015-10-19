object EditForm: TEditForm
  Left = 0
  Top = 0
  ActiveControl = TabSheet1
  Caption = 'Edit Report'
  ClientHeight = 412
  ClientWidth = 354
  Color = clBtnFace
  Constraints.MaxHeight = 450
  Constraints.MaxWidth = 450
  Constraints.MinHeight = 250
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    354
    412)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 338
    Height = 365
    ActivePage = TabSheet1
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Edit Report'
      object lblUsername: TLabel
        Left = 8
        Top = 13
        Width = 48
        Height = 13
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Username'
        Transparent = True
      end
      object lblFilename: TLabel
        Left = 8
        Top = 42
        Width = 42
        Height = 13
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Filename'
      end
      object lblRecordcount: TLabel
        Left = 8
        Top = 93
        Width = 64
        Height = 13
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Record count'
      end
      object lblHash: TLabel
        Left = 8
        Top = 69
        Width = 24
        Height = 13
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Hash'
        Transparent = True
      end
      object lblRating: TLabel
        Left = 8
        Top = 123
        Width = 31
        Height = 13
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Rating'
      end
      object lblMergeVersion: TLabel
        Left = 8
        Top = 150
        Width = 68
        Height = 13
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Merge version'
      end
      object lblNotes: TLabel
        Left = 8
        Top = 177
        Width = 28
        Height = 13
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Notes'
      end
      object edUsername: TEdit
        Left = 136
        Top = 12
        Width = 186
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnKeyDown = edKeyDown
      end
      object edFilename: TEdit
        Left = 136
        Top = 39
        Width = 186
        Height = 21
        Hint = 'Filename invalid.'
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnKeyDown = edKeyDown
      end
      object edRecordCount: TEdit
        Left = 136
        Top = 93
        Width = 186
        Height = 21
        Hint = 'Filename invalid.'
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        NumbersOnly = True
        TabOrder = 2
        OnKeyDown = edKeyDown
      end
      object edHash: TEdit
        Left = 136
        Top = 66
        Width = 186
        Height = 21
        Hint = 'Warning: Mod folder already exists.'
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnKeyDown = edKeyDown
      end
      object cbRating: TComboBox
        Left = 136
        Top = 120
        Width = 186
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 5
        TabOrder = 4
        Text = '4: Perfect'
        Items.Strings = (
          '-1: Blacklisted'
          '0: Failure'
          '1: Dysfunctional'
          '2: Partially functional'
          '3: Tweaking required'
          '4: Perfect')
      end
      object edMergeVersion: TEdit
        Left = 136
        Top = 147
        Width = 186
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        OnKeyDown = edKeyDown
      end
      object meNotes: TMemo
        Left = 8
        Top = 201
        Width = 314
        Height = 128
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          '<Notes>')
        ScrollBars = ssVertical
        TabOrder = 6
      end
    end
  end
  object btnOk: TButton
    Left = 190
    Top = 379
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 271
    Top = 379
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
