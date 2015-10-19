object EditForm: TEditForm
  Left = 0
  Top = 0
  Caption = 'Edit patch'
  ClientHeight = 186
  ClientWidth = 333
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
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 317
    Height = 139
    ActivePage = EditTabSheet
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    TabWidth = 60
    object EditTabSheet: TTabSheet
      Caption = 'Edit'
      object lblName: TLabel
        Left = 12
        Top = 13
        Width = 27
        Height = 13
        Caption = 'Name'
      end
      object lblFilename: TLabel
        Left = 12
        Top = 40
        Width = 42
        Height = 13
        Caption = 'Filename'
      end
      object lblSetting: TLabel
        Left = 12
        Top = 67
        Width = 34
        Height = 13
        Caption = 'Setting'
      end
      object edName: TEdit
        Left = 102
        Top = 10
        Width = 202
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object edFilename: TEdit
        Left = 102
        Top = 37
        Width = 202
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object cbSetting: TComboBox
        Left = 102
        Top = 64
        Width = 202
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
    end
  end
  object btnOK: TButton
    Left = 250
    Top = 153
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = btnOKClick
  end
end
