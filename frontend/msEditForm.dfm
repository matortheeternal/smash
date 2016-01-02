object EditForm: TEditForm
  Left = 0
  Top = 0
  ActiveControl = edName
  Caption = 'Edit patch'
  ClientHeight = 151
  ClientWidth = 333
  Color = clBtnFace
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
    Width = 317
    Height = 104
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
      object edName: TEdit
        Left = 102
        Top = 10
        Width = 202
        Height = 21
        Margins.Right = 8
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edNameChange
        OnKeyDown = edKeyDown
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
        OnChange = edFilenameChange
        OnEnter = edFilenameEnter
        OnKeyDown = edKeyDown
      end
    end
  end
  object btnOK: TButton
    Left = 250
    Top = 118
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = btnOKClick
  end
end
