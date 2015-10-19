object SettingForm: TSettingForm
  Left = 0
  Top = 0
  Caption = 'Edit Smash Setting'
  ClientHeight = 426
  ClientWidth = 714
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
    Width = 698
    Height = 379
    ActivePage = GeneralTabSheet
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    TabWidth = 100
    object GeneralTabSheet: TTabSheet
      Caption = 'Edit Setting'
      ExplicitWidth = 625
      ExplicitHeight = 517
      object TreeView: TTreeView
        Left = 3
        Top = 3
        Width = 407
        Height = 345
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Indent = 19
        TabOrder = 0
        ExplicitWidth = 703
        ExplicitHeight = 511
      end
      object RightPanel: TPanel
        Left = 416
        Top = 0
        Width = 274
        Height = 351
        Align = alRight
        BevelOuter = bvLowered
        TabOrder = 1
        ExplicitLeft = 712
        ExplicitTop = 4
        ExplicitHeight = 517
        object lblName: TLabel
          Left = 8
          Top = 11
          Width = 27
          Height = 13
          Align = alCustom
          Caption = 'Name'
        end
        object lblDescription: TLabel
          Left = 8
          Top = 57
          Width = 53
          Height = 13
          Align = alCustom
          Caption = 'Description'
        end
        object meDescription: TMemo
          Left = 5
          Top = 76
          Width = 264
          Height = 271
          Margins.Left = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Align = alCustom
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 0
          ExplicitHeight = 278
        end
        object edName: TEdit
          Left = 8
          Top = 30
          Width = 259
          Height = 21
          Margins.Right = 8
          Align = alCustom
          TabOrder = 1
        end
      end
    end
  end
  object btnCancel: TButton
    Left = 630
    Top = 393
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitLeft = 926
    ExplicitTop = 559
  end
  object btnOK: TButton
    Left = 549
    Top = 393
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 2
    OnClick = btnOKClick
    ExplicitLeft = 845
    ExplicitTop = 559
  end
  object TreePopupMenu: TPopupMenu
    Left = 48
    Top = 40
  end
end
