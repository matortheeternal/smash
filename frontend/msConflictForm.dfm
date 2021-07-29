object ConflictForm: TConflictForm
  Left = 0
  Top = 0
  Caption = 'Combine smash settings'
  ClientHeight = 377
  ClientWidth = 709
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 545
    Top = 344
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 626
    Top = 344
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object tvRecords: TTreeView
    Left = 301
    Top = 8
    Width = 400
    Height = 330
    Align = alCustom
    Anchors = [akTop, akRight, akBottom]
    DoubleBuffered = True
    Indent = 19
    MultiSelect = True
    ParentDoubleBuffered = False
    ReadOnly = True
    StateImages = StateImages
    TabOrder = 2
    OnCustomDrawItem = tvRecordsCustomDrawItem
    OnMouseMove = tvRecordsMouseMove
  end
  object sbChoices: TScrollBox
    Left = 8
    Top = 8
    Width = 287
    Height = 330
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
  object FlagIcons: TImageList
    Left = 408
    Top = 24
    Bitmap = {
      494C010103000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000D7D7F8FF7373E6FF3434DBFF2121BEFF2121BEFF2D2DDAFF6B6BE4FFD2D2
      F7FF000000000000000000000000000000000000000000000000000000000000
      0000E4F1E779D7D6B8CCD4BF91F3D2B683FED2B581FED1BD8FF3D5D3B5CCE3F0
      E6790000000000000000000000000000000000000000D1D1D12E4F4F4FB01B1B
      1BE42B2B2BD48282827DF7F7F708000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F8F8FDFF7373
      E6FF2020B5FF2020B5FF3434DBFF5B5BE2FF5B5BE2FF3D3DDCFF2020B5FF2020
      B5FF6464E3FFF8F8FDFF00000000000000000000000000000000F2F8F33CD9D8
      BACBDCAB6CFEE1974BFFE18F3DFFE18A37FFDF8835FFDD8A39FFD99044FFD4A3
      64FED4D4B6CBF1F7F23C0000000000000000D1D1D12E0C0C0CF3000000FF0000
      00FF000000FF000000FF3B3B3BC4F2F2F20D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F8F8FDFF4242DDFF2020
      B5FF5353E0FFD7D7F8FF00000000000000000000000000000000E2E2F9FF5B5B
      E2FF2020B5FF3D3DDCFFEFEFFCFF0000000000000000F2F8F33CDBCDA5DEE3A1
      58FFE69240FFE58F3AFFE48D39FFE28B37FFE08A36FFDF8834FFDD8733FFDB87
      35FFD7944CFFD3C49DDEF1F7F23C000000004E4E4EB1000000FF4D4D4DB2D7D7
      D728B2B2B24D1A1A1AE5000000FF3A3A3AC5F2F2F20D00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007373E6FF2020B5FF8282
      E9FF000000000000000000000000FEFEFE03FDFDFE0700000000000000000000
      00009292EBFF2020B5FF6464E3FF0000000000000000DBDABCCBE6A35AFFE994
      41FFE8923EFFE7903CFFE58E3AFFE38D38FFE28B37FFE08935FFDF8834FFDD86
      32FFDC8633FFD6944BFFD4D3B5CB000000001B1B1BE4000000FFD7D7D7280000
      000000000000D9D9D9261B1B1BE4000000FF3A3A3AC5F2F2F20D000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D7D7F8FF2020B5FF5353E0FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006464E3FF2020B5FFC8C8F5FFE6F3E879E3B273FEEC9846FFEB95
      41FFEA933FFFE8913DFFEAA560FFF9F7F5FFF8F0E8FFE18B37FFE08935FFDE88
      33FFDD8632FFDA8735FFD3A263FEE3F0E6792B2B2BD4000000FFB3B3B34C0000
      00000000000000000000D9D9D926BABABA45EFEFEF10F3F3F30C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007A7AE7FF2020B5FFD2D2F7FF0000
      00000000000000000000FEFEFE0D0000000000000000F4F4FC28000000000000
      000000000000E2E2F9FF2020B5FF6B6BE4FFDDDBBDCCEBA256FFEE9844FFED96
      42FFEB9440FFE9933FFFEBA562FFF9F7F5FFF8F0E8FFE38C38FFE18A36FFE089
      35FFDE8733FFDD8632FFD88F43FFD4D3B5CC8383837C000000FF1A1A1AE5D9D9
      D9260000000000000000C3C3C33C424242BD131313EC262626D97F7F7F80F7F7
      F708000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003434DBFF3434DBFF000000000000
      0000B9B9F2FFB9B9F2FFB9B9F2FFB9B9F2FFB9B9F2FFB9B9F2FFB9B9F2FFB9B9
      F2FFEFEFFCFF000000004242DDFF2525D6FFDDC89AF3F09D4BFFF09945FFEE97
      43FFED9642FFEB9440FFECA663FFF9F7F5FFF8F0E9FFE48D39FFE28C38FFE18A
      36FFDF8934FFDE8733FFDB8937FFD1BC8DF3F7F7F7083B3B3BC4000000FF1B1B
      1BE4D9D9D926E3E3E31C060606F9000000FF000000FF000000FF000000FF3B3B
      3BC4F2F2F20D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002323C6FF5353E0FF000000000000
      00002121BEFF2020B5FF2020B5FF2020B5FF2020B5FF2020B5FF2020B5FF2020
      B5FFC8C8F5FF000000006464E3FF2121BEFFDFC38FFEF39C49FFF19A46FFEF99
      44FFEE9743FFEC9541FFEEA864FFF9F7F5FFF8F0E9FFE68F3BFFE48D39FFE28B
      37FFE18A36FFDF8834FFDD8734FFD0B480FE00000000F2F2F20D3A3A3AC50000
      00FF1B1B1BE4CECECE31444444BB7171718EE1E1E11EB7B7B7481A1A1AE50000
      00FF3A3A3AC5F2F2F20D00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002323C6FF5353E0FF000000000000
      00002020B5FF2020B5FF2020B5FF2020B5FF2020B5FF2020B5FF2020B5FF2020
      B5FFC8C8F5FF000000006464E3FF2121BEFFE0C390FEF49D4AFFF29B47FFF19A
      46FFEF9844FFEE9743FFEFA965FFF9F7F5FFF8F0E9FFE7903CFFE58E3AFFE48D
      39FFE28B37FFE08A35FFDF8835FFD1B581FE0000000000000000F2F2F20D3A3A
      3AC5000000FF1A1A1AE5B8B8B847E1E1E11E7171718E444444BBCECECE311B1B
      1BE4000000FF3A3A3AC5F2F2F20D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003D3DDCFF2D2DDAFF000000000000
      0000F8F8FDFFF8F8FDFFEFEFFCFFEFEFFCFFEFEFFCFFEFEFFCFFEFEFFCFFF8F8
      FDFFF8F8FDFF000000003D3DDCFF2525D6FFDFCA9CF3F3A14FFFF39D48FFF29B
      47FFF09A45FFEF9844FFF0B172FFF9F7F5FFF8F0E9FFE8913DFFE7903CFFE58E
      3AFFE38C38FFE28B37FFDF8C3BFFD2BE90F3000000000000000000000000F2F2
      F20D3B3B3BC4000000FF000000FF000000FF000000FF060606F9E3E3E31CD9D9
      D9261B1B1BE4000000FF3B3B3BC4F7F7F7080000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007373E6FF2020B5FFD2D2F7FF0000
      0000000000000000000000000000F1F1FA0FFAFAFD05F1F1FC32000000000000
      000000000000E2E2F9FF2020B5FF6B6BE4FFDFDDBFCCF1A85BFFF49E4AFFF39C
      48FFF29B47FFF5CBA3FFF9F7F5FFF9F7F5FFF8F0E9FFEA933FFFE8913DFFE68F
      3BFFE58E3AFFE38C38FFDE9548FFD7D5B7CC0000000000000000000000000000
      0000F7F7F7087F7F7F80262626D9131313EC424242BDC3C3C33C000000000000
      0000D9D9D9261A1A1AE5000000FF8383837C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D7D7F8FF2020B5FF5353E0FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006464E3FF2020B5FFC8C8F5FFE7F3E979E9B879FEF4A14FFFF49D
      49FFF39C48FFF19B46FFF19F51FFF6DEC6FFF8F1E9FFEB9440FFE9933EFFE891
      3DFFE68F3BFFE3903DFFDAA96AFEE4F1E7790000000000000000000000000000
      00000000000000000000F3F3F30CEFEFEF10BABABA45D9D9D926000000000000
      000000000000B3B3B34C000000FF2C2C2CD30000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007A7AE7FF2020B5FF8282
      E9FF000000000000000000000000000000000000000000000000000000000000
      00008B8BEAFF2020B5FF6464E3FF0000000000000000DFDEC0CBEFAD64FFF4A0
      4CFFF49D49FFF39C48FFF19A46FFF09945FFEE9743FFEC9641FFEB9440FFE992
      3EFFE7913EFFE19E55FFD8D7B9CB000000000000000000000000000000000000
      00000000000000000000F2F2F20D3A3A3AC5000000FF1B1B1BE4D9D9D9260000
      000000000000D7D7D728000000FF1B1B1BE40000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F8F8FDFF4242DDFF2020
      B5FF5353E0FFD7D7F8FF00000000000000000000000000000000D7D7F8FF5B5B
      E2FF2020B5FF4242DDFFEFEFFCFF0000000000000000F3F9F43CE1D2ABDEEFAD
      63FFF4A04EFFF49D49FFF29B47FFF19A46FFEF9944FFEE9743FFEC9541FFE996
      43FFE3A158FFD9CAA3DEF2F8F33C000000000000000000000000000000000000
      0000000000000000000000000000F2F2F20D3A3A3AC5000000FF1A1A1AE5B3B3
      B34CD7D7D7284E4E4EB1000000FF4F4F4FB00000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F8F8FDFF7373
      E6FF2020B5FF2020B5FF3434DBFF6464E3FF5B5BE2FF3D3DDCFF2020B5FF2020
      B5FF6B6BE4FFEFEFFCFF00000000000000000000000000000000F3F9F43CDFDD
      C0CBE8B778FEF0A75AFFF2A04EFFF29B48FFF19A47FFEE9B49FFE9A053FFE0B0
      71FEDAD9BCCBF2F8F33C00000000000000000000000000000000000000000000
      000000000000000000000000000000000000F2F2F20D3B3B3BC4000000FF0000
      00FF000000FF000000FF0C0C0CF3D2D2D22D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D7D7F8FF7373E6FF2D2DDAFF2121BEFF2121BEFF2D2DDAFF6B6BE4FFD2D2
      F7FF000000000000000000000000000000000000000000000000000000000000
      0000E6F3E979DFDCBFCCDEC99BF3DFC28EFEDEC18EFEDCC799F3DCDABCCCE5F2
      E879000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F7F7F7088282827D2B2B
      2BD41C1C1CE34F4F4FB0D2D2D22D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00F00FF00F81FF0000C003C00300FF0000
      83C18001007F00008E718001183F00001FF800001C3F00001DB800000C0F0000
      3004000000070000300400008003000030040000C001000030040000E0000000
      1E380000F03000001FF80000FC3800008FF18001FC18000083C18001FE000000
      C003C003FF000000F00FF00FFF81000000000000000000000000000000000000
      000000000000}
  end
  object StateImages: TImageList
    Height = 17
    Width = 17
    Left = 336
    Top = 24
    Bitmap = {
      494C010104000800040011001100FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000440000002200000001002000000000002024
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
      0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4
      F4FFF4F4F4FFF4F4F4FFF5F5F5FFF9F9F9FFF8F8F8FFF5F5F5FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F
      8EFFF4F4F4FFF4F4F4FFF4F4F4FFF5F5F5FFF9F9F9FFF8F8F8FFF5F5F5FFF4F4
      F4FFF4F4F4FFF4F4F4FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF8F8F8EFFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF8F8F8EFFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FF8F8F8EFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFCCCBCAFFDBDADAFFE9E2DFFFBA99
      8CFFBD9D90FFF6F3F2FFEDEDECFFECEBEBFFEAE9E9FFF4F4F4FF8F8F8EFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFCCCBCAFFDBDADAFFE9E2
      DFFFBA998CFFBD9D90FFF6F3F2FFEDEDECFFECEBEBFFEAE9E9FFF4F4F4FF8F8F
      8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFCCCBCAFFD5D4
      D4FFDCDBDBFFE1E1E0FFE7E7E6FFEBEBEAFFECECEBFFECEBEBFFEAE9E9FFF4F4
      F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FF714E
      21FF6A491FFF61441DFF593E1AFF553B19FF553B19FF553A19FF553A19FF553B
      19FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4
      F4FFCAC8C6FFF0ECEAFFBB998BFF975F4AFF98614CFFD1B9B0FFF9F9F9FFF6F6
      F6FFE6E6E6FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F
      8EFFF4F4F4FFCAC8C6FFF0ECEAFFBB998BFF975F4AFF98614CFFD1B9B0FFF9F9
      F9FFF6F6F6FFE6E6E6FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF8F8F8EFFF4F4F4FFC6C4C2FFE9E9E9FFEDEDEDFFF0F0F0FFF4F4F4FFF6F6
      F6FFF6F6F6FFF6F6F6FFE6E6E6FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF8F8F8EFFF4F4F4FF755123FFB89255FFAE864CFFA47A44FF996E
      3BFF936532FF8F5F2CFF8C5925FF553B19FFF4F4F4FF8F8F8EFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFD1CFCDFFE9E1DEFF955D48FF965F
      49FF97604BFFA47361FFFAF9F8FFF4F4F4FFE2E2E1FFF4F4F4FF8F8F8EFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFD1CFCDFFE9E1DEFF955D
      48FF965F49FF97604BFFA47361FFFAF9F8FFF4F4F4FFE2E2E1FFF4F4F4FF8F8F
      8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFC2BFBCFFE5E4
      E3FFE9E9E9FFEDEDEDFFF2F2F2FFF4F4F4FFF5F5F5FFF4F4F4FFE2E2E1FFF4F4
      F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FF7B55
      24FFC29D5DFFB38927FFAA8124FFA07822FF966F1FFF8F691EFF8F602CFF553B
      19FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4
      F4FFE1E0DEFFAA7F6EFF945C47FFE2D4CFFFA77867FF97604BFFD5BFB7FFF6F6
      F6FFDEDDDCFFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F
      8EFFF4F4F4FFE1E0DEFFAA7F6EFF945C47FFE2D4CFFFA77867FF97604BFFD5BF
      B7FFF6F6F6FFDEDDDCFFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF8F8F8EFFF4F4F4FFBFBBB8FFE1DFDDFFE5E5E4FFEAEAEAFFEFEFEFFFF2F2
      F2FFF2F2F2FFF2F2F2FFDEDDDCFFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF8F8F8EFFF4F4F4FF815A26FFCBA966FFBF942AFFB88E28FFB087
      26FFA77F24FF9F7722FF9A6B34FF5C401BFFF4F4F4FF8F8F8EFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFCDC9C5FFDDCFC9FFC8AEA3FFEEEE
      EDFFD5C1BAFF965E49FFA57664FFF8F8F8FFD6D5D5FFF4F4F4FF8F8F8EFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFCDC9C5FFDDCFC9FFC8AE
      A3FFEEEEEDFFD5C1BAFF965E49FFA57664FFF8F8F8FFD6D5D5FFF4F4F4FF8F8F
      8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFBCB7B2FFDCD8
      D5FFDFDCDAFFE3E1E0FFE8E8E8FFECECECFFEDEDEDFFEDEDEDFFD6D5D4FFF4F4
      F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FF855D
      27FFD4B36DFFC99E2CFFC4992BFFBE932AFFB78D28FFAE8525FFA6793EFF6546
      1EFFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4
      F4FFB9B3AEFFDDD9D5FFE5E2DFFFDCD8D5FFF4F3F2FFA1715EFF945C47FFD6C3
      BCFFDCDCDBFFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F
      8EFFF4F4F4FFB9B3AEFFDDD9D5FFE5E2DFFFDCD8D5FFF4F3F2FFA1715EFF945C
      47FFD6C3BCFFDCDCDBFFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF8F8F8EFFF4F4F4FFB9B3AEFFD7D1CDFFD9D4D0FFDBD7D4FFDFDDDBFFE3E2
      E1FFE6E6E5FFE8E8E8FFCDCDCCFFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF8F8F8EFFF4F4F4FF895F28FFDABB74FFD1A42EFFCEA22DFFC99D
      2CFFC2972BFFBB9129FFB18647FF6E4C20FFF4F4F4FF8F8F8EFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFB9B3AEFFD5CFCBFFD5CFCBFFD6D1
      CDFFE6E2E0FFCFB8AFFF925A45FFA57765FFE8E7E7FFF4F4F4FF8F8F8EFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFB9B3AEFFD5CFCBFFD5CF
      CBFFD6D1CDFFE6E2E0FFCFB8AFFF925A45FFA57765FFE8E7E7FFF4F4F4FF8F8F
      8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFB9B3AEFFD5CF
      CBFFD5CFCBFFD6D1CDFFDAD5D2FFDEDBD8FFE1DFDDFFE4E3E2FFC8C7C6FFF4F4
      F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FF8B61
      29FFE0C27BFFD5A82FFFD3A62FFFD0A42EFFCB9F2DFFC5992BFFBB9250FF7652
      23FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4
      F4FFB9B3AEFFD5CFCBFFD5CFCBFFD5CFCBFFD6D0CCFFF1EEEDFF9D6A57FF925A
      44FFD0BFB9FFF6F6F6FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F
      8EFFF4F4F4FFB9B3AEFFD5CFCBFFD5CFCBFFD5CFCBFFD6D0CCFFF1EEEDFF9D6A
      57FF925A44FFD0BFB9FFF6F6F6FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF8F8F8EFFF4F4F4FFB9B3AEFFD5CFCBFFD5CFCBFFD5CFCBFFD5CFCBFFD8D3
      D0FFDCD8D5FFDFDDDBFFC5C3C1FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF8F8F8EFFF4F4F4FF8D6229FFE4C87FFFE1C37BFFDDBE75FFD9B7
      6EFFD3AF68FFCCA760FFC49D59FF7C5624FFF4F4F4FF8F8F8EFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFB9B3AEFFB9B3AEFFB9B3AEFFB9B3
      AEFFB9B3AEFFD0CCC9FFC0A79DFFAB8677FFE4DFDCFFF5F5F5FF8F8F8EFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFB9B3AEFFB9B3AEFFB9B3
      AEFFB9B3AEFFB9B3AEFFD0CCC9FFC0A79DFFAB8677FFE4DFDCFFF5F5F5FF8F8F
      8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FFB9B3AEFFB9B3
      AEFFB9B3AEFFB9B3AEFFB9B3AEFFB9B3AEFFBAB4AFFFBDB9B4FFC1BEBBFFF4F4
      F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4F4FF8D62
      29FF8E622AFF8E622AFF8D622AFF8C6129FF8A6028FF875E28FF845B27FF8059
      26FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFFF4F4
      F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF8F8F8FFF9F9
      F9FFF6F6F6FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F
      8EFFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF8F8
      F8FFF9F9F9FFF6F6F6FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF8F8F8EFFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FF8F8F8EFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF8F8F8EFFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FF8F8F8EFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F8EFF8F8F
      8EFF8F8F8EFF8F8F8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF424D3E000000000000003E000000
      2800000044000000220000000100010000000000980100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
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
      000000000000000000000000000000000000000000000000000000000000}
  end
end
