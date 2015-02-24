object Form_main: TForm_main
  Left = 150
  Top = 195
  BorderStyle = bsSingle
  Caption = 'SVN Graphic Interface'
  ClientHeight = 525
  ClientWidth = 845
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo_List: TMemo
    Left = 16
    Top = 48
    Width = 521
    Height = 113
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object edt_Command: TEdit
    Left = 16
    Top = 8
    Width = 521
    Height = 33
    AutoSize = False
    TabOrder = 1
  end
  object btn_Send: TButton
    Left = 544
    Top = 8
    Width = 81
    Height = 33
    Caption = 'send'
    TabOrder = 2
    OnClick = btn_SendClick
  end
  object btn_login: TButton
    Left = 544
    Top = 48
    Width = 81
    Height = 25
    Caption = 'Login'
    TabOrder = 3
    OnClick = btn_loginClick
  end
  object Memo_log: TMemo
    Left = 216
    Top = 392
    Width = 417
    Height = 121
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object kpFolderTreeView1: TkpFolderTreeView
    Left = 16
    Top = 168
    Width = 193
    Height = 345
    Indent = 19
    TabOrder = 5
    OnClick = kpFolderTreeView1Click
    AltColor = clBlue
    Directory = 'C:\'
  end
  object kpFileListBox1: TkpFileListBox
    Left = 216
    Top = 168
    Width = 145
    Height = 217
    Style = lbOwnerDrawFixed
    TabOrder = 6
    OnClick = kpFileListBox1Click
    AltColor = clBlue
    Directory = 'C:\windows\'
    FileType = [ftDirectory, ftArchive, ftNormal]
    Mask = '*.*'
    ReadOnly = False
    ShowFolders = True
    FoldersOnly = False
  end
  object dr_dlgSavelog: TDirDialog
    NewFolder = False
    Left = 416
    Top = 272
  end
end
