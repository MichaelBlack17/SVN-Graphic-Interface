object Form_login: TForm_login
  Left = 520
  Top = 248
  Width = 298
  Height = 327
  Caption = 'Login'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 16
    Top = 64
    Width = 121
    Height = 41
    AutoSize = False
    Caption = #1051#1086#1075#1080#1085
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbl2: TLabel
    Left = 16
    Top = 112
    Width = 105
    Height = 25
    AutoSize = False
    Caption = #1055#1072#1088#1086#1083#1100
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbl3: TLabel
    Left = 32
    Top = 0
    Width = 209
    Height = 41
    AutoSize = False
    Caption = #1040#1076#1088#1077#1089' '#1088#1077#1087#1086#1079#1080#1090#1086#1088#1080#1103
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbl4: TLabel
    Left = 8
    Top = 152
    Width = 257
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1076#1080#1088#1077#1082#1090#1086#1088#1080#1102' '#1076#1083#1103' '#1088#1077#1074#1080#1079#1080#1080
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object edt_login: TEdit
    Left = 104
    Top = 64
    Width = 153
    Height = 21
    TabOrder = 0
  end
  object edt_password: TEdit
    Left = 104
    Top = 112
    Width = 153
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object btn_connect: TButton
    Left = 192
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 2
    OnClick = btn_connectClick
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 240
    Width = 145
    Height = 17
    Caption = 'Save login and Passworg'
    TabOrder = 3
  end
  object edt_adres: TEdit
    Left = 8
    Top = 32
    Width = 249
    Height = 21
    TabOrder = 4
    Text = ' http://dev.iu7.bmstu.ru/svn/se_15_m-chernyak/'
  end
  object edt_plase: TEdit
    Left = 8
    Top = 168
    Width = 257
    Height = 21
    TabOrder = 5
  end
  object btn1: TButton
    Left = 136
    Top = 200
    Width = 131
    Height = 25
    Caption = #1042#1099#1073#1088#1072#1090#1100'...'
    TabOrder = 6
    OnClick = btn1Click
  end
  object Dialog_folder: TDirDialog
    NewFolder = True
    Left = 40
    Top = 192
  end
end
