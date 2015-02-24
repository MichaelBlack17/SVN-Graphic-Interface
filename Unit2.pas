unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry, SVNGI, PDirSelected;

type
  TForm_login = class(TForm)
    edt_login: TEdit;
    edt_password: TEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    btn_connect: TButton;
    CheckBox1: TCheckBox;
    lbl3: TLabel;
    edt_adres: TEdit;
    edt_plase: TEdit;
    lbl4: TLabel;
    btn1: TButton;
    Dialog_folder: TDirDialog;
    procedure btn_connectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_login: TForm_login;
  RegFileLg: TregIniFile;
  RegFilePass: TregIniFile;
  RegFileAdress: TregIniFile;
  RegFileBool: TregIniFile;
  Check: boolean;
const
  SubSect: String = 'SvnClient';
implementation



{$R *.dfm}

procedure TForm_login.btn_connectClick(Sender: TObject);
var
  Msg:string;
  Login, Password,Adres: string;

begin
  Login := Edt_Login.text;
  Password := edt_password.Text;
  Adres:= edt_Adres.Text;
  {  Занесение логина и пароля в реестр  }
  if CheckBox1.Checked=false then
  begin
    Check:=CheckBox1.Checked;
    RegFileBool.Writebool('Login','LgCheck',Check);
    RegFileAdress.WriteString('Login','lgAdres','');
    RegFileLg.WriteString('Login','lg','');
    RegFilePass.WriteString('Login','lgP','');
  end
  else
  begin
    Check:=CheckBox1.Checked;
    RegFileBool.Writebool('Login','LgCheck',Check);
    RegFileAdress.WriteString('Login','lgAdres',Adres);
    RegFileLg.WriteString('Login','lg',Login);
    RegFilePass.WriteString('Login','lgP',Password);
  end;

  {  изменение адреса к подключаемой директории  }

  Form_main.request('cd ' +Edt_plase.text);
  
  {  команды подключения к репозиторию при помощи логина/пароля  }
 Form_main.request('svn checkout '+ Form_login.edt_adres.text+' --username '+login);
 Form_main.Delay(3000);

 if (ansiPos('revision',Form_main.Memo_List.Lines[1]) <> 0) or(ansiPos('revision',Form_main.Memo_List.Lines[2]) <> 0) then
 begin
   Form_main.Memo_Log.Lines.Add('['+Timetostr(GetTime)+'] You are login as '+ Form_login.edt_login.text + #13#10);
   Form_login.Hide;
 end
 else
 begin
   Form_main.request(password);
   Form_main.Delay(3000);
   if ansiPos('revision',Form_main.Memo_List.Lines[1]) <> 0 then
   begin
     Form_main.Memo_Log.Lines.Add('['+Timetostr(GetTime)+'] You are login as '+ Form_login.edt_login.text + #13#10);
     Form_login.Hide;
   end
   else
   begin
     ShowMessage('You can not login');
     Form_main.Memo_Log.Lines.Add('['+Timetostr(GetTime)+'] You can not login as '+ Form_login.edt_login.text + #13#10);
   end;
 end;
end;

procedure TForm_login.FormCreate(Sender: TObject);
begin
  { Создание пустых файлов для логина и пароля  }
  RegFilePass:= TregIniFile.Create(SubSect);
  RegFileLg:= TregIniFile.Create(SubSect);
  RegFileAdress:= TregIniFile.Create(SubSect);
  RegFileBool:= TregIniFile.Create(SubSect);
  Edt_Login.Text:=(RegfileLg.ReadString('Login','lg',''));
  Edt_password.Text:=(RegfilePass.ReadString('Login','lgP',''));
  Edt_adres.Text:=(RegfileAdress.ReadString('Login','lgAdres',''));
  if RegfileBool.ReadBool('Login','LgCheck',false) then
    Check:=True
  else
    Check:=false;
  CheckBox1.Checked:=Check;
end;

procedure TForm_login.btn1Click(Sender: TObject);
begin
  dialog_folder.execute;
  edt_plase.Text :=  dialog_folder.DirPath;
end;

end.
