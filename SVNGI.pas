unit SVNGI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,ShellApi, ExtCtrls, Clipbrd, ComCtrls, Menus,
  kpExplorer, PDirSelected;

type
  TReadThread = class(TThread)
  private
    buf: array [0..255] of Char;
    dummy: Cardinal;
    procedure UpdateForm;
  protected
    procedure Execute; override;
  public
    constructor Create;

    destructor Destroy; override;
  end;
  TForm_main = class(TForm)

    Memo_List: TMemo;
    edt_Command: TEdit;
    btn_Send: TButton;
    btn_login: TButton;
    Memo_log: TMemo;
    kpFolderTreeView1: TkpFolderTreeView;
    kpFileListBox1: TkpFileListBox;
    dr_dlgSavelog: TDirDialog;
    procedure btn_SendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn_loginClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure kpFolderTreeView1Click(Sender: TObject);
    procedure kpFileListBox1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    hPipeInputRead: THandle;
    hPipeInputWrite: THandle;
    hPipeOutputRead: THandle;
    hPipeOutputWrite: THandle;
    hProcess: THandle;
    AReadThread: TReadThread;
  public
    procedure request(Msg:string);
    procedure Delay(dwMilliseconds: Longint);
    { Public declarations }
  end;

var
  Form_main: TForm_main;
  Msg,Answer: string;

implementation

uses Unit2;

{$R *.dfm}
procedure TForm_main.Delay(dwMilliseconds: Longint);
 var
   iStart, iStop: DWORD;
 begin
   iStart := GetTickCount;
   repeat
     iStop := GetTickCount;
     Application.ProcessMessages;
   until (iStop - iStart) >= DWORD(dwMilliseconds);
end;

procedure TForm_main.request(Msg:string);
var
  buf: array [0..255] of Char;
  dummy: Cardinal;
begin
  Form_main.Memo_List.Clear;
  StrPCopy(buf, msg+#13#10);
  WriteFile(Form_main.hPipeInputWrite, buf, Length(msg)+2, dummy, nil);
  Form_main.Edt_Command.Clear;
end;

procedure TForm_main.btn_SendClick(Sender: TObject);
begin
  request(edt_Command.text);
end;

procedure TForm_main.FormCreate(Sender: TObject);
var
  securityattributes: TSecurityAttributes;
  startupinfo: TStartupInfo;
  processinformation: TProcessInformation;
begin
  securityattributes.nLength:=SizeOf(TSecurityAttributes);
  securityattributes.lpSecurityDescriptor:=nil;
  securityattributes.bInheritHandle:=True;
  CreatePipe(hPipeInputRead, hPipeInputWrite, @securityattributes, 0);
  CreatePipe(hPipeOutputRead, hPipeOutputWrite, @securityattributes, 0);
  ZeroMemory(@startupinfo, SizeOf(TStartupInfo));
  ZeroMemory(@processinformation, SizeOf(TProcessInformation));
  startupinfo.cb:=SizeOf(TStartupInfo);
  startupinfo.dwFlags:=STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  startupinfo.wShowWindow:=SW_HIDE;
  startupinfo.hStdInput:=hPipeInputRead;
  startupinfo.hStdOutput:=hPipeOutputWrite;
  startupinfo.hStdError:=hPipeOutputWrite;
  CreateProcess(nil, PChar('cmd'), nil, nil, True, CREATE_NEW_CONSOLE, nil, nil, startupinfo, processinformation);
  hProcess:=processinformation.hProcess;
  AReadThread:=TReadThread.Create;
end;

procedure TForm_main.btn_loginClick(Sender: TObject);
begin
  form_login.show;
end;

procedure TForm_main.FormDestroy(Sender: TObject);
begin
  TerminateProcess(hProcess, 255);
  WaitForSingleObject(hProcess, INFINITE);
  CloseHandle(hProcess);
  AReadThread.Terminate;
  CloseHandle(hPipeInputWrite);
  CloseHandle(hPipeInputRead);
  CloseHandle(hPipeOutputWrite);
  CloseHandle(hPipeOutputRead);
  AReadThread.WaitFor;
  AReadThread.Free;
end;

constructor TReadThread.Create;
begin
  inherited Create(False);
  FreeOnTerminate:=False;
end;

destructor TReadThread.Destroy;
begin
  inherited Destroy;
end;

procedure TReadThread.Execute;
begin
  while not Terminated do
    if ReadFile(Form_main.hPipeOutputRead, buf, Length(buf), dummy, nil) then Synchronize(UpdateForm);
end;

procedure TReadThread.UpdateForm;
begin
  OemToAnsiBuff(buf, buf, dummy);
  Form_Main.Memo_List.Text:=Form_Main.Memo_List.Text+Copy(buf, 1, dummy);
end;
procedure TForm_main.kpFolderTreeView1Click(Sender: TObject);
begin
kpFileListBox1.Directory := kpFolderTreeView1.Directory;
end;

procedure TForm_main.kpFileListBox1Click(Sender: TObject);
begin
 kpFolderTreeView1.Directory := kpFileListBox1.Directory;
end;

procedure TForm_main.FormCloseQuery(Sender: TObject;
var
  CanClose: Boolean);
var
  F: Textfile;
  I: Byte;
begin
 ///////////////////////////////////////////////////////////////////////////////
 /////////  Сохранение Лога ////////////////////////////////////////////////////
  if Memo_log.Lines[0]<>''then
    begin
      if MessageDlg('Сохранить лог?', mtCustom, [mbYes, mbNo], 0) = mrYes then
        begin
          dr_dlgSavelog.execute;
          AssignFile(F,dr_dlgSavelog.DirPath+'\LogSVN.ini');
          Rewrite(F);
          for I:= 0 to Memo_log.Lines.Count-1 do
            WriteLn(F,Memo_log.Lines[I]);
          CloseFile(F);
          CanClose:=True;
          ShowMessage('Сохранено в '+dr_dlgSavelog.DirPath+' ');
        end
      else
        CanClose:=True;
    end;
////////////////////////////////////////////////////////////////////////////////    
end;

end.
