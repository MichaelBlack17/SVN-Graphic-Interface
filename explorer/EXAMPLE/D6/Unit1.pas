unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, kpExplorer, Buttons;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    kpFolderTreeView1: TkpFolderTreeView;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Panel2: TPanel;
    kpFileListBox1: TkpFileListBox;
    Label1: TLabel;
    CheckBox2: TCheckBox;
    Label2: TLabel;
    Edit2: TEdit;
    Button1: TButton;
    ListBox1: TListBox;
    Label3: TLabel;
    Panel3: TPanel;
    CheckBox3: TCheckBox;
    Edit3: TEdit;
    Button3: TButton;
    ListBox2: TListBox;
    Label4: TLabel;
    kpFileListView1: TkpFileListView;
    Label5: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton9: TSpeedButton;
    TabSheet4: TTabSheet;
    Panel4: TPanel;
    kpFileListView2: TkpFileListView;
    kpFolderCombo1: TkpFolderCombo;
    Label9: TLabel;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    StatusBar2: TStatusBar;
    StatusBar3: TStatusBar;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    SpeedButton5: TSpeedButton;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox2Click(Sender: TObject);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure CheckBox3Click(Sender: TObject);
    procedure Edit3KeyPress(Sender: TObject; var Key: Char);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure kpFileListBox1FileSelect(Sender: TObject; Name: String);
    procedure kpFileListView1FileSelect(Sender: TObject; Name: String);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure kpFileListView2FileRename(Sender: TObject; OldName,
      NewName: String; Success, IsFolder: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
   kpFolderTreeView1.NetBrowse := CheckBox1.Checked;
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
     if Key = #13 then kpFolderTreeView1.Directory := Edit1.Text;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
    kpFileListBox1.ShowFolders := CheckBox2.Checked;
end;

procedure TForm1.Edit2KeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then kpFileListBox1.Mask := Edit2.Text;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   kpFileListBox1.GetFilesSelected(ListBox1.Items);
end;

procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
   with (Control as TListBox), Canvas do begin
     if Items.Objects[Index] <> nil then
          Font.Color := clRed
     else Font.Color := clBlack;
     Rect.Left := Rect.Left + 5; 
     DrawText(Canvas.Handle, PChar(Items[Index]),
              Length(Items[Index]), Rect, DT_LEFT  or DT_VCENTER );
   end;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
   kpFileListView1.ShowFolders := CheckBox3.Checked;
end;

procedure TForm1.Edit3KeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then kpFileListView1.Mask := Edit3.Text;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
   kpFileListView1.ViewStyle := vsIcon;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
   kpFileListView1.ViewStyle := vsSmallIcon;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
   kpFileListView1.ViewStyle := vsList;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
   kpFileListView1.ViewStyle := vsReport;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
   kpFileListView1.GetFilesSelected(ListBox2.Items);
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
    kpFolderTreeView1.CreateFolder(False);
end;

procedure TForm1.SpeedButton8Click(Sender: TObject);
begin
   kpFileListBox1.OneLevelUp;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
   kpFileListView1.OneLevelUp;
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
    kpFileListView1.ShowFolders := True;
    CheckBox3.Checked := True;
    kpFileListView1.CreateFolder;
end;

procedure TForm1.SpeedButton9Click(Sender: TObject);
begin
    kpFileListBox1.ShowFolders := True;
    CheckBox2.Checked := True;
    kpFileListBox1.CreateFolder;
end;

procedure TForm1.SpeedButton10Click(Sender: TObject);
begin
   kpFolderCombo1.OneLevelUp;
end;

procedure TForm1.SpeedButton11Click(Sender: TObject);
begin
   kpFileListView2.CreateFolder;
end;

procedure TForm1.kpFileListBox1FileSelect(Sender: TObject; Name: String);
begin
   ShowMessage('Выбран файл ' + Name);
end;

procedure TForm1.kpFileListView1FileSelect(Sender: TObject; Name: String);
begin
   ShowMessage('Выбран файл ' + Name);
end;

function FormatFileSize(ByteSize: extended): string;
begin
   if ByteSize <= 0 then
      Result := ''
   else
   if ByteSize < 1024 then
      Result := Format ( '%0.f байт', [ByteSize] )
   else
     if (ByteSize <= 1024*1024) then
        Result := Format( '%.1f Кбайт', [ByteSize / 1024] )
     else
     if (ByteSize > 1024*1024) and (ByteSize <= 1024*1024*1024) then
        Result := Format ( '%.1f Мбайт', [ByteSize / (1024*1024)] )
     else
        Result := Format ( '%.1f Гбайт', [ByteSize / (1024*1024*1024)] );
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var Total, SelCount: integer;
    TotalSize, SelSize: extended;
begin
   Total := kpFileListView1.ItemsCount;
   TotalSize := kpFileListView1.TotalSize;
   SelCount := kpFileListView1.SelCount;
   SelSize := kpFileListView1.SelSize;
   if SelCount > 0 then begin
      StatusBar1.Panels[0].Text := 'Выбрано объектов: '+IntToStr(SelCount);
      StatusBar1.Panels[1].Text := FormatFileSize(SelSize);
   end
   else begin
      StatusBar1.Panels[0].Text := 'Объектов: '+IntToStr(Total);
      StatusBar1.Panels[1].Text := FormatFileSize(TotalSize);
   end;

   Total := kpFileListView2.ItemsCount;
   TotalSize := kpFileListView2.TotalSize;
   SelCount := kpFileListView2.SelCount;
   SelSize := kpFileListView2.SelSize;
   if SelCount > 0 then begin
      StatusBar2.Panels[0].Text := 'Выбрано объектов: '+IntToStr(SelCount);
      StatusBar2.Panels[1].Text := FormatFileSize(SelSize);
   end
   else begin
      StatusBar2.Panels[0].Text := 'Объектов: '+IntToStr(Total);
      StatusBar2.Panels[1].Text := FormatFileSize(TotalSize);
   end;

   Total := kpFileListBox1.ItemsCount;
   TotalSize := kpFileListBox1.TotalSize;
   SelCount := kpFileListBox1.SelCount;
   SelSize := kpFileListBox1.SelSize;
   if SelCount > 0 then begin
      StatusBar3.Panels[0].Text := 'Выбрано объектов: '+IntToStr(SelCount);
      StatusBar3.Panels[1].Text := FormatFileSize(SelSize);
   end
   else begin
      StatusBar3.Panels[0].Text := 'Объектов: '+IntToStr(Total);
      StatusBar3.Panels[1].Text := FormatFileSize(TotalSize);
   end;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
   kpFolderTreeView1.Notify := CheckBox4.Checked;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
   kpFileListBox1.Notify := CheckBox5.Checked;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
   kpFileListView1.Notify := CheckBox6.Checked;
end;

procedure TForm1.CheckBox7Click(Sender: TObject);
begin
   kpFolderCombo1.NetBrowse := CheckBox7.Checked;
   kpFileListView2.NetBrowse := CheckBox7.Checked;
end;

procedure TForm1.CheckBox8Click(Sender: TObject);
begin
   kpFolderCombo1.Notify := CheckBox8.Checked;
   kpFileListView2.Notify := CheckBox8.Checked;
end;

procedure TForm1.kpFileListView2FileRename(Sender: TObject; OldName,
  NewName: String; Success, IsFolder: Boolean);
var FType: string;
begin
    if IsFolder then
         FType := 'Каталог '
    else FType := 'Файл ';
    if Success then
       MessageDlg(FType + OldName + ' переименован в ' + NewName,
                   mtInformation, [mbOK], 0 )
    else
       MessageDlg('Не удалось переименовать ' + FType + OldName + ' в ' + NewName,
                   mtError, [mbOK], 0 );
end;

end.
