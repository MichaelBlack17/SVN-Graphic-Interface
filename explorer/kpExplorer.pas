{*******************************************************}
{                                                       }
{         Delphi VCL Components                         }
{         for selecting/creating/viewing a folder       }
{         Explorer-like style                           }
{                                                       }
{         version 2.45                                  }
{                                                       }
{         Supports                                      }
{           - file notification for fixed and           }
{             RAM-drives (optional)                     }
{           - network browser (optional)                }
{                                                       }
{         Copyright (c) 1998-2012 Konstantin Polyakov   }
{                 mailto: kpolyakov@mail.ru             }
{                                                       }
{*******************************************************}
unit kpExplorer;

{$IFDEF VER140}   {Borland Delphi 6.0}
{$DEFINE D6PLUS}
{$ENDIF}
{$IFDEF VER150}   {Borland Delphi 7.0}
{$DEFINE D6PLUS}
{$ENDIF}

{$IFDEF D6PLUS}   {Borland Delphi 6.0+}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CommCtrl, Buttons, ExtCtrls, ShellAPI, ShlObj, StdCtrls, FileCtrl;

const
  SErrAllocHandle = 'Невозможно установить обработчик нотификаций';

  SErrorMsgCaption = 'Ошибка';
  SNoAccessToDrive = 'Нет доступа к устройству';
  SDriveNotReady   = 'Устройство не готово.';

  SByteName  = 'байт';
  SKByteName = 'Кб';
  SMByteName = 'Мб';
  SGByteName = 'Гб';
  SFileWord  = 'Файл';

  SFolderQueryCaption = 'Создание новой папки';
  SFolderQueryMsg = 'Имя новой папки';
  SCreateFailure  = 'Невозможно создать каталог';
  SNoDiskInDrive  = 'В дисководе %s:\ нет диска.';

  SDriveRemovable = 'Съемный диск';
  SDriveFixed     = 'Локальный диск';
  SDriveRemote    = 'Сетевой диск';
  SDriveCDROM     = 'Компакт-диск';
  SDriveRAM       = 'Виртуальный диск';
  SDriveUnknown   = 'Неизвестный тип';

type
{**********************************************************
                FILE NOTIFY
**********************************************************}
{------------------------------------------------------------
  Fnugry File Notify Component
  Copyright (C) 1997 Gleb Yourchenko
  Version 1.0.1.1

  Contact:
  Gleb Yourchenko
  E-mail: eip__@hotmail.com
------------------------------------------------------------}
  EFileNotificationError = class(Exception);

  TNotificationOption = (fnoFile, fnoFolder, fnoAttr,
    fnoSize, fnoTime, fnoWatchSubtree);

  TNotificationOptions = set of TNotificationOption;

  TFileNotifyEvent = procedure(Sender :TObject;
    Action :TNotificationOption) of object;

  TFileNotify = class;

{----------------- FILE NOTIFICATION THREAD ----------------------}
  TNotificationThread = class(TThread)
  private
    FOwner             :TFileNotify;
    FHandles           :array[0..7] of THandle;
    FHandleFilters     :array[0..7] of TNotificationOption;
    FHandleCount       :Integer;
    FActiveFilter      :TNotificationOption;
    procedure ReleaseHandle;
    procedure AllocateHandle;
  protected
    procedure Execute; override;
    procedure Notify;
  public
    constructor Create(Owner :TFileNotify);
    destructor Destroy; override;
    procedure Reset;
  end;

{----------------- FILE NOTIFY ----------------------}
  TFileNotify = class
  private
    FThread      :TNotificationThread;
    FAsyncNotify :Boolean;
    FFolder      :String;
    FOptions     :TNotificationOptions;
    FOnNotify    :TFileNotifyEvent;
    procedure SetOptions(const Value :TNotificationOptions);
    procedure SetRootFolder(const Value :String);
    procedure SetAsyncNotify(Value :Boolean);
  protected
    procedure Notify(Action :TNotificationOption); virtual;
    procedure Prepare;
    procedure StartMonitoring;
    procedure StopMonitoring;
  public
    constructor Create(AOwner :TCOmponent;
                        AOptions :TNotificationOptions );
    destructor Destroy; override;
    property   OnNotify :TFileNotifyEvent read FOnNotify write FOnNotify;
    property   Options  :TNotificationOptions read FOptions write SetOptions;
    property   Folder   :String read FFolder write SetRootFolder;
    property   AsyncNotify :Boolean read FAsyncNotify write SetAsyncNotify;
  end;

{**********************************************************
                AUXILIARY DATA TYPES
**********************************************************}

  PFileObject = ^TFileObject;
  TFileObject = record
     DisplayName: string;
     ParsingName: string;
     Description: string;
     Size: Extended;
     FreeSize: Extended;
     DateTime: TDateTime;
     Attrs: UINT;
     DriveType: integer;
     ImageIndex: integer;
     OverlayIndex: integer;
     NetResource: PNetResource;
     FileNotify: TFileNotify;
  end;

  TFileObjType = (fotDirectory, fotFiles, fotVirtual);
  TFileObjTypes = set of TFileObjType;

  TFileList = class(TList)
  protected
//    function  NewFileObject(FindData: TWin32FindData): PFileObject;
//    function  NewNetObject(NetItem: PNetResource): PFileObject;
    function  AddFile(FilePath: string): PFileObject;
    function  FillByFiles(Location: string; FileTypes: TFileObjTypes): Boolean;
    procedure FillByDrives(DoFreshDrives: Boolean);
    procedure FillFromNethood(Root: PNetResource);
  public
    destructor Destroy; override;
{$IFDEF VER100} { Borland Delphi 3.0 }
    procedure Clear;
{$ELSE}
    procedure Clear; override;
{$ENDIF}
    procedure Delete(Index: Integer);
    procedure Sort;
  end;

  TFileAttr = (ftReadOnly, ftHidden, ftSystem, ftVolumeID, ftDirectory,
               ftArchive, ftNormal);
  TFileType = set of TFileAttr;

  TkpFolderTreeView = class;
  TkpFolderCombo = class;

{**********************************************************
                KP FILE LIST BOX
**********************************************************}
  TFolderCreatedEvent = procedure(Sender: TObject; Name: string) of object;
  TFileSelectEvent = procedure (Sender: TObject; Name: string) of object;
  TFileListChangeEvent = procedure (Sender: TObject) of object;

  TkpFileListBox = class(TCustomListBox)
  private
    FAltColor:      TColor;
    FMask:          string;
    FDirectory:     string;
    FDirLabel:      TLabel;
    FFileNotify:    TFileNotify;
    FFileType:      TFileType;
    FFileList:      TFileList;
    FFreshDrives:    Boolean;
    FFolderTree:    TkpFolderTreeView;
    FFolderCombo:   TkpFolderCombo;
    FIsRefreshing:  Boolean;
    FNeedRefresh:   Boolean;
    FNetBrowse:     Boolean;
    FNotification:  Boolean;
    FUpdateCount:   integer;
    FWaitFirstNotify: Boolean;

    FPrevIconIndex: Integer;
    FIconBitmap:    TBitmap;
    FTempBitmap:    TBitmap;
    FIconWidth:     Integer;
    FIconHeight:    Integer;
    FTextOffset:    Integer;
    FReadOnly:      Boolean;
    FFoldersOnly:   Boolean;

    FOnFileSelect:        TFileSelectEvent;
    FOnFileListChange:    TFileListChangeEvent;
    FOnFolderCreated:     TFolderCreatedEvent;

  protected
           { SET procedures }
    procedure   SetAltColor(const Value: TColor);
    procedure   SetDirLabel(Value: TLabel);
    procedure   SetFileType(NewFileType: TFileType);
    procedure   SetFolderTree(const Value: TkpFolderTreeView);
    procedure   SetFolderCombo(const Value: TkpFolderCombo);
    procedure   SetMask(Value: string);
    procedure   SetNetBrowse(Value: Boolean);
    procedure   SetNotification(Value: Boolean);
    procedure   SetShowFolders(Value: Boolean);
    procedure   SetFoldersOnly(Value: Boolean);
           { GET functions }
    function    GetDirectory: string;
    function    GetFileName: string;
    function    GetItemsCount: integer;
    function    GetFileSize(Index: integer): extended;
    function    GetSelCount: integer;
    function    GetSelSize: extended;
    function    GetShowFolders: Boolean;
    function    GetTotalSize: extended;
           { overridden methods }
    procedure   DblClick; override;
    procedure   DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);  override;
    procedure   KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
           { additional methods }
    procedure   AssignFileList;
    function    CanBrowseNet: Boolean;
    procedure   BeginUpdate;
    procedure   EndUpdate;
    procedure   FreshList(NewDir: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   CreateFolder;
    procedure   GetFilesSelected(List: TStrings);
    procedure   OneLevelUp;
    procedure   Refresh;
    function    IsSpecialFolder: Boolean;

  published
          { inherited properties }
    property Align;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property IntegralHeight;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
          { inherited events }
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
          { specific published methods }
    procedure ChangeFileSystem(Sender :TObject; Action :TNotificationOption);
    function  IsRefreshing: Boolean;
    procedure SetDirectory(NewDirectory: string);
          { specific published properties }
    property AltColor: TColor read FAltColor write SetAltColor;
    property DirLabel: TLabel read FDirLabel write SetDirLabel;
    property Directory: string read GetDirectory write SetDirectory;
    property FileName: string read GetFileName;
    property FileType: TFileType read FFileType write SetFileType;
    property FreshDrives: Boolean read FFreshDrives write FFreshDrives default False;
    property FolderTree: TkpFolderTreeView read FFolderTree write SetFolderTree;
    property FolderCombo: TkpFolderCombo read FFolderCombo write SetFolderCombo;
    property ItemsCount: integer read GetItemsCount;
    property Mask: string read Fmask write SetMask;
    property NetBrowse: Boolean read FNetBrowse write SetNetBrowse default False;
    property Notify: Boolean read FNotification write SetNotification default False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property SelCount: integer read GetSelCount;
    property SelSize: extended read GetSelSize;
    property ShowFolders: Boolean read GetShowFolders write SetShowFolders;
    property FoldersOnly: Boolean read FFoldersOnly write SetFoldersOnly;
    property TotalSize: extended read GetTotalSize;
          { specific events }
    property OnFileSelect: TFileSelectEvent read FOnFileSelect write FOnFileSelect;
    property OnFileListChange: TFileListChangeEvent read FOnFileListChange
                                        write FOnFileListChange;
    property OnFolderCreated: TFolderCreatedEvent read FOnFolderCreated write FOnFolderCreated;
  end;

{**********************************************************
                KP FILE LIST VIEW
**********************************************************}
  TNameSpace = (nsNone, nsFiles, nsDrives, nsMisc);
  TFileRenameEvent = procedure (Sender: TObject;
                       OldName, NewName: string; Success, IsFolder: Boolean) of object;

  TkpFileListView = class(TCustomListView)
  private
    FAltColor:        TColor;
    FDirectory:       string;
    FDirLabel:        TLabel;
    FFileNotify:      TFileNotify;
    FFileType:        TFileType;
    FFileList:        TFileList;
    FFreshDrives:      Boolean;
    FFolderTree:      TkpFolderTreeView;
    FFolderCombo:     TkpFolderCombo;
    FIsEditingNew:    Boolean;
    FIsRefreshing:    Boolean;
    FMask:            string;
    FNameSpace:       TNameSpace;
    FNetBrowse:       Boolean;
    FNotification:    Boolean;
    FNeedRefresh:     Boolean;
    FSaveWndProc:     TWndMethod;
    FSortOrder:       array [0..4] of integer;
    FUpdateCount:   integer;
    FWaitFirstNotify: Boolean;
    FFoldersOnly:     Boolean;

    FUserOnEdited:     TLVEditedEvent;
    FOnFileSelect:     TFileSelectEvent;
    FOnFileListChange: TFileListChangeEvent;
    FOnFileRename:     TFileRenameEvent;
    FOnFolderCreated:  TFolderCreatedEvent;

  protected
           { SET functions }
    procedure   SetAltColor(const Value: TColor);
    procedure   SetDirLabel(Value: TLabel);
    procedure   SetFileType(NewFileType: TFileType);
    procedure   SetMask(Value: string);
    procedure   SetFolderTree(const Value: TkpFolderTreeView);
    procedure   SetFolderCombo(const Value: TkpFolderCombo);
    procedure   SetNetBrowse(Value: Boolean);
    procedure   SetNotification(Value: Boolean);
    procedure   SetShowFolders(Value: Boolean);
    procedure   SetFoldersOnly(Value: Boolean);
    procedure   SetNameSpace(Value: TNameSpace);
           { GET functions }
    function    GetDirectory: string;
    function    GetFileName: string;
    function    GetFileSize(Index: integer): extended;
    function    GetItemsCount: integer;
    function    GetSelCount: integer;
    function    GetSelSize: extended;
    function    GetTotalSize: extended;
    function    GetShowFolders: Boolean;
           { overridden methods }
    function    CanEdit(Item: TListItem): Boolean; override;
    procedure   Edit(const Item: TLVItem); override;
    procedure   ColClick(Column: TListColumn); override;
    procedure   CreateWnd; override;
    procedure   DblClick; override;
    procedure   KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure   OwnWndProc(var Message: TMessage);
           { custom draw methods }
    procedure   CNNotify(var msg: TWMNotify); message CN_NOTIFY;
    procedure   GetCustomDrawColors(ItemNo, SubItemNo: DWORD;
                                    var ColText, ColBk: TColor);
           { additional methods }
    procedure   AssignFileList;
    procedure   BeginUpdate;
    function    CanBrowseNet: Boolean;
    procedure   CompareItems(Sender: TObject; Item1, Item2: TListItem;
                                     Data: Integer; var Compare: Integer);
    procedure   EndUpdate;
    procedure   FreshList(NewDir: string);
    procedure   NewOnEdited(Sender: TObject; var Item: TListItem; var S: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    FileName: string read GetFileName;
    procedure   GetFilesSelected(List: TStrings);
    procedure   CreateFolder;
    procedure   OneLevelUp;
    procedure   Refresh;
    function    IsSpecialFolder: Boolean;

  published
          { inherited properties }
    property Align;
    property BorderStyle;
    property Color;
    property OnClick;
    property OnDblClick;
    property Ctl3D;
    property DragMode;
    property ReadOnly default False;
    property Enabled;
    property Font;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property IconOptions;
    property Checkboxes;
    property AllocBy;
    property MultiSelect;
    property RowSelect;
    property OnChange;
    property OnChanging;
    property OnColumnClick;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnDragDrop;
    property OnDragOver;
    property DragCursor;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
          { inherited events }
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
          { specific published methods }
    procedure ChangeFileSystem(Sender :TObject; Action :TNotificationOption);
    function  IsRefreshing: Boolean;
    procedure SetDirectory(NewDirectory: string);
          { specific published properties }
    property AltColor: TColor read FAltColor write SetAltColor;
    property DirLabel: TLabel read FDirLabel write SetDirLabel;
    property Directory: string read GetDirectory write SetDirectory;
    property FileType: TFileType read FFileType write SetFileType;
    property FreshDrives: Boolean read FFreshDrives write FFreshDrives default False;
    property FolderTree: TkpFolderTreeView read FFolderTree write SetFolderTree;
    property FolderCombo: TkpFolderCombo read FFolderCombo write SetFolderCombo;
    property ItemsCount: integer read GetItemsCount;
    property Mask: string read Fmask write SetMask;
    property NetBrowse: Boolean read FNetBrowse write SetNetBrowse default False;
    property Notify: Boolean read FNotification write SetNotification default False;
    property SelCount: integer read GetSelCount;
    property SelSize: extended read GetSelSize;
    property ShowFolders: Boolean read GetShowFolders write SetShowFolders;
    property FoldersOnly: Boolean read FFoldersOnly write SetFoldersOnly;
    property TotalSize: extended read GetTotalSize;
          { specific events }
    property OnEdited: TLVEditedEvent read FUserOnEdited write FUserOnEdited;
    property OnFileSelect: TFileSelectEvent read FOnFileSelect write FOnFileSelect;
    property OnFileListChange: TFileListChangeEvent read FOnFileListChange
                                        write FOnFileListChange;
    property OnFileRename: TFileRenameEvent read FOnFileRename write FOnFileRename;
    property OnFolderCreated: TFolderCreatedEvent read FOnFolderCreated write FOnFolderCreated;
  end;

{**********************************************************
                TCOMPONENT HACK
**********************************************************}
{$HINTS OFF}
  TComponent_ = class(TPersistent)
  private
    FOwner: TComponent;
    FName: TComponentName;
    FTag: Longint;
    FComponents: TList;
    FFreeNotifies: TList;
    FDesignInfo: Longint;
    FComponentState: TComponentState;
    ///
  end;
{$HINTS ON}

{**********************************************************
                DIRECTORY TREE VIEW
**********************************************************}
  TTVNewEditCancelEvent = procedure( Sender: TObject;
                          Node: TTreeNode; var Delete: Boolean) of object;
  TTVBeginCreatingEvent = procedure( Sender: TObject; Node: TTreeNode) of object;
  TTVEndCreatingEvent = procedure( Sender: TObject;   Node: TTreeNode) of object;
  TCustomDrawState = set of (cdsSelected, cdsGrayed, cdsDisabled, cdsChecked,
                     cdsFocused, cdsDefault, cdsHot, cdsMarked, cdsIndeterminate);

  TkpFolderTreeView = class(TCustomTreeView)
  private
    FAltColor:      TColor;
    FDirectory:     string;
    FDirLabel:      TLabel;
    FFreshDrives:    Boolean;
    FIsAddingTree:  Boolean;
    FIsEditingNew:  Boolean;
    FIsSettingPath: Boolean;
    FIsSettingParent: Boolean;
    FMouseInControl: Boolean;
    FMyComputerIndex: integer;
    FNeedRefresh:   Boolean;
    FNetBrowse:     Boolean;
    FNethoodIndex:  integer;
    FNotification:  Boolean;
    FNotifyList:    TList;
    FTimer:         TTimer;
    FUpdateCount:   integer;
    FViewList:      TList;

    FOnEditCancel:    TTVChangedEvent;
    FOnNewEditCancel: TTVNewEditCancelEvent;
    FUserOnEdited:    TTVEditedEvent;
    FOnBeginCreating: TTVBeginCreatingEvent;
    FOnEndCreating:   TTVEndCreatingEvent;
    FOnFileRename:    TFileRenameEvent;
    FOnFolderCreated: TFolderCreatedEvent;

  protected
           { SET procedures }
    procedure SetAltColor(const Value: TColor);
    procedure SetDirLabel(Value: TLabel);
    procedure SetDirectory(NewDirectory: string);
    procedure SetNetBrowse(Value: Boolean);
    procedure SetNotification(Value: Boolean);
           { GET procedures }
    function  GetDirectory: string;
           { overridden methods }
    function  CanChange(Node: TTreeNode): Boolean; override;
    function  CanEdit(Node: TTreeNode): Boolean; override;
    function  CanExpand(Node: TTreeNode): Boolean; override;
    procedure Change(Node: TTreeNode); override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Collapse(Node: TTreeNode); override;
    procedure Edit(const Item: TTVItem); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
                             Operation: TOperation); override;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMouseMove(var Msg: TWMMouseMove); message WM_MOUSEMOVE;
    procedure SetParent(AParent: TWinControl); override;
            { custom draw support }
    procedure CNNotify(var msg: TWMNotify); message CN_NOTIFY;
    procedure GetCustomDrawColors(Node: TTreeNode;
                   State: TCustomDrawState; var ColText, ColBk: TColor);
    function  IsAltColor(Node: TTreeNode): Boolean;
            { additional methods }
    function  AddDirectories(Node: TTreeNode): Boolean;
    procedure AddDummyChild(Node: TTreeNode);
    function  AddNetworkChildren(Node: TTreeNode): Boolean;
    procedure AddViewChild(ViewObject: TComponent);
    function  AllowCreation: Boolean;
    procedure BeginUpdate;
    property  CanCreateNode: Boolean read AllowCreation;
    procedure ChangeFileSystem(Sender :TObject; Action :TNotificationOption);
    procedure DelayedFreshing;
    procedure DeleteDummyChild(Node: TTreeNode);
    procedure EndUpdate;
    procedure ExpandPath(Path: string; ExpandNode: Boolean);
    function  FillFileList(Path: string; FileList: TFileList): Boolean;
    procedure FreshLinks;
    procedure ChangeNotifyLinks;
    function  GetDiskParentNode(Node: TTreeNode): TTreeNode;
    function  GetNodePath(Node: TTreeNode): string;
    function  IsDroppedDown: Boolean;
    property  IsEditingNew: Boolean read FIsEditingNew  write FIsEditingNew;
    function  IsNetworkContainer(Node: TTreeNode): Boolean;
    function  IsNetworkDomain(Node: TTreeNode): Boolean;
    function  IsNetworkEntity(Node: TTreeNode; EntityType: DWORD): Boolean;
    function  IsNetworkNetwork(Node: TTreeNode): Boolean;
    function  IsNetworkNode(Node: TTreeNode): Boolean;
    function  IsNetworkShare(Node: TTreeNode): Boolean;
    function  IsNetworkServer(Node: TTreeNode): Boolean;
    procedure NewOnDeletion(Sender: TObject; Node: TTreeNode);
    procedure NewOnEdited(Sender: TObject; Node: TTreeNode; var S: string);
    function  NewChildAndEdit(Node: TTreeNode; const S: String): TTreeNode;
    procedure OneLevelUp;
    procedure OnTimer(Sender: TObject);
    procedure RemoveViewChild(ViewObject: TComponent);

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   CreateFolder(GoInside: Boolean);
    procedure   Refresh;
    function    IsSpecialFolder: Boolean;

  published
           { inherited properties }
    property Align;
    property BorderStyle;
    property Ctl3D;
    property Color;
    property DragCursor;
    property Enabled;
    property Font;
    property Indent;
    property HideSelection;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RightClickSelect;
    property ShowHint;
    property ShowButtons;
    property ShowLines;
    property TabOrder;
    property TabStop default True;
    property Visible;
           { inherited events }
    property OnChange;
    property OnChanging;
    property OnCollapsing;
    property OnCompare;
    property OnEditing;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
           { specific properties }
    property AltColor: TColor read FAltColor write SetAltColor;
    property DirLabel: TLabel read FDirLabel write SetDirLabel;
    property Directory: string read GetDirectory write SetDirectory;
    property NetBrowse: Boolean read FNetBrowse write SetNetBrowse default False;
    property Notify: Boolean read FNotification write SetNotification default False;
    property FreshDrives: Boolean read FFreshDrives write FFreshDrives default False;

           { specific events }
    property OnEditCancel: TTVChangedEvent read FOnEditCancel write FOnEditCancel;
    property OnNewEditCancel: TTVNewEditCancelEvent read FOnNewEditCancel write FOnNewEditCancel;
    property OnEdited: TTVEditedEvent read FUserOnEdited write FUserOnEdited;
    property OnBeginCreating: TTVBeginCreatingEvent read FOnBeginCreating write FOnBeginCreating;
    property OnEndCreating:   TTVEndCreatingEvent read FOnEndCreating write FOnEndCreating;
    property OnFileRename: TFileRenameEvent read FOnFileRename write FOnFileRename;
    property OnFolderCreated: TFolderCreatedEvent read FOnFolderCreated write FOnFolderCreated;

  end;


{**********************************************************
        FOLDER COMBO
**********************************************************}
  TkpFolderCombo = class(TCustomEdit)
  private
    FAltColor:   TColor;
    FBtn:        TSpeedButton;
    FCanvas:     TControlCanvas;
    FDirectory:  string;
    FDirLabel:   TLabel;
    FFreshDrives: Boolean;
    FImageIndex: integer;
    FIndent:     integer;
    FIsSettingParent: Boolean;
    FNotify:     Boolean;
    FNetBrowse:  Boolean;
    FRightClickSelect: Boolean;
    FShowButtons: Boolean;
    FShowLines:  Boolean;
    FTree:       TkpFolderTreeView;

  protected
           { SET procedures }
    procedure SetAltColor(const Value: TColor);
    procedure SetDirectory(const Value: string);
    procedure SetDirLabel(Value: TLabel);
    procedure SetFreshDrives(Value: Boolean);
    procedure SetIndent(Value: Integer);
    procedure SetNetBrowse(Value: Boolean);
    procedure SetNotification(Value: Boolean);
    procedure SetRightClickSelect(Value: Boolean);
    procedure SetShowButtons(Value: Boolean);
    procedure SetShowLines(Value: Boolean);
    procedure SetText(Value: string);
           { GET functions }
    function  GetDirectory: string;
    function  GetText: string;
           { overridden methods }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMNCHitTest(var Msg: TMessage); message WM_NCHITTEST;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure SetParent(AParent: TWinControl); override;
           { specific methods }
    procedure BtnClick(Sender: TObject);
    procedure CancelBrowse;
    procedure EndBrowse;
           { specific protected properties }
    property  ImageIndex: integer read FImageIndex  write FImageIndex;
    property  Text: string read Gettext write SetText;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   OneLevelUp;
    procedure   Refresh;
    procedure   SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function    IsSpecialFolder: Boolean;

  published
          { inherited properties }
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
          { inherited events }
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
          { specific properties }
    property AltColor: TColor read FAltColor write SetAltColor;
    property DirLabel: TLabel read FDirLabel write SetDirLabel;
    property Directory: string read GetDirectory write SetDirectory;
    property FreshDrives: Boolean read FFreshDrives write SetFreshDrives default False;
    property Indent: Integer read FIndent write SetIndent;
    property NetBrowse: Boolean read FNetBrowse write SetNetBrowse default False;
    property Notify: Boolean read FNotify write SetNotification default False;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons default True;
    property ShowLines: Boolean read FShowLines write SetShowLines default True;
    property RightClickSelect: Boolean read FRightClickSelect write SetRightClickSelect default False;
  end;


//############################################################
procedure SetDirLabelCaption(DirLabel: TLabel; Directory: string; MaxWidth: integer);
function  HasSubdirs(DirName: string): Boolean;
function  DiskInDrive(Drive: Char): Boolean;
function  GetFileSysIconIndex(Filename: String; Modifier: integer): Integer;
function  GetSpecIconIndex(Index: integer): integer;
function  GetFileDisplayName(Filename: String): string;
function  GetPIDLIconIndex(Folder: integer): Integer;
function  GetPIDLDisplayName(Folder: integer): string;

procedure Register;

//############################################################
implementation

uses Registry;

{$R KPEXPICONS.RES}

{--------------- CUSTOM DRAW SUPPORT ----------------------------}
const NM_CUSTOMDRAW = (NM_FIRST-12);
      CDDS_PREPAINT = $000000001;
      CDDS_ITEM          = $000010000;
      CDDS_SUBITEM       = $00020000;
      CDDS_ITEMPREPAINT  = (CDDS_ITEM OR CDDS_PREPAINT);
      CDRF_DODEFAULT        = $00000000;
      CDRF_NOTIFYITEMDRAW   = $00000020;
      CDRF_NOTIFYSUBITEMDRAW = $00000020;

type PNMCustomDrawInfo = ^TNMCustomDrawInfo;
     TNMCustomDrawInfo = packed record
        hdr : TNMHDR;
        dwDrawStage : LONGINT;
        hdc : HDC;
        rc : TRect;
        dwItemSpec : LONGINT;  // this is control specific
        uItemState : Cardinal;
        lItemlParam : Longint;
     end;
     PNMLVCustomDraw = ^TNMLVCustomDraw;
     TNMLVCustomDraw = packed record
        nmcd : TNMCustomDrawInfo;
        clrText : COLORREF;
        clrTextBk : COLORREF;
        iSubItem: Integer;
     end;
     PNMTVCustomDraw = ^TNMTVCustomDraw;
     TNMTVCustomDraw = packed record
        nmcd : TNMCustomDrawInfo;
        clrText : COLORREF;
        clrTextBk : COLORREF;
        iLevel: Integer;
     end;
{----------------------------------------------------------------}

{ TkpImageList }
  TkpImageList = class(TImageList)
  public
    function AddIcon(Image: TIcon): Integer;
  end;

function TkpImageList.AddIcon(Image: TIcon): Integer;
begin
  if Image = nil then
    Result := Add(nil, nil)
  else
  begin
//    CheckImage(Image);
    Result := ImageList_AddIcon(Handle, Image.Handle);
  end;
  Change;
end;

const
     DefaultMask     = '*.*';
     DummyChildName  = '.';
     SpecDirAlias    = '?.';
     DesktopAlias    = '?.0';
     MyComputerAlias = '?.1';
     NethoodAlias    = '?.2';
     NetworkAlias    = '?.3';
     DomainAlias     = '?.4';
     ServerAlias     = '?.5';
     ShareAlias      = '?.6';

var
     SysSmallImageList: TImageList;
     SysLargeImageList: TImageList;
     kpSmallImageList:  TkpImageList;
     kpLargeImageList:  TkpImageList;
     FirstExtraIndex:   integer;
     LastExtraIndex:    integer;
     ExtIcons:          TStringList;

     DesktopName:       string;
     MyComputerName:    string;
     NethoodName:       string;
     FolderDescription: string;

     DeskTopIconIndex:        Integer;
     MyComputerIconIndex:     Integer;
     FolderIconIndex:         Integer;
     OpenFolderIconIndex:     Integer;
     ServerIconIndex:         Integer;
     ServerNoAccessIconIndex: Integer;
     NetworkIconIndex:        Integer;
     NethoodIconIndex:        Integer;
     DomainIconIndex:         Integer;
     Disk35IconIndex:         Integer;
     Disk525IconIndex:        Integer;


{--------------- DISK IN DRIVE ------------------}
function DiskInDrive(Drive: Char): Boolean;
var ErrorMode: WORD;
begin
  if Drive > 'Z' then Dec(Drive, 32);
  ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
  try
    Result := (DiskSize(Ord(Drive) - Ord('A') + 1) <> -1);
  finally
    SetErrorMode(ErrorMode);
  end;
end;

{--------------- GET DISK SIZE ------------------}
function GetDiskSize(Root: string): Extended;
var SectorPerCluster, BytesPerSector, FreeClusters, TotalClusters: DWORD;
begin
  Result := 0;
  if DiskInDrive(Root[1]) then begin
     GetDiskFreeSpace(PChar(Root), SectorPerCluster, BytesPerSector,
                                FreeClusters, TotalClusters);
     Result := TotalClusters * SectorPerCluster;
     Result := Result * BytesPerSector;
  end;
end;

{--------------- GET FREE DISK SIZE ------------------}
function GetFreeDiskSize(Root: string): Extended;
var SectorPerCluster, BytesPerSector, FreeClusters, TotalClusters: DWORD;
begin
  Result := 0;
  if DiskInDrive(Root[1]) then begin
     GetDiskFreeSpace(PChar(Root), SectorPerCluster, BytesPerSector,
                      FreeClusters, TotalClusters);
     Result := FreeClusters * SectorPerCluster;
     Result := Result * BytesPerSector;
  end;
end;


{--------------- PURE DIRECTORY ------------------}
function PureDirectory(Directory: string): string;
var p: Integer;
begin
    p := Pos(SpecDirAlias, Directory);
    if  p > 0 then begin
        Result := Copy(Directory,p+Length(SpecDirAlias)+1,999);
        p := Pos(SpecDirAlias, Result);
        if  p > 0 then Result := Copy(Result, 1, p-1);
    end
    else Result := Directory;
end;

{--------------- LAST DIRECTORY ------------------}
function LastDirectory(Directory: string): string;
var p: Integer;
begin
    while True do begin
      p := Pos('\', Directory);
      if p = 0 then break;
      Directory := Copy(Directory,p+1,999);
    end;
    Result := Directory;
end;

//-------------------------------------------------------
//    FREE FILE OBJECT
//-------------------------------------------------------
procedure FreeFileObject(Obj: PFileObject);
begin
  if Assigned(Obj) then
     with Obj^ do begin
        DisplayName := '';
        ParsingName := '';
        Description := '';
        if Assigned(NetResource) then begin
           with NetResource^ do begin
              if Assigned(lpLocalName) then StrDispose(lpLocalName);
              if Assigned(lpRemoteName) then StrDispose(lpRemoteName);
              if Assigned(lpComment) then StrDispose(lpComment);
              if Assigned(lpProvider) then StrDispose(lpProvider);
           end;
           Dispose(NetResource);
        end;
        FileNotify.Free;
        Dispose(Obj);
     end;
end;

//-------------------------------------------------------
//    FILE LIST
//-------------------------------------------------------
destructor TFileList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TFileList.Clear;
var i: integer;
begin  for i:=0 to Count-1 do Delete(0); end;

procedure TFileList.Delete(Index: Integer);
begin
  if Assigned(Items[Index]) then
     FreeFileObject( PFileObject(Items[Index]) );
  inherited;
end;

function IsFolder(Obj: PFileObject): Boolean;
begin
     Result := (faDirectory and Obj^.Attrs) <> 0;
end;

function CompareFiles(Item1, Item2: Pointer): Integer;
var Obj1, Obj2: PFileObject;
begin
   Obj1 := PFileObject(Item1);
   Obj2 := PFileObject(Item2);
   if IsFolder(Obj1) and not IsFolder(Obj2) then Result := - 1
   else
   if not IsFolder(Obj1) and IsFolder(Obj2) then Result := 1
   else Result := AnsiCompareFileName(Obj1^.DisplayName, Obj2^.DisplayName);
end;

procedure TFileList.Sort;
begin inherited Sort(CompareFiles); end;

{-------------------- COPY NET RESOURCE -------------------}
function CopyNetResource(NetItem: PNetResource): PNetResource;
begin
  Result := nil;
  if not Assigned(NetItem) then Exit;

  New(Result);
  System.Move(NetItem^, Result^, SizeOf(TNetResource));
  with NetItem^ do begin
     if lpLocalName <> nil then
        Result^.lpLocalName := StrNew(lpLocalName);
     if lpComment <> nil then
        Result^.lpComment := StrNew(lpComment);
     if lpRemoteName <> nil then
        Result^.lpRemoteName := StrNew(lpRemoteName);
     if lpProvider <> nil then
        Result^.lpProvider := StrNew(lpProvider);
  end;
end;

{-------------------- COPY FILE OBJECT -------------------}
function CopyFileObject(Obj: PFileObject): PFileObject;
begin
  New(Result);
  with Result^ do begin
     DisplayName := Obj^.DisplayName;
     ParsingName := Obj^.ParsingName;
     Description := Obj^.Description;
     Size := Obj^.Size;
     FreeSize := Obj^.FreeSize;
     DateTime := Obj^.DateTime;
     Attrs := Obj^.Attrs;
     DriveType := Obj^.DriveType;
     ImageIndex := Obj^.ImageIndex;
     OverlayIndex := Obj^.OverlayIndex;
     NetResource := CopyNetResource(Obj^.NetResource);
     FileNotify := nil;
  end;
end;

{-------------------- FIIL BY DRIVES -------------------}
procedure TFileList.FillByDrives(DoFreshDrives: Boolean);
var DriveNo: Byte;
    RootDirName: string;
    LogDrives: set of 0..25;
    NewObj: PFileObject;
    ErrorMode: WORD;
begin
    Clear;
    integer(LogDrives):=GetLogicalDrives;
    for DriveNo := 0 to 25 do
      if DriveNo in LogDrives then begin
         New(NewObj);
         RootDirName := Char(DriveNo+$41)+':\';
         with NewObj^ do begin
           DisplayName := GetFileDisplayName(RootDirName);;
           ParsingName := RootDirName;
//           Description := '';
           DateTime := 0;
           DriveType := GetDriveType(PChar(RootDirName));
           if (DriveType <> DRIVE_REMOVABLE) or
              (DoFreshDrives and DiskInDrive(Char(DriveNo+$41))) then begin
                ErrorMode := SetErrorMode(SEM_FailCriticalErrors); // Блокируем ошибку чтения
                Attrs := GetFileAttributes(PChar(RootDirName));
                SetErrorMode(ErrorMode);                          // Восстановили обычный режим
                Size := GetDiskSize(RootDirName);
                FreeSize := GetFreeDiskSize(RootDirName);
           end
           else begin
                Attrs := 0;
                Size := 0;
                FreeSize := 0;
           end;
           case DriveType of
               DRIVE_REMOVABLE: Description := SDriveRemovable;
               DRIVE_FIXED:     Description := SDriveFixed;
               DRIVE_REMOTE:    Description := SDriveRemote;
               DRIVE_CDROM:     Description := SDriveCDROM;
               DRIVE_RAMDISK:   Description := SDriveRAM
               else             Description := SDriveUnknown;
           end;
           if DriveType = DRIVE_REMOVABLE then begin
              if (Pos('3.5',DisplayName) > 0) or
                 (Pos('3,5 ',DisplayName) > 0) then
                   ImageIndex := Disk35IconIndex
              else ImageIndex := Disk525IconIndex;
           end
           else
              ImageIndex := GetSpecIconIndex(GetFileSysIconIndex(RootDirName,0));
           OverlayIndex := -1;
           NetResource := nil;
           FileNotify := nil;
         end;
         Add(NewObj);
      end;
end;

{--------------- NEW FILE OBJECT --------------------}
function NewFileObject(FindData: TWin32FindData): PFileObject;
var LocalFileTime: TFileTime;
    Time: integer;
begin
    New(Result);
    with Result^ do begin
        DisplayName := FindData.cFileName;
        ParsingName := FindData.cFileName;
        Description := '';
        FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
        FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi, LongRec(Time).Lo);
        DateTime := FileDateToDateTime(Time);
        Size := FindData.nFileSizeHigh * MAXDWORD;
        Size := Size + FindData.nFileSizeLow;
        FreeSize := 0;
        Attrs := FindData.dwFileAttributes;
        DriveType := -1;
        if Attrs and faDirectory <> 0 then
             ImageIndex := FolderIconIndex
        else ImageIndex := -1;
        OverlayIndex := -1;
        NetResource := nil;
        FileNotify := nil;
    end;
end;
{--------------- FILL BY FILES --------------------}
function TFileList.FillByFiles(Location: string;
                               FileTypes: TFileObjTypes): Boolean;
var FindHandle: THandle;
    FindData: TWin32FindData;
begin
  Clear;
  if (Location[1] <> '\') and (not DiskInDrive(Location[1])) then begin
     Result := False;
     Exit;
  end;

  Result := True;
  FindHandle := FindFirstFile(PChar(Location+'*.*'), FindData);

  while FindHandle <> INVALID_HANDLE_VALUE do begin

     if (((fotDirectory in FileTypes) and
          (FILE_ATTRIBUTE_DIRECTORY and FindData.dwFileAttributes <> 0)) or
         ((fotFiles in FileTypes) and
          (FILE_ATTRIBUTE_DIRECTORY and FindData.dwFileAttributes = 0))) and
        (FindData.cFileName[0] <> '.') then
        Add(NewFileObject(FindData));

     if not FindNextFile(FindHandle, FindData) then begin
        Windows.FindClose(FindHandle);
        Sort;
        break;
     end;

  end;
end;

{--------------- ADD FILE --------------------}
function TFileList.AddFile(FilePath: string): PFileObject;
var FindHandle: THandle;
    FindData: TWin32FindData;
begin
  Result := nil;
  FindHandle := FindFirstFile(PChar(FilePath), FindData);
  if FindHandle <> INVALID_HANDLE_VALUE then begin
      Result := NewFileObject(FindData);
      Add(Result);
      Sort;
      Windows.FindClose(FindHandle);
  end;
end;

{--------------- NEW NET OBJECT --------------------}
function NewNetObject(NetItem: PNetResource): PFileObject;
begin
    New(Result);
    with Result^ do begin
        DisplayName := LastDirectory(NetItem^.lpRemoteName);
        ParsingName := DisplayName;
        if Assigned(NetItem^.lpComment) then
             Description := NetItem^.lpComment
        else Description := '';
        Size := 0;
        FreeSize := 0;
        DateTime := 0;
        Attrs := 0;
        DriveType := 0;
        ImageIndex := -1;
        case NetItem^.dwDisplayType of
          RESOURCEDISPLAYTYPE_NETWORK: ImageIndex := NetworkIconIndex;
          RESOURCEDISPLAYTYPE_DOMAIN:  ImageIndex := DomainIconIndex;
          RESOURCEDISPLAYTYPE_SERVER:  ImageIndex := ServerIconIndex;
          RESOURCEDISPLAYTYPE_SHARE:   ImageIndex := FolderIconIndex;
        end;
        OverlayIndex := -1 ;
        NetResource := CopyNetResource(NetItem);
        FileNotify := nil ;
    end;
end;

{--------------- FILL FROM NETHOOD --------------------}
procedure TFileList.FillFromNethood(Root: PNetResource);
type
  PNetResourceArray = ^TNetResourceArray;
  TNetResourceArray = array[0..MaxInt div SizeOf(TNetResource)-1]
                      of TNetResource;
var
  I, BufSize, NetResult, Count: Integer;
  Size: DWORD;
  NetHandle: THandle;
  NetResources: PNetResourceArray;
begin

  Clear;
  BufSize := 50 * SizeOf(TNetResource);
  GetMem(NetResources, BufSize);

  WNetOpenEnum(RESOURCE_GLOBALNET, RESOURCETYPE_DISK,
               RESOURCEUSAGE_CONNECTABLE or RESOURCEUSAGE_CONTAINER,
               Root, NetHandle);

  try
     while True do begin
          Count := - 1;
          Size := BufSize;
          NetResult := WNetEnumResource(NetHandle, DWORD(Count), NetResources, Size);
          if NetResult = ERROR_MORE_DATA then begin
             BufSize := Size;
             ReallocMem(NetResources, BufSize);
             Continue;
          end;
          if NetResult <> NO_ERROR then Break;

          for I := 0 to Count-1 do
             Add( NewNetObject(@NetResources[I]) );
     end;
//------------------------------------------------------------------
  finally
     WNetCloseEnum(NetHandle);
     FreeMem(NetResources, BufSize);
  end;

end;

{--------------- SIGN ------------------}
function sign(x: extended): integer;
begin
  Result := 0;
  if x > 0 then Result := 1;
  if x < 0 then Result := -1;
end;

{--------------- GET SPEC ICON INDEX ------------------}
{ with account for added network icons }
function GetSpecIconIndex(Index: integer): integer;
var AddCount: integer;
    Icon: TIcon;
begin
   Result := Index;
   if Result >= FirstExtraIndex then begin
      AddCount := LastExtraIndex-FirstExtraIndex+1;
      Inc(Result, AddCount);
      if Result > kpSmallImageList.Count-1 then begin
         Icon := TIcon.Create;
         try
           for Index:=kpSmallImageList.Count-AddCount to
                    SysSmallImageList.Count-1 do begin
              SysSmallImageList.GetIcon(Index, Icon);
              kpSmallImageList.AddIcon(Icon);
           end;
           for Index:=kpLargeImageList.Count-AddCount to
                    SysLargeImageList.Count-1 do begin
              SysLargeImageList.GetIcon(Index, Icon);
              kpLargeImageList.AddIcon(Icon);
           end;
         finally
           Icon.Free;
         end;
      end;
   end;
end;

{--------------- GET FILE SYS ICON INDEX ------------------}
function GetFileSysIconIndex(Filename: String; Modifier: integer): Integer;
var
  Fileinfo: TSHFileInfo;
begin
  Result := 0;
  if SHGetFileInfo(PChar(FileName),
        0, Fileinfo, sizeof(TSHFileInfo),
        SHGFI_ICON or SHGFI_SYSICONINDEX or Modifier ) <> 0 then begin
    Result := Fileinfo.IIcon;
  end;
end;

{--------------- GET PIDL ICON INDEX ------------------}
function GetPIDLIconIndex(Folder: integer): Integer;
var
  Fileinfo: TSHFileInfo;
  PIDL: PItemIDList;
begin
  Result := 0;
  SHGetSpecialFolderLocation(Application.Handle, Folder, PIDL);
  if SHGetFileInfo(PChar(PIDL), 0, Fileinfo, sizeof(TSHFileInfo),
         SHGFI_PIDL or SHGFI_SYSICONINDEX) <> 0 then
     Result := Fileinfo.IIcon;
end;

{--------------- GET FILE DISPLAY NAME ------------------}
function GetFileDisplayName(Filename: string): string;
var Fileinfo: TSHFileInfo;
begin
  Result := '';
  if SHGetFileInfo(PChar(FileName), 0, Fileinfo, sizeof(TSHFileInfo),
         SHGFI_DISPLAYNAME) <> 0 then
     Result := StrPas(FileInfo.szDisplayName);
end;

{--------------- GET PIDL DISPLAY NAME ------------------}
function GetPIDLDisplayName(Folder: integer): string;
var
  Fileinfo: TSHFileInfo;
  PIDL: PItemIDList;
begin
  Result := '';
  SHGetSpecialFolderLocation(Application.Handle, Folder, PIDL);
  if SHGetFileInfo(PChar(PIDL), 0, Fileinfo, sizeof(TSHFileInfo),
         SHGFI_PIDL or SHGFI_DISPLAYNAME) <> 0 then
     Result := StrPas(FileInfo.szDisplayName);
end;


{--------------- STR FILE SIZE ------------------}
function StrFileSize ( ByteSize: Extended ): string;
begin
  if ByteSize < 1 then Result := ''
  else
  if ByteSize < 1024 then
       Result := IntToStr(Round(ByteSize)) + ' ' + SByteName
  else
  if ByteSize < 1024*1024 then
     Result := IntToStr(Round(ByteSize/1024)) + ' ' + SKByteName
  else
     Result := IntToStr(Round(ByteSize/1024/1024)) + ' ' + SMByteName;
end;

{--------------- STR FILE DESCRIPTION ------------------}
function StrFileDescription ( Ext: string ): string;
{$J+}
const FolderDescription: string = '';
begin
  if (Ext = 'Directory') and (FolderDescription <> '') then begin
     Result := FolderDescription;
     Exit;
  end;
  Result := '';
  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKey(Ext, False) then begin
       Result := ReadString('');
       if OpenKey('\'+Result, False) then Result := ReadString('');
    end;
  finally
    Free;
    if (Ext = 'Directory') then FolderDescription := Result;
    if Result = '' then begin
       if (Length(Ext) > 0) and (Ext[1] = '.') then System.Delete(Ext,1,1);
       Result := SFileWord + ' ' + AnsiUpperCase(Ext);
    end;
  end;
end;
{$J-}

{--------------- STR FILE ATTR ------------------}
const faCompressed = $800;
function StrFileAttr ( Attr: integer ): string;
begin
   Result := '';
   if (faReadOnly and Attr) = faReadOnly then Result := Result + 'R';
   if (faHidden and Attr) = faHidden then Result := Result + 'H';
   if (faSysFile and Attr) = faSysFile then Result := Result + 'S';
   if (faArchive and Attr) = faArchive then Result := Result + 'A';
   if (faCompressed and Attr) = faCompressed then Result := Result + 'C';
end;

{--------------- GET FILE DESCRIPTION ------------------}
function GetFileDescription ( FileName: string ): string;
var Ext: string;
    i, p: integer;
begin
   Ext := AnsiLowerCase(ExtractFileExt(FileName));
   if Ext = '' then Ext := '.';
   Result := '';
   for i:=0 to ExtIcons.Count-1 do begin
      p := Pos(Ext+'|',ExtIcons[i]);
      if p <> 0 then begin
         p := Pos('|',ExtIcons[i]);
         Result := Copy(ExtIcons[i], p+1, 999);
         Exit;
      end;
   end;
   Result := StrFileDescription(Ext);
   ExtIcons.AddObject(Ext + '|' + Result,
        TObject(GetSpecIconIndex(GetFileSysIconIndex(FileName,0))));
end;

{--------------- GET FILE ICON INDEX ------------------}
function GetFileIconIndex ( FileName: string ): integer;
var Ext: string;
    i, p: integer;
begin
   Ext := AnsiLowerCase(ExtractFileExt(FileName));
   if Ext = '' then Ext := '.';
   for i:=0 to ExtIcons.Count-1 do begin
      p := Pos(Ext+'|',ExtIcons[i]);
      if p <> 0 then begin
         Result := integer(ExtIcons.Objects[i]);
         Exit;
      end;
   end;
   Result := GetSpecIconIndex(GetFileSysIconIndex(FileName, 0));
   if Pos(Ext+'.', '.exe.ico.lnk.') = 0 then
      ExtIcons.AddObject(Ext + '|' + StrFileDescription(Ext), TObject(Result) );
end;

{--------------- IS MATCHING MASK ------------------}
function IsMatchingMask(Name, Mask: string; CaseSensitive: Boolean): Boolean;
var pMask, pName: PChar;
begin
   if not CaseSensitive then begin
      Name := AnsiLowerCase(Name);
      Mask := AnsiLowerCase(Mask);
   end;
   if Pos('.',Name) = 0 then Name := Name + '.';  
   pName := PChar(Name);
   pMask := PChar(Mask);
   Result := False;
   while (pMask^ <> #0) and (pName^ <> #0) do
      case PMask^ of
        '?': begin Inc(pMask); Inc(pName); end;
        '*': begin
             Inc(pMask);
             if not (pMask^ in ['*','?']) then
                while (pName^ <> #0) and (pName^ <> pMask^) do
                    Inc(pName);
             end
        else begin
             if pMask^ <> pName^ then Exit;
             Inc(pMask); Inc(pName);
             end;
      end;
   Result := (pMask^ = pName^) or
             ((pMask^ = '*') and ((pMask+1)^ = #0));
end;

{--------------- IS MATCHING NASK LIST ------------------}
function IsMatchingMaskList(Name, MaskList: string; CaseSensitive: Boolean): Boolean;
var p: integer;
    Mask1: string;
begin
   Result := True;
   while Length(MaskList)>0 do begin
      p := Pos(';',MaskList);
      if p > 0 then begin
         Mask1 := Copy(MaskList,1,p-1);
         MaskList := Copy(MaskList,p+1,999);
      end
      else begin
         Mask1 := MaskList;
         MaskList := '';
      end;
      if IsMatchingMask(Name, Mask1, CaseSensitive) then Exit;
   end;
   Result := False;
end;

{--------------- IS MATCHING ATTRS ------------------}
function IsMatchingAttrs(Attrs: integer; FileType: TFileType): Boolean;
begin
  Result := False;
  if not (ftReadOnly in FileType) and (faReadOnly and Attrs <> 0) then Exit;
  if not (ftHidden in FileType) and (faHidden and Attrs <> 0) then Exit;
  if not (ftSystem in FileType) and (faSysFile and Attrs <> 0) then Exit;
  if not (ftVolumeID in FileType) and (faVolumeID and Attrs <> 0) then Exit;
  if not (ftArchive in FileType) and (faArchive and Attrs <> 0) then Exit;
  if not (ftDirectory in FileType) and (faDirectory and Attrs <> 0) then Exit;
  if not (ftNormal in FileType) and (Attrs <> 0) then Exit;
  Result := True;
end;

{------------------ HAS SUBDIRS ------------------}
function HasSubdirs(DirName: string): Boolean;
var FindHandle: THandle;
    FindData: TWin32FindData;
begin
                // найти первую папку
    Result := False;
    FindHandle := FindFirstFile(PChar(DirName+'*.*'), FindData);
    while FindHandle <> INVALID_HANDLE_VALUE do begin
       if (FindData.cFileName[0] <> '.') and
         (FILE_ATTRIBUTE_DIRECTORY and FindData.dwFileAttributes <> 0) then begin
         Result := True;
         Windows.FindClose(FindHandle);
         break;
       end;
       if not FindNextFile(FindHandle, FindData) then begin
         Windows.FindClose(FindHandle);
         break;
       end;
    end;
end;

{--------------- SET DIR LABEL CAPTION ------------------}
procedure SetDirLabelCaption(DirLabel: TLabel; Directory: string;
                             MaxWidth: integer);
var DirWidth: Integer;
begin
  if DirLabel <> nil then begin
    DirWidth := MaxWidth;
    if not DirLabel.AutoSize then DirWidth := DirLabel.Width;
    DirLabel.Caption := MinimizeName(PureDirectory(Directory),
                                     DirLabel.Canvas, DirWidth);
  end;
end;

{--------------- ONE LEVEL UP DIRECTORY ------------------}
function OneLevelUpDirectory(Directory: string): string;
var p, last: integer;
    NewDir: string;
begin
  NewDir := Directory;
  if NewDir[Length(NewDir)] = '\' then
     Delete(NewDir, Length(NewDir), 1);
  last := 0;
  p := 0;
  repeat
    last := last + p;
    p := Pos('\', Copy(NewDir,last+1,999));
  until p = 0;
  if last = 0 then
       Result := '?.0'  // Перейти на Рабочий стол
  else Result := Copy(NewDir, 1, last);
end;

{**********************************************************
           ALL CHAIN VISIBLE
**********************************************************}
function AllChainVisible(Child: TControl): Boolean;
var P: TControl;
begin
  Result := False;
  P := Child;
  while P <> nil do begin
     if not P.Visible then Exit;
     P := P.Parent;
  end;
  Result := True;
end;

{**********************************************************
                GET MY COMPUTER ROOT
**********************************************************}
function GetMyComputerRoot: PFileObject;
begin
    New(Result);
    with Result^ do begin
      DisplayName := MyComputerName;
      ParsingName := MyComputerAlias;
      Description := MyComputerName;
      Size := 0;
      FreeSize := 0;
      DateTime := 0;
      Attrs := 0;
      DriveType := -1;
      ImageIndex := MyComputerIconIndex;
      OverlayIndex := -1;
      FileNotify := nil ;
      NetResource := nil;
    end;
end;

{**********************************************************
                GET NETWORK ROOT
**********************************************************}
function GetNetworkRoot: PFileObject;
var NetResource: PNetResource;
    NetHandle: THandle;
begin
    New(NetResource);
    with NetResource^ do begin
        dwScope := RESOURCE_GLOBALNET;
        dwType := RESOURCETYPE_ANY;
        dwDisplayType := RESOURCEDISPLAYTYPE_SERVER;
        dwUsage := RESOURCEUSAGE_CONNECTABLE;
        lpLocalName := Nil;
        lpRemoteName := Nil;
        lpComment := StrNew('');
        lpProvider := StrNew('');
    end;

                // Если не существует сеть, освободить ресурсы
    if WNetOpenEnum(RESOURCE_GLOBALNET, RESOURCETYPE_DISK,
                 RESOURCEUSAGE_CONNECTABLE or RESOURCEUSAGE_CONTAINER,
                 NetResource, NetHandle) <> NO_ERROR then begin
        StrDispose(NetResource^.lpComment);
        StrDispose(NetResource^.lpProvider);
        Dispose(NetResource);
        Result := nil;
        Exit;
    end;
    WNetCloseEnum(NetHandle);

    New(Result);
    with Result^ do begin
      DisplayName := NethoodName;
      ParsingName := NethoodAlias;
      Description := NethoodName;
      Size := 0;
      FreeSize := 0;
      DateTime := 0;
      Attrs := 0;
      DriveType := -1;
      ImageIndex := NethoodIconIndex;
      OverlayIndex := -1;
      FileNotify := nil ;
    end;
    Result^.NetResource := NetResource;
end;

{**********************************************************
                CREATE
**********************************************************}
constructor TkpFileListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 145;
{  IntegralHeight := True;}
  FFileType := [ftArchive,ftDirectory,ftNormal];
  FMask := DefaultMask;
  MultiSelect := False;
  Style := lbOwnerDrawFixed;

  FNeedRefresh  := False;
  FIsRefreshing := False;
  FNotification := False;
  Directory := 'C:\';
  FAltColor := clBlue;

  FPrevIconIndex := - 1;
  FIconBitmap := TBitmap.Create;
  FTempBitmap := TBitmap.Create;
  FIconWidth  := - 1;
  FIconHeight := - 1;
  FTextOffset := - 1;

  FFileList := TFileList.Create;

end;

{**********************************************************
                DESTROY
**********************************************************}
destructor TkpFileListBox.Destroy;
begin
    if FFileNotify <> nil then FFileNotify.Free;
    FIconBitmap.Free;
    FTempBitmap.Free;
    FFileList.Free;
    inherited;
end;

{**********************************************************
                DBL CLICK
**********************************************************}
procedure TkpFileListBox.DblClick;
var Obj: PFileObject;
//----------------------------------------------------------
    function FolderSelected: Boolean;
    begin
        Result := False;
        if ItemIndex < 0 then Exit;
        Result := (Obj^.ImageIndex = FolderIconIndex)
                  or (Pos(SpecDirAlias, FDirectory) > 0);
    end;
//----------------------------------------------------------
    procedure SelectTreeChild (Tree: TkpFolderTreeView; ChildName: string);
    var ParentNode, Child: TTreeNode;
    begin
       ParentNode := Tree.Selected;
       if ParentNode <> nil then begin
         if not ParentNode.Expanded then ParentNode.Expand(False);
         Child := ParentNode.GetFirstChild;
         while Child <> nil do begin
           if Child.Text = ChildName then begin
             Tree.Selected := Child;
             break;
           end;
           Child := ParentNode.GetNextChild(Child);
         end;
       end;
    end;
//----------------------------------------------------------
    procedure GoInside(DirName: string);
    var NewDir: string;
    begin
      if Pos(':',DirName) = 0 then
         NewDir := FDirectory + DirName + '\';
      SetDirectory(NewDir);
    end;
//----------------------------------------------------------
begin
   Obj := PFileObject(Items.Objects[ItemIndex]);
   if FolderSelected then begin
       if Assigned(FolderTree) then
          SelectTreeChild(FolderTree, LastDirectory(Obj^.DisplayName))
       else
       if Assigned(FolderCombo) then begin
          SelectTreeChild(FolderCombo.FTree, LastDirectory(Obj^.DisplayName));
          FolderCombo.EndBrowse;
       end
       else GoInside(Obj^.ParsingName);
   end
   else
   if (ItemIndex >= 0) and Assigned(FOnFileSelect) then
       OnFileSelect(Self, Obj^.ParsingName);

   inherited;
end;

{**********************************************************
         SET ALT COLOR
**********************************************************}
procedure TkpFileListBox.SetAltColor(const Value: TColor);
begin
  FAltColor := Value;
  if Visible then Invalidate;
end;

{**********************************************************
                SET DIR LABEL
**********************************************************}
procedure TkpFileListBox.SetDirLabel (Value: TLabel);
begin
  FDirLabel := Value;
  if Value <> nil then begin
     Value.FreeNotification(Self);
     SetDirLabelCaption(Value, FDirectory, Width);
  end;
end;

{**********************************************************
                NOTIFICATION
**********************************************************}
procedure TkpFileListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = FFolderTree  then FFolderTree := nil;
    if AComponent = FFolderCombo then FFolderCombo := nil;
    if AComponent = FDirLabel    then FDirLabel := nil;
  end;
end;

{**********************************************************
                IS REFRESHING
**********************************************************}
function TkpFileListBox.IsRefreshing: Boolean;
begin Result := FIsRefreshing; end;

{**********************************************************
                GET FILES SELECTED
**********************************************************}
procedure TkpFileListBox.GetFilesSelected(List: TStrings);
var i: integer;
//----------------------------------------------------------
    procedure AddName(Index: integer);
    var Obj: PFileObject;
    begin
       Obj := PFileObject(Items.Objects[Index]);
       if Obj^.ImageIndex = FolderIconIndex then
            List.AddObject(Obj^.ParsingName, TObject(9) )
       else List.Add(Obj^.ParsingName);
    end;
//----------------------------------------------------------
begin
     List.Clear;
     if MultiSelect then begin
        for i:=0 to Items.Count-1 do
            if Selected[i] then AddName(i);
     end
     else if ItemIndex >= 0 then AddName(ItemIndex);
end;

{**********************************************************
                GET FILE NAME
**********************************************************}
function TkpFileListBox.GetFileName;
var i: integer;
//----------------------------------------------------------
    procedure AddName(Index: integer);
    var Obj: PFileObject;
    begin
       if Result <> '' then Result := Result + '|';
       Obj := PFileObject(Items.Objects[Index]);
       if Obj^.ImageIndex = FolderIconIndex then Result := Result + #9;
       Result := Result + Obj^.DisplayName;
    end;
//----------------------------------------------------------
begin
     Result := '';
     if MultiSelect then begin
        for i:=0 to Items.Count-1 do
            if Selected[i] then AddName(i);
     end
     else if ItemIndex >= 0 then AddName(ItemIndex);
end;

{**********************************************************
        REFRESH
**********************************************************}
procedure TkpFileListBox.Refresh;
begin
  ChangeFileSystem(Self, fnoFile);
end;

{**********************************************************
        KEY DOWN
**********************************************************}
procedure TkpFileListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_BACK then OneLevelUp;
  if Key = VK_RETURN then DblClick;
  if Key = VK_F5 then Refresh;
end;

{**********************************************************
                CHANGE FILE SYSTEM
**********************************************************}
procedure TkpFileListBox.ChangeFileSystem(Sender :TObject;
                                          Action :TNotificationOption);
var OldDir: string;
begin
    if Pos(SpecDirAlias, FDirectory) = 0 then begin
       if (not ShowFolders) and (Action = fnoFolder) then Exit;
       if Notify and (Sender <> FFileNotify) and (Sender <> Self) then Exit;
    end;
    if Parent <> nil then begin
       if not FWaitFirstNotify then
          if not FIsRefreshing then begin
               OldDir := FDirectory;
               FDirectory := '';
               SetDirectory(OldDir);
          end
          else FNeedRefresh := True;
       FWaitFirstNotify := False;
    end;
end;


{**********************************************************
                SET MASK
**********************************************************}
procedure TkpFileListBox.SetMask(Value: string);
begin
    if AnsiCompareText(Fmask,Value)<>0 then begin
        FMask := Value;
        FreshList(FDirectory);
    end;
end;

{**********************************************************
                SET NOTIFICATION
**********************************************************}
procedure TkpFileListBox.SetNotification(Value: Boolean);
var NotificationOptions: TNotificationOptions;
begin
    if Value <> FNotification then begin
       FNotification := Value;

       if FNotification then begin
          if ftDirectory in FFileType then
               NotificationOptions := [fnoFile,fnoFolder]
          else NotificationOptions := [fnoFile];
          if not Assigned(FFileNotify) then begin
             FFileNotify := TFileNotify.Create(Self,NotificationOptions);
             with FFileNotify do begin
               AsyncNotify := False;
               OnNotify := ChangeFileSystem;
               Prepare;
             end;
          end;
          if not (csDesigning in ComponentState) then begin
             FFileNotify.Folder := FDirectory;
             FFileNotify.StartMonitoring;
          end;
       end
       else begin
          FFileNotify.StopMonitoring;
       end;
    end;
end;

{**********************************************************
                SET NET BROWSE
**********************************************************}
procedure TkpFileListBox.SetNetBrowse(Value: Boolean);
var OldDir: string;
begin
    if FNetBrowse = Value then Exit;
    FNetBrowse := Value;
    if Pos(SpecDirAlias, FDirectory) > 0 then begin
       OldDir := FDirectory;
       FDirectory := '';
       SetDirectory(OldDir);
    end;
end;

{**********************************************************
                SET FILE TYPE
**********************************************************}
procedure  TkpFileListBox.SetFileType(NewFileType: TFileType);
begin
   if NewFileType = FFileType then Exit;
   if  (FFileNotify<>nil) and
       ((ftDirectory in NewFileType) <>
        (fnoFolder in FFileNotify.Options)) then
       if (ftDirectory in NewFileType) then
            FFileNotify.Options := [fnoFile,fnoFolder]
       else FFileNotify.Options := [fnoFile];

   FFileType := NewFileType;

   AssignFileList;  // не перечитывать заново!!!
end;

{**********************************************************
                GET/SET SHOW FOLDERS
**********************************************************}
procedure TkpFileListBox.SetShowFolders(Value: Boolean);
begin
   if Value then
         FileType := FileType + [ftDirectory]
   else  FileType := FileType - [ftDirectory];
end;

function TkpFileListBox.GetShowFolders: Boolean;
begin
   Result := ftDirectory in FileType;
end;

procedure TkpFileListBox.SetFoldersOnly(Value: Boolean);
begin
   if FFoldersOnly = Value then Exit;
   FFoldersOnly := Value;
   AssignFileList;
end;

{**********************************************************
                CAN BROWSE NET
**********************************************************}
function TkpFileListBox.CanBrowseNet: Boolean;
begin
  Result := True;
  if Assigned(FFolderTree) and FFolderTree.NetBrowse then Exit;
  if Assigned(FFolderCombo) and FFolderCombo.NetBrowse then Exit;
  Result := FNetBrowse;
end;

{**********************************************************
                ONE LEVEL UP
**********************************************************}
procedure TkpFileListBox.OneLevelUp;
begin
  if Assigned(FFolderTree) then begin
     FFolderTree.OneLevelUp;
     Exit;
  end;
  if Assigned(FFolderCombo) then begin
     FFolderCombo.OneLevelUp;
     Exit;
  end;
  if Pos(SpecDirAlias, FDirectory) = 0 then
     SetDirectory(OneLevelUpDirectory(FDirectory));
end;

{**********************************************************
                GET DIRECTORY
**********************************************************}
function TkpFileListBox.GetDirectory: string;
begin
  Result := PureDirectory(FDirectory);
end;

{**********************************************************
                BEGIN/END UPDATE
**********************************************************}
procedure TkpFileListBox.BeginUpdate;
begin
     if FUpdateCount = 0 then LockWindowUpdate(Handle);
     Inc(FUpdateCount);
end;
procedure TkpFileListBox.EndUpdate;
begin
     Dec(FUpdateCount);
     if FUpdateCount = 0 then LockWindowUpdate(0);
end;

{**********************************************************
                SET DIRECTORY
**********************************************************}
procedure TkpFileListBox.SetDirectory(NewDirectory: string);
var i, p: integer;
    Obj: PFileObject;
    NetResource: PNetResource;
    OldCapture: HWND;
    ReadyList: Boolean;
begin
     //  Сетевые ресурсы преваратить в пути
  if (Pos(ShareAlias, NewDirectory) = 1) then begin
     NewDirectory := Copy(NewDirectory, 4, 999);
     p := Pos(SpecDirAlias, NewDirectory);
     if p > 0 then NewDirectory := Copy(NewDirectory,1,p-1);
     if (NewDirectory <> '') and (NewDirectory[Length(NewDirectory)] <> '\') then
        NewDirectory := NewDirectory + '\';
  end;

  if (AnsiCompareFileName(NewDirectory, FDirectory) = 0) then Exit;
  if not Assigned(Parent) then Exit;

  Screen.Cursor := crHourGlass;
  OldCapture := SetCapture(Handle);
try
     // Рабочий стол
  if Pos(DeskTopAlias, NewDirectory) > 0  then begin
     BeginUpdate;
   try
     Items.Clear;
     FFileList.Clear;
     Obj := GetMyComputerRoot;
     FFileList.Add(Obj);
     Items.AddObject(Obj^.DisplayName, TObject(Obj));
     if CanBrowseNet then begin
        Obj := GetNetworkRoot;
        FFileList.Add(Obj);
        Items.AddObject(Obj^.DisplayName, TObject(Obj));
     end;
   finally
     EndUpdate;
     FDirectory := NewDirectory;
   end;
     Exit;
  end;

     // Мой компьютер
  if Pos(MyComputerAlias, NewDirectory) > 0  then begin
     BeginUpdate;
    try
     Items.Clear;
     ReadyList := False;
     if not FFreshDrives then begin
        if Assigned(FFolderTree) then
           ReadyList := FFolderTree.FillFileList(NewDirectory, FFileList);
        if not ReadyList and Assigned(FFolderCombo) then
           ReadyList := FFolderCOmbo.FTree.FillFileList(NewDirectory, FFileList);
     end;
     if not ReadyList then
        FFileList.FillByDrives(FFreshDrives);
     for i:=0 to FFileList.Count-1 do begin
       Obj := PFileObject(FFileList[i]);  // уничтожаются при очистке FFileList!!!
       Items.AddObject(Obj^.DisplayName, TObject(Obj));
     end;
    finally
     EndUpdate;
     FDirectory := NewDirectory;
    end;
     Exit;
  end;

     //  Сетевые контейнеры
  if (Pos(SpecDirAlias, NewDirectory) > 0) then begin
     FDirectory := NewDirectory;
   try
     BeginUpdate;
     Items.Clear;
     if CanBrowseNet then begin
        p := Pos(SpecDirAlias, Copy(NewDirectory,3,999));
        if p > 0 then begin
           ReadyList := False;
           NetResource := PNetResource(StrToInt(Copy(NewDirectory,p+4,999)));
           NewDirectory := Copy(NewDirectory,1,p+1);
           if Assigned(FFolderTree) then
              ReadyList := FFolderTree.FillFileList(NewDirectory, FFileList);
           if not ReadyList and Assigned(FFolderCombo) then
              ReadyList := FFolderCOmbo.FTree.FillFileList(NewDirectory, FFileList);
           if not ReadyList then
              FFileList.FillFromNethood(NetResource);
           for i:=0 to FFileList.Count-1 do begin
             Obj := PFileObject(FFileList[i]);  // уничтожаются при очистке FFileList!!!
             Items.AddObject(LastDirectory(Obj^.DisplayName), TObject(Obj));
           end;
        end;
     end;
   finally
     EndUpdate;
   end;
     Exit;
  end;

  FDirectory := NewDirectory;
  if (FDirectory <> '') and (FDirectory[Length(FDirectory)] <> '\') then
     FDirectory := FDirectory + '\';

  FreshList(FDirectory);

  if FFileNotify<>nil then begin
     if FFileNotify.Folder<>'' then FWaitFirstNotify := True;
     FFileNotify.Folder := FDirectory;
  end;

finally
   if Pos(SpecDirAlias, NewDirectory) = 0 then begin
      if Assigned(FFolderTree) then FFolderTree.SetDirectory(FDirectory);
      if Assigned(FFolderCombo) then FFolderCombo.SetDirectory(FDirectory);
   end;
   SetDirLabelCaption(FDirLabel, FDirectory, Width);
   Screen.Cursor := crDefault;
   SetCapture(OldCapture);
end;

end;

{**********************************************************
                IS SPECIAL FOLDER
**********************************************************}
function TkpFileListBox.IsSpecialFolder: Boolean;
begin
   Result := (Pos(SpecDirAlias,FDirectory) > 0);
end;

{**********************************************************
                SET FOLDER TREE
**********************************************************}
procedure TkpFileListBox.SetFolderTree(const Value: TkpFolderTreeView);
begin
   if FFolderTree <> Value then begin
      if FFolderCombo <> nil then
         FFolderCombo.FTree.Notification(Self, opRemove);
      FFolderCombo := nil;
      if FFolderTree <> nil then
         FFolderTree.Notification(Self, opRemove);
      FFolderTree := Value;
      if FFolderTree <> nil then begin
         FreeNotification(FFolderTree);
         FFolderTree.AddViewChild(Self);
         SetDirectory(FFolderTree.FDirectory);
      end;
   end;
end;

{**********************************************************
                SET FOLDER COMBO
**********************************************************}
procedure TkpFileListBox.SetFolderCombo(const Value: TkpFolderCombo);
begin
   if FFolderCombo <> Value then begin
      if FFolderCombo <> nil then
         FFolderCombo.FTree.Notification(Self, opRemove);
      FFolderCombo := Value;
      if FFolderTree <> nil then
         FFolderTree.Notification(Self, opRemove);
      FFolderTree := nil;
      if FFolderCombo <> nil then begin
         FreeNotification(FFolderCombo);
         FFolderCombo.FTree.AddViewChild(Self);
         SetDirectory(FFolderCombo.FTree.FDirectory);
      end;
   end;
end;


{**********************************************************
                DRAW ITEM
**********************************************************}
procedure TkpFileListBox.DrawItem(Index: Integer; Rect: TRect;
                                State: TOwnerDrawState);
var IconIndex: integer;
    rct:       TRect;
    stdColor:  TColor;
    Obj:       PFileObject;
begin
     with Canvas do begin
        FillRect(Rect);
        Obj := PFileObject(Items.Objects[Index]);
        IconIndex := Obj^.ImageIndex;
        if IconIndex <> FPrevIconIndex then begin
            kpSmallImageList.GetBitmap(IconIndex,FTempBitmap);
            if FIconWidth < 0 then begin
               FIconWidth  := MulDiv(FTempBitmap.Width,ItemHeight,16);
               FIconHeight := MulDiv(FTempBitmap.Height,ItemHeight,16);
               FTextOffset := FIconWidth + 8;
               FIconBitmap.Width := FIconWidth;
               FIconBitmap.Height := FIconHeight;
            end;
            rct := Classes.Rect(0,0,FIconWidth,FIconHeight);
            FIconBitmap.Canvas.StretchDraw(rct,FTempBitmap);
        end;

        Draw(Rect.Left+2,Rect.Top,FIconBitmap);

        stdColor := Font.Color;
        if (Obj^.Attrs and faCompressed <> 0) and
           (not (odSelected in State)) then
           Font.Color := FAltColor;
        TextOut(Rect.Left+FTextOffset, Rect.Top, Items[Index]);
        Font.Color := stdColor;

        FPrevIconIndex := IconIndex;
     end;
end;

{**********************************************************
                ASSIGN FILE LIST
**********************************************************}
procedure TkpFileListBox.AssignFileList;
var i: integer;
    Obj: PFileObject;
    //----------------------------------------------------
    function FileNeeded: Boolean;
    begin
       Result := (Pos(SpecDirAlias, FDirectory) > 0) or
          (IsMatchingAttrs(Obj^.Attrs, FFileType) and
            (IsMatchingMaskList(Obj^.ParsingName, FMask, False) or
              (Obj^.Attrs and faDirectory <> 0)));
       if (Obj^.Attrs and faDirectory = 0) and FFoldersOnly then
          Result := False;
    end;
    //----------------------------------------------------
begin
    BeginUpdate;
  try
    Items.Clear;
    for i:=0 to FFileList.Count-1 do begin
       Obj := PFileObject(FFileList[i]);
       if FileNeeded then begin
          Items.AddObject(Obj^.DisplayName, TObject(Obj));
          if Obj^.ImageIndex < 0 then
             Obj^.ImageIndex := GetFileIconIndex(FDirectory+Obj^.ParsingName);
       end;
    end;
  finally
    EndUpdate;
  end;
end;

{**********************************************************
                FRESH FILE LIST
**********************************************************}
procedure TkpFileListBox.FreshList(NewDir: string);
var OldCapture: HWND;
begin
    if Pos(SpecDirAlias, FDirectory) <> 0 then Exit;
    Screen.Cursor := crHourGlass;
    OldCapture := SetCapture(Handle);
    FIsRefreshing := True;

  try
   repeat
     FNeedRefresh  := False;
     FFileList.FillByFiles(NewDir, [fotDirectory,fotFiles]);
   until not FNeedRefresh;

  finally
    AssignFileList;
    SetCapture(OldCapture);
    Screen.Cursor := crDefault;
    FIsRefreshing := False;
  end;

  if Assigned(FOnFileListChange) then OnFileListChange(self);
end;

{**********************************************************
                CREATE FOLDER
**********************************************************}
procedure TkpFileListBox.CreateFolder;
var NewFolder, Path: string;
begin
     if not ShowFolders or ReadOnly then Exit;
     if Pos(SpecDirAlias,FDirectory) > 0 then Exit;

     if Assigned(FFolderTree) and AllChainVisible(FFolderTree) then begin
        FFolderTree.CreateFolder(False);
        Exit;
     end;

     NewFolder := '';
     if not InputQuery(SFolderQueryCaption, SFolderQueryMsg, NewFolder) then  Exit;

     Path := FDirectory + NewFolder;
     if not CreateDir(Path) then begin
        MessageDlg( SCreateFailure + ' ' + Path, mtError, [mbOK], 0);
        Exit;
     end;

     FFileList.AddFile(Path);
     AssignFileList;
     if Assigned(FOnFolderCreated) then FOnFolderCreated(Self, NewFolder);

end;

{**********************************************************
          GET FILE SIZE
**********************************************************}
function  TkpFileListBox.GetFileSize(Index: integer): extended;
var Obj: PFileObject;
begin
  Result := 0;
  Obj := PFileObject(Items.Objects[Index]);
  if Assigned(Obj) then Result := Obj^.Size;
end;

{**********************************************************
          GET SEL SIZE
**********************************************************}
function  TkpFileListBox.GetSelSize: extended;
var i, Count: integer;
begin
  Result := 0;
  Count := 0;
  if not MultiSelect then begin
     if ItemIndex >= 0 then
        Result := GetFileSize(ItemIndex);
     Exit;
  end;
  for i:=0 to Items.Count-1 do
    if Selected[i] then begin
       Result := Result + GetFileSize(i);
       if (Pos(SpecDirAlias, FDirectory) > 0) and (Count > 0) then begin
          Result := 0;
          Exit;
       end;
       Inc(Count);
    end;
end;

{**********************************************************
          GET ITEMS COUNT
**********************************************************}
function TkpFileListBox.GetItemsCount: integer;
begin  Result := Items.Count; end;

{**********************************************************
          GET SEL COUNT
**********************************************************}
function TkpFileListBox.GetSelCount: integer;
begin
  Result := 0;
  if MultiSelect then Result := inherited SelCount
  else if ItemIndex >= 0 then Result := 1;
end;

{**********************************************************
          GET TOTAL SIZE
**********************************************************}
function  TkpFileListBox.GetTotalSize: extended;
var i: integer;
begin
  Result := 0;
  if Pos(SpecDirAlias, FDirectory) > 0 then Exit;
  for i:=0 to Items.Count-1 do
    Result := Result + GetFileSize(i);
end;

{**********************************************************
                CREATE
**********************************************************}
constructor TkpFileListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 145;
{  IntegralHeight := True;}
  FFileType := [ftArchive,ftDirectory,ftNormal];
  FMask := DefaultMask;
  MultiSelect := False;

  FNeedRefresh  := False;
  FIsRefreshing := False;
  FNotification := False;
  FNameSpace := nsNone;   

  LargeImages := TImageList(kpLargeImageList);
  SmallImages := TImageList(kpSmallImageList);

  ShowColumnHeaders := True;
  MultiSelect := True;

  OnCompare := CompareItems;

  if not (csDesigning in ComponentState) then begin
     FSaveWndProc := WindowProc;
     WindowProc := OwnWndProc;
  end;

//  inherited OnEdited := NewOnEdited;
  FIsEditingNew := False;
  IconOptions.AutoArrange := True;
  FAltColor := clBlue;

  FFileList := TFileList.Create;
  FFreshDrives := False;

end;

{**********************************************************
        SET NAME SPACE
**********************************************************}
procedure TkpFileListView.SetNameSpace(Value: TNameSpace);
begin
  if Columns.Count = 0 then Exit;   
  if FNameSpace = Value then Exit;

  FNameSpace := Value;
  BeginUpdate;
try
  if FNameSpace = nsFiles then begin
      with Columns[0] do begin
        Alignment := taLeftJustify;
        Caption := 'Имя файла';
        Width := 130;
      end;
      with Columns[1] do begin
        Alignment := taRightJustify;
        Caption := 'Размер';
        Width := 80;
      end;
      with Columns[2] do begin
        Alignment := taLeftJustify;
        Caption := 'Тип';
        Width := 150;
      end;
      with Columns[3] do begin
        Alignment := taLeftJustify;
        Caption := 'Изменен';
        Width := 120;
      end;
      with Columns[4] do begin
        Alignment := taRightJustify;
        Caption := 'Атрибуты';
        Width := 70;
      end;
  end
  else

  if FNameSpace = nsDrives then begin
      with Columns[0] do begin
        Alignment := taLeftJustify;
        Caption := 'Имя диска';
        Width := 100;
      end;
      with Columns[1] do begin
        Alignment := taLeftJustify;
        Caption := 'Тип';
        Width := 150;
      end;
      with Columns[2] do begin
        Alignment := taRightJustify;
        Caption := 'Размер';
        Width := 80;
      end;
      with Columns[3] do begin
        Alignment := taRightJustify;
        Caption := 'Свободно';
        Width := 80;
      end;
      with Columns[4] do begin
        Alignment := taRightJustify;
        Caption := 'Атрибуты';
        Width := 70;
      end;
  end;

  if FNameSpace = nsMisc then begin
      with Columns[0] do begin
        Alignment := taLeftJustify;
        Caption := 'Имя';
        Width := 150;
      end;
      with Columns[1] do begin
        Alignment := taLeftJustify;
        Caption := 'Описание';
        Width := 150;
      end;
      with Columns[2] do begin
        Alignment := taRightJustify;
        Caption := '';
        Width := 80;
      end;
      with Columns[3] do begin
        Alignment := taRightJustify;
        Caption := '';
        Width := 80;
      end;
      with Columns[4] do begin
        Alignment := taRightJustify;
        Caption := '';
        Width := 70;
      end;
  end;
finally
  EndUpdate;
end;
end;

{**********************************************************
                OWN WND PROC
**********************************************************}
procedure TkpFileListView.OwnWndProc(var Message: TMessage);
var Item: TLVItem;
    NotifyMsg: TWMNotify;
begin
  if (Message.Msg = WM_KEYDOWN) and
     (Message.wParam = VK_RETURN) then ShowMessage('aaa');
  if (Message.Msg = WM_LBUTTONDOWN) and
     not AllChainVisible(Self) then Exit;

  if FIsEditingNew and (Message.Msg = CN_NOTIFY) then begin
     NotifyMsg := TWMNotify(Message);
     if (NotifyMsg.NMHdr^.code = LVN_ENDLABELEDIT) and
        (PLVDispInfo(Pointer(NotifyMsg.NMHdr))^.item.pszText = nil)
     then begin
       Item.state := LVIF_PARAM;
       Item.lParam := Integer(ItemFocused);
       Item.pszText := nil;
       Edit(Item);
       Exit;
     end;
  end;

  FSaveWndProc(Message);
end;

{**********************************************************
                CREATE WND
**********************************************************}
procedure TkpFileListView.CreateWnd;
begin
  inherited;
  Columns.Add;
  Columns.Add;
  Columns.Add;
  Columns.Add;
  Columns.Add;
  SetNameSpace(nsFiles);
  if FDirectory = '' then SetDirectory('C:\');
end;

{**********************************************************
                DESTROY
**********************************************************}
destructor TkpFileListView.Destroy;
begin
    if FFileNotify <> nil then FFileNotify.Free;
    FFileList.Free;   
    inherited;
end;

{**********************************************************
      GET CUSTOM DRAW COLORS
**********************************************************}
procedure TkpFileListView.GetCustomDrawColors(ItemNo, SubItemNo: DWORD;
                                    var ColText, ColBk: TColor);
var Obj: PFileObject;
begin
   Obj := PFileObject(Items[ItemNo].Data);
   if Assigned(Obj) and (Obj^.Attrs and faCompressed <> 0) then
      ColText := FAltColor;
end;

{**********************************************************
         CN_NOTIFY
**********************************************************}
procedure TkpFileListView.CNNotify(var msg: TWMNotify);
var TextCol: TColor;
    BackCol: TColor;
    SubItem: integer;
begin
  if msg.NMHdr^.code = NM_CUSTOMDRAW then
    with PNMLVCustomDraw(msg.NMHdr)^ do begin
      if (nmcd.dwDrawStage = CDDS_PREPAINT) then
          msg.Result := CDRF_DODEFAULT or CDRF_NOTIFYITEMDRAW
      else
      if (nmcd.dwDrawStage and CDDS_ITEMPREPAINT) <> 0 then begin
          TextCol := TColor(clrText);
          BackCol := TColor(clrTextBk);
          if (nmcd.dwDrawStage and CDDS_SUBITEM) <> 0 then
               SubItem:=iSubItem
          else SubItem:=0;
          GetCustomDrawColors(nmcd.dwItemSpec, SubItem, TextCol, BackCol);
          clrText  := ColorToRGB(TextCol);
          clrTextBk := ColorToRGB(BackCol);
          msg.Result:=CDRF_DODEFAULT;
          msg.Result:=msg.Result or CDRF_NOTIFYSUBITEMDRAW;
      end;
      Exit;
    end;

  inherited;
end;

{**********************************************************
                DBL CLICK
**********************************************************}
procedure TkpFileListView.DblClick;
var Obj: PFileObject;
//----------------------------------------------------------
    function FolderSelected: Boolean;
    begin
        Result := False;
        if Selected = nil then Exit;
        Result := (Selected.ImageIndex = FolderIconIndex)
                  or (Pos(SpecDirAlias, FDirectory) > 0);
    end;
//----------------------------------------------------------
    procedure SelectTreeChild (Tree: TkpFolderTreeView; ChildName: string);
    var ParentNode, Child: TTreeNode;
    begin
       ParentNode := Tree.Selected;
       if ParentNode <> nil then begin
         if not ParentNode.Expanded then ParentNode.Expand(False);
         Child := ParentNode.GetFirstChild;
         while Child <> nil do begin
           if Child.Text = ChildName then begin
             Tree.Selected := Child;
             break;
           end;
           Child := ParentNode.GetNextChild(Child);
         end;
       end;
    end;
//----------------------------------------------------------
    procedure GoInside(DirName: string);
    var NewDir: string;
    begin
      if Pos(':',DirName) = 0 then
         NewDir := FDirectory + DirName + '\';
      SetDirectory(NewDir);
    end;
//----------------------------------------------------------
begin
   if (Selected = nil) or  (Selected.Data = nil) then Exit;
   Obj := PFileObject(Selected.Data);
   if FolderSelected then begin
       if Assigned(FolderTree) then
          SelectTreeChild(FolderTree, LastDirectory(Obj^.DisplayName))
       else
       if Assigned(FolderCombo) then begin
          SelectTreeChild(FolderCombo.FTree, LastDirectory(Obj^.DisplayName));
          FolderCombo.EndBrowse;
       end
       else GoInside(Obj^.ParsingName);
   end
   else
   if Assigned(FOnFileSelect) then
       OnFileSelect(Self, Obj^.ParsingName);

   inherited;
end;

{**********************************************************
         SET ALT COLOR
**********************************************************}
procedure TkpFileListView.SetAltColor(const Value: TColor);
begin
  FAltColor := Value;
  if Visible then Invalidate;
end;

{**********************************************************
                SET DIR LABEL
**********************************************************}
procedure TkpFileListView.SetDirLabel (Value: TLabel);
begin
  FDirLabel := Value;
  if Value <> nil then begin
     Value.FreeNotification(Self);
     SetDirLabelCaption(Value, FDirectory, Width);
  end;
end;

{**********************************************************
                NOTIFICATION
**********************************************************}
procedure TkpFileListView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = FFolderTree then  FFolderTree := nil;
    if AComponent = FFolderCombo then FFolderCombo := nil;
    if AComponent = FDirLabel then    FDirLabel := nil;
  end;
end;

{**********************************************************
                IS REFRESHING
**********************************************************}
function TkpFileListView.IsRefreshing: Boolean;
begin Result := FIsRefreshing; end;

{**********************************************************
                COMPARE ITEMS
**********************************************************}
procedure TkpFileListView.CompareItems(Sender: TObject; Item1, Item2: TListItem;
                       Data: Integer; var Compare: Integer);
var i: integer;
    Obj1, Obj2: PFileObject;
begin
  Compare := 0;
  Obj1 := PFileObject(Item1.Data);
  Obj2 := PFileObject(Item2.Data);
  if (Obj1 = nil) or (Obj2 = nil) then Exit;

  i := Data - 1;
  if (Item1.ImageIndex = FolderIconIndex) and
     (Item2.ImageIndex <> FolderIconIndex) then Compare := - 1
  else
  if (Item1.ImageIndex <> FolderIconIndex) and
     (Item2.ImageIndex = FolderIconIndex) then Compare := 1
  else
  begin
    if FNameSpace = nsFiles then
      case i of
        0:    Compare := Sign(Obj1^.Size - Obj2^.Size);
        2:    Compare := Sign(Obj1^.DateTime - Obj2^.DateTime);
        1, 3: Compare := AnsiCompareText(Item1.SubItems[i], Item2.SubItems[i]);
        else  Compare := AnsiCompareText(Item1.Caption, Item2.Caption);
      end
    else
    if FNameSpace = nsDrives then
      case i of
        1:     Compare := Sign(Obj1^.Size - Obj2^.Size);
        2:     Compare := Sign(Obj1^.FreeSize - Obj2^.FreeSize);
        0, 3:  Compare := AnsiCompareText(Item1.SubItems[i], Item2.SubItems[i]);
        else   Compare := AnsiCompareText(Item1.Caption, Item2.Caption);
      end;
    if FSortOrder[Data] < 0 then Compare := - Compare;
  end;
end;

{**********************************************************
                COL CLICK
**********************************************************}
procedure TkpFileListView.ColClick(Column: TListColumn);
var i: integer;
begin
  for i:=0 to 4 do
      if i <> Column.Index then FSortOrder[i] := 0
      else
         if FSortOrder[i] = 0 then FSortOrder[i] := 1
         else FSortOrder[i] := - FSortOrder[i];
  CustomSort(nil, Column.Index);
  inherited;
end;

{**********************************************************
                GET FILES SELECTED
**********************************************************}
procedure TkpFileListView.GetFilesSelected(List: TStrings);
var i: integer;
//----------------------------------------------------------
    procedure AddName(Index: integer);
    var Obj: PFileObject;
    begin
       Obj := PFileObject(Items[Index].Data);
       if Obj^.ImageIndex = FolderIconIndex then
            List.AddObject(Obj^.ParsingName, TObject(9) )
       else List.Add(Obj^.ParsingName);
    end;
//----------------------------------------------------------
begin
     List.Clear;
     for i:=0 to Items.Count-1 do
            if Items[i].Selected then AddName(i);
end;

{**********************************************************
                GET FILE NAME
**********************************************************}
function TkpFileListView.GetFileName;
var i: integer;
//----------------------------------------------------------
    procedure AddName(Index: integer);
    var Obj: PFileObject;
    begin
       if Result <> '' then Result := Result + '|';
       Obj := PFileObject(Items[Index].Data);
       if Obj^.ImageIndex = FolderIconIndex then Result := Result + #9;
       Result := Result + Obj^.DisplayName;
    end;
//----------------------------------------------------------
begin
     Result := '';
     if MultiSelect then
        for i:=0 to Items.Count-1 do
            if Items[i].Selected then AddName(i);
end;

{**********************************************************
        REFRESH
**********************************************************}
procedure TkpFileListView.Refresh;
begin
   ChangeFileSystem(Self, fnoFile);
end;

{**********************************************************
        KEY DOWN
**********************************************************}
procedure TkpFileListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
//  inherited;
  if (not FIsEditingNew) and (not IsEditing) then begin
     if (Key = VK_BACK)   then OneLevelUp;
     if (Key = VK_RETURN) then DblClick;
     if Key = VK_F5 then Refresh;
  end;
end;

{**********************************************************
                CHANGE FILE SYSTEM
**********************************************************}
procedure TkpFileListView.ChangeFileSystem(Sender :TObject;Action :TNotificationOption);
var OldDir: string;
begin
    if Pos(SpecDirAlias, FDirectory) = 0 then begin
       if not ShowFolders and (Action = fnoFolder) then Exit;
       if Notify and (Sender <> FFileNotify) and (Sender <> Self) then Exit;
    end;
    if Parent <> nil then begin
       if not FWaitFirstNotify then
          if not FIsRefreshing then begin
               OldDir := FDirectory;
               FDirectory := '';
               SetDirectory(OldDir);
          end
          else FNeedRefresh := True;
       FWaitFirstNotify := False;
    end;
end;

{**********************************************************
          EDIT
**********************************************************}
type THackListItems = class(TListItems);
procedure TkpFileListView.Edit(const Item: TLVItem);
var S: string;
    EditItem: TListItem;
begin
  with Item do begin
    S := pszText;
    if (state and LVIF_PARAM) <> 0 then
          EditItem := Pointer(lParam)
    else  EditItem := THackListItems(Items).GetItem(iItem);
    if pszText = nil then begin
       Items.Delete(Items.IndexOf(EditItem));
    end
    else begin
       NewOnEdited(Self, EditItem, S); //NB: EditItem can be changed!!!
       if (EditItem <> nil) and (S <> '') then
          EditItem.Caption := S;
    end;
  end;
end;

{**********************************************************
          CAN EDIT
**********************************************************}
function TkpFileListView.CanEdit(Item: TListItem): Boolean;
begin
   Result := inherited CanEdit(Item);
   if Result then
      Result := FIsEditingNew or
                ((not ReadOnly) and (Pos('?', FDirectory) = 0));
end;

{**********************************************************
          NEW ON EDITED
**********************************************************}
procedure TkpFileListView.NewOnEdited(Sender: TObject;
                        var Item: TListItem; var S: string);
var OldPath, Path: string;
    Obj: PFileObject;
    i: integer;
    //-------------------------------------------------------
    function IsFolder (FileName: string): Boolean;
    var Obj: PFileObject;
        Index: integer;
    begin
       Result := False;
       for Index:=1 to Items.Count do begin
          Obj := PFileObject(Items[Index].Data);
          if Obj^.DisplayName = FileName then begin
             Result :=  Obj^.ImageIndex = FolderIconIndex;
             break;
          end;
       end;
    end;
    //-------------------------------------------------------
begin
    if Assigned(FUserOnEdited) then FUserOnEdited(Sender, Item, S);

    if (Item <> nil) and (S <> '') then begin
       Path := FDirectory + S;
       if FIsEditingNew then begin //--- new directory created
          Item.Caption := S;
          if not CreateDir(Path) then begin
               MessageDlg( SCreateFailure + ' ' + Path, mtError, [mbOK], 0);
               Items.Delete(Items.IndexOf(Item));
               S := '';
          end;
          Obj := FFileList.AddFile(Path);
          if Assigned(FFolderCombo) then FFolderCOmbo.Refresh;
          AssignFileList;
          Item := nil;
          for i:=0 to Items.Count do
            if PFileObject(Items[i].Data) = Obj then begin
               Item := Items[i];
               break;
            end;
          if (S <> '') and Assigned(FOnFolderCreated) then
             FOnFolderCreated(Sender, S);
          FIsEditingNew := False;
       end
       else begin  //--- change file name
          if Pos('?', FDirectory) > 0 then begin //--- special virtual folder or disk
               OldPath := Item.Caption;
               Path := S;
               S := '';
          end
          else begin   //--- ordinary file or folder
               OldPath := FDirectory + Item.Caption;
               if not RenameFile(OldPath, Path) then S := '';
          end;
          if Assigned(FOnFileRename) then
             FOnFileRename(Self, OldPath, Path, (S <> ''), IsFolder(Item.Caption));
       end;
    end;
end;

{**********************************************************
                CREATE FOLDER
**********************************************************}
procedure TkpFileListView.CreateFolder;
var NewItem: TListItem;
begin
     if (not ShowFolders) or ReadOnly then Exit;
     if Pos(SpecDirAlias,FDirectory) > 0 then Exit;

     if Assigned(FFolderTree) and AllChainVisible(FFolderTree) then begin
        FFolderTree.CreateFolder(False);
        Exit;
     end;

     FIsEditingNew := True;
     NewItem := Items.Insert(0);
     NewItem.ImageIndex := FolderIconIndex;
     NewItem.SubItems.Add('');
     NewItem.Caption := '-----';
     SetFocus;
     NewItem.EditCaption;
end;

{**********************************************************
                ASSIGN FILE LIST
**********************************************************}
procedure TkpFileListView.AssignFileList;
var i: integer;
    Obj: PFileObject;
    //----------------------------------------------------
    function FileNeeded: Boolean;
    begin
       Result := (Pos(SpecDirAlias, FDirectory) > 0) or
          (IsMatchingAttrs(Obj^.Attrs, FFileType) and
            (IsMatchingMaskList(Obj^.ParsingName, FMask, False) or
             (Obj^.Attrs and faDirectory <> 0)));
       if (Obj^.Attrs and faDirectory = 0) and FFoldersOnly then
          Result := False;
    end;
    //----------------------------------------------------
begin
    SortType := stNone;
    BeginUpdate;
  try
    Items.Clear;
    for i:=0 to FFileList.Count-1 do begin
       Obj := PFileObject(FFileList[i]);
       if FileNeeded then begin
          with Items.Add do begin
             if Obj^.Description = '' then
                if Obj^.ImageIndex = FolderIconIndex then
                     Obj^.Description := StrFileDescription('Directory')
                else Obj^.Description := GetFileDescription(FDirectory+Obj^.ParsingName);
             if Obj^.ImageIndex < 0 then
                Obj^.ImageIndex := GetFileIconIndex(FDirectory+Obj^.ParsingName);
             Caption := Obj^.DisplayName;
             Data := Obj;
             ImageIndex := Obj^.ImageIndex;
             OverlayIndex := Obj^.OverlayIndex;
             SubItems.Add(StrFileSize(Obj^.Size));
             SubItems.Add(Obj^.Description);
             SubItems.Add(FormatDateTime('dd.mm.yyyy hh.nn.ss', Obj^.DateTime));
             SubItems.Add(StrFileAttr(Obj^.Attrs));
          end;
       end;
    end;
  finally
    EndUpdate;
  end;
end;

{**********************************************************
                FRESH FILE LIST
**********************************************************}
procedure TkpFileListView.FreshList(NewDir: string);
var OldCapture: HWND;
begin
    if Pos(SpecDirAlias, FDirectory) <> 0 then Exit;
    Screen.Cursor := crHourGlass;
    OldCapture := SetCapture(Handle);
    FIsRefreshing := True;
  try
   repeat
     FNeedRefresh  := False;
     FFileList.FillByFiles(NewDir, [fotDirectory,fotFiles]);
   until not FNeedRefresh;

  finally
    AssignFileList;
    SetCapture(OldCapture);
    Screen.Cursor := crDefault;
    FIsRefreshing := False;
  end;

  if Assigned(FOnFileListChange) then OnFileListChange(self);

end;

{**********************************************************
                SET MASK
**********************************************************}
procedure TkpFileListView.SetMask(Value: string);
begin
    if AnsiCompareText(Fmask,Value)<>0 then begin
        FMask := Value;
        FreshList(FDirectory);
    end;
end;

{**********************************************************
                SET NOTIFICATION
**********************************************************}
procedure TkpFileListView.SetNotification(Value: Boolean);
var NotificationOptions: TNotificationOptions;
begin
    if Value <> FNotification then begin
       FNotification := Value;

       if FNotification then begin
          if ftDirectory in FFileType then
               NotificationOptions := [fnoFile,fnoFolder]
          else NotificationOptions := [fnoFile];
          if not Assigned(FFileNotify) then begin
             FFileNotify := TFileNotify.Create(Self,NotificationOptions);
             with FFileNotify do begin
               AsyncNotify := False;
               OnNotify := ChangeFileSystem;
               Prepare;
             end;
          end;
          if not (csDesigning in ComponentState) then begin
             FFileNotify.Folder := FDirectory;
             FFileNotify.StartMonitoring;
          end;
       end
       else begin
          FFileNotify.StopMonitoring;
       end;
    end;
end;

{**********************************************************
                SET NET BROWSE
**********************************************************}
procedure TkpFileListView.SetNetBrowse(Value: Boolean);
var OldDir: string;
begin
    if FNetBrowse = Value then Exit;
    FNetBrowse := Value;
    if Pos(SpecDirAlias, FDirectory) > 0 then begin
       OldDir := FDirectory;
       FDirectory := '';
       SetDirectory(OldDir);
    end;
end;


{**********************************************************
                SET FILE TYPE
**********************************************************}
procedure  TkpFileListView.SetFileType(NewFileType: TFileType);
begin
     if NewFileType = FFileType then Exit;

     if  (FFileNotify<>nil) and
         ((ftDirectory in NewFileType) <>
          (fnoFolder in FFileNotify.Options)) then
       if (ftDirectory in NewFileType) then
            FFileNotify.Options := [fnoFile,fnoFolder]
       else FFileNotify.Options := [fnoFile];

     FFileType := NewFileType;
     AssignFileList;
end;

{**********************************************************
                GET/SET SHOW FOLDERS
**********************************************************}
procedure TkpFileListView.SetShowFolders(Value: Boolean);
begin
   if Value then
         FileType := FileType + [ftDirectory]
   else  FileType := FileType - [ftDirectory];
end;

function TkpFileListView.GetShowFolders: Boolean;
begin
   Result := ftDirectory in FileType;
end;

procedure TkpFileListView.SetFoldersOnly(Value: Boolean);
begin
   if FFoldersOnly = Value then Exit;
   FFoldersOnly := Value;
   AssignFileList;
end;

{**********************************************************
                CAN BROWSE NET
**********************************************************}
function TkpFileListView.CanBrowseNet: Boolean;
begin
  Result := True;
  if Assigned(FFolderTree) and FFolderTree.NetBrowse then Exit;
  if Assigned(FFolderCombo) and FFolderCombo.NetBrowse then Exit;
  Result := FNetBrowse;
end;

{**********************************************************
                ONE LEVEL UP
**********************************************************}
procedure TkpFileListView.OneLevelUp;
begin
  if Assigned(FFolderTree) then begin
     FFolderTree.OneLevelUp;
     Exit;
  end;
  if Assigned(FFolderCombo) then begin
     FFolderCombo.OneLevelUp;
     Exit;
  end;
  if Pos(SpecDirAlias, FDirectory) = 0 then
     SetDirectory( OneLevelUpDirectory(Directory) );
end;

{**********************************************************
                BEGIN/END UPDATE
**********************************************************}
procedure TkpFileListView.BeginUpdate;
begin
     if FUpdateCount = 0 then LockWindowUpdate(Handle);
     Inc(FUpdateCount);
end;
procedure TkpFileListView.EndUpdate;
begin
     Dec(FUpdateCount);
     if FUpdateCount = 0 then LockWindowUpdate(0);
end;

{**********************************************************
                GET DIRECTORY
**********************************************************}
function TkpFileListView.GetDirectory: string;
begin
  Result := PureDirectory(FDirectory);
end;

{**********************************************************
                SET DIRECTORY
**********************************************************}
procedure TkpFileListView.SetDirectory(NewDirectory: string);
var i, p: integer;
    Obj: PFileObject;
    NetResource: PNetResource; 
    OldCapture: HWND;
    ReadyList: Boolean;
begin

  if (Pos(ShareAlias, NewDirectory) = 1) then begin
     NewDirectory := Copy(NewDirectory, 4, 999);
     p := Pos(SpecDirAlias, NewDirectory);
     if p > 0 then NewDirectory := Copy(NewDirectory,1,p-1);
     if (NewDirectory <> '') and (NewDirectory[Length(NewDirectory)] <> '\') then
        NewDirectory := NewDirectory + '\';
  end;

  if (AnsiCompareFileName(NewDirectory, FDirectory) = 0) then Exit;
  if not Assigned(Parent) then Exit;

  Screen.Cursor := crHourGlass;
  OldCapture := SetCapture(Handle);
try
       // Рабочий стол
  if Pos(DeskTopAlias, NewDirectory) > 0  then begin
     BeginUpdate;
   try 
     Items.Clear;
     SetNameSpace(nsMisc);
     FFileList.Clear;
     Obj := GetMyComputerRoot;
     FFileList.Add(Obj);
     with Items.Add do begin
       Caption := Obj^.DisplayName;
       Data := Obj;
       ImageIndex := Obj^.ImageIndex;
     end;
     if CanBrowseNet then begin
        Obj := GetNetworkRoot;
        FFileList.Add(Obj);
        with Items.Add do begin
          Caption := Obj^.DisplayName;
          Data := Obj;
          ImageIndex := Obj^.ImageIndex;
        end;
     end;
   finally
     EndUpdate;
     FDirectory := NewDirectory;
   end;
     Exit;
  end;

       // Мой компьютер
  if Pos(MyComputerAlias, NewDirectory) > 0  then begin
     BeginUpdate;
   try
     Items.Clear;
     ReadyList := False;
     if not FFreshDrives then begin
        if Assigned(FFolderTree) then
           ReadyList := FFolderTree.FillFileList(NewDirectory, FFileList);
        if not ReadyList and Assigned(FFolderCombo) then
           ReadyList := FFolderCOmbo.FTree.FillFileList(NewDirectory, FFileList);
     end;
     if not ReadyList then
        FFileList.FillByDrives(FFreshDrives);
     SetNameSpace(nsDrives);
     for i:=0 to FFileList.Count-1 do begin
       Obj := PFileObject(FFileList[i]);
       with Items.Add do begin
          Caption := Obj^.DisplayName;
          Data := Obj;
          ImageIndex := Obj^.ImageIndex;
          OverlayIndex := Obj^.OverlayIndex;
          SubItems.Add(Obj^.Description); // тип диска
          SubItems.Add(StrFileSize(Obj^.Size));
          SubItems.Add(StrFileSize(Obj^.FreeSize));
          SubItems.Add(StrFileAttr(Obj^.Attrs));
       end;
     end;
   finally
     EndUpdate;
     FDirectory := NewDirectory;
   end;
     Exit;
  end;

     //  Сетевые контейнеры
  if (Pos(SpecDirAlias, NewDirectory) > 0) then begin
     FDirectory := NewDirectory;
     BeginUpdate;
   try
     Items.Clear;
     SetNameSpace(nsMisc);
     if CanBrowseNet then begin
        p := Pos(SpecDirAlias, Copy(NewDirectory,3,999));
        if p > 0 then begin
           ReadyList := False;
           NetResource := PNetResource(StrToInt(Copy(NewDirectory,p+4,999)));
           NewDirectory := Copy(NewDirectory,1,p+1);
           if Assigned(FFolderTree) then
              ReadyList := FFolderTree.FillFileList(NewDirectory, FFileList);
           if not ReadyList and Assigned(FFolderCombo) then
              ReadyList := FFolderCOmbo.FTree.FillFileList(NewDirectory, FFileList);
           if not ReadyList then
              FFileList.FillFromNethood(NetResource);
           for i:=0 to FFileList.Count-1 do begin
             Obj := PFileObject(FFileList[i]);  // уничтожаются при очистке FFileList!!!
             with Items.Add do begin
                Caption := LastDirectory(Obj^.DisplayName);
                Data := Obj;
                ImageIndex := Obj^.ImageIndex;
                OverlayIndex := Obj^.OverlayIndex;
                SubItems.Add(Obj^.Description);
             end;
           end;
        end;
     end;
   finally
     EndUpdate;
   end;
     Exit;
  end;

  if (FNameSpace <> nsFiles) or (Columns[0].Caption = '') then begin
     FNameSpace := nsNone;
     SetNameSpace(nsFiles);
  end;

  FDirectory := NewDirectory;
  if (FDirectory <> '') and (FDirectory[Length(FDirectory)] <> '\') then
     FDirectory := FDirectory + '\';
     
  FreshList(FDirectory);

  if FFileNotify<>nil then begin
     if FFileNotify.Folder<>'' then FWaitFirstNotify := True;
     FFileNotify.Folder := FDirectory;
  end;

finally
   if Pos(SpecDirAlias, NewDirectory) = 0 then begin
      if Assigned(FFolderTree) then FFolderTree.SetDirectory(FDirectory);
      if Assigned(FFolderCombo) then FFolderCombo.SetDirectory(FDirectory);
   end;
   SetDirLabelCaption(FDirLabel, FDirectory, Width);
   Screen.Cursor := crDefault;
   SetCapture(OldCapture);
end;

end;

{**********************************************************
                IS SPECIAL FOLDER
**********************************************************}
function TkpFileListView.IsSpecialFolder: Boolean;
begin
   Result := (Pos(SpecDirAlias,FDirectory) > 0);
end;

{**********************************************************
                SET FOLDER TREE
**********************************************************}
procedure TkpFileListView.SetFolderTree(const Value: TkpFolderTreeView);
begin
   if FFolderTree <> Value then begin
      if FFolderCombo <> nil then
         FFolderCombo.FTree.Notification(Self, opRemove);
      FFolderCombo := nil;
      if FFolderTree <> nil then
         FFolderTree.Notification(Self, opRemove);
      FFolderTree := Value;
      if FFolderTree <> nil then begin
         FreeNotification(FFolderTree);
         FFolderTree.AddViewChild(Self);
         SetDirectory( FFolderTree.FDirectory );
      end;
   end;
end;

{**********************************************************
                SET FOLDER COMBO
**********************************************************}
procedure TkpFileListView.SetFolderCombo(const Value: TkpFolderCombo);
begin
   if FFolderCombo <> Value then begin
      if FFolderCombo <> nil then
         FFolderCombo.FTree.Notification(Self, opRemove);
      FFolderCombo := Value;
      if FFolderTree <> nil then
         FFolderTree.Notification(Self, opRemove);
      FFolderTree := nil;
      if FFolderCombo <> nil then begin
         FreeNotification(FFolderCombo);
         FFolderCombo.FTree.AddViewChild(Self);
         SetDirectory( FFolderCombo.FTree.FDirectory );
      end;
   end;
end;

{**********************************************************
          GET FILE SIZE
**********************************************************}
function  TkpFileListView.GetFileSize(Index: integer): extended;
var Obj: PFileObject;
begin
  Result := 0;
  Obj := PFileObject(Items[Index].Data);
  if Assigned(Obj) then Result := Obj^.Size;
end;

{**********************************************************
          GET SEL SIZE
**********************************************************}
function  TkpFileListView.GetSelSize: extended;
var i, Count: integer;
begin
  Result := 0;
  Count := 0;
  for i:=0 to Items.Count-1 do
    if Items[i].Selected then begin
      if FNameSpace = nsFiles  then
         Result := Result + GetFileSize(i)
      else
      if FNameSpace = nsDrives then
         if (Count > 0) then begin
            Result := - 1;
            Exit;
         end
         else Result := Result + GetFileSize(i);
      Inc(Count);
    end;
end;

{**********************************************************
          GET ITEMS COUNT
**********************************************************}
function TkpFileListView.GetItemsCount: integer;
begin
  Result := Items.Count;
end;

{**********************************************************
          GET SEL COUNT
**********************************************************}
function TkpFileListView.GetSelCount: integer;
var i: integer;
begin
  Result := 0;
  for i:=0 to Items.Count-1 do
    if Items[i].Selected then Inc(Result);
end;

{**********************************************************
          GET TOTAL SIZE
**********************************************************}
function  TkpFileListView.GetTotalSize: extended;
var i: integer;
begin
  if FNameSpace = nsDrives then begin
     Result := - 1;
     Exit;
  end;
  Result := 0;
  for i:=0 to Items.Count-1 do
    Result := Result + GetFileSize(i);
end;


{**********************************************************
                CREATE
**********************************************************}
constructor TkpFolderTreeView.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);

    FIsSettingPath := False;

    inherited OnEdited    := NewOnEdited;
    inherited OnDeletion  := NewOnDeletion;

    FNotifyList := TList.Create;
    FViewList := TList.Create;

    FNeedRefresh := False;
    FNetBrowse := False;
    FNotification := False;
    FAltColor := clBlue;

    FTimer := TTimer.Create(Self);
    FTimer.Enabled := False;
    FTimer.Interval := 1000;
    FTimer.OnTimer := Self.OnTimer;

    FIsSettingParent := False;
end;

{**********************************************************
                DESTROY
**********************************************************}
destructor TkpFolderTreeView.Destroy;
begin
    inherited Destroy;
    if FNotifyList <> nil then FNotifyList.Free;
    if FViewList <> nil  then FViewList.Free;
end;

{**********************************************************
                CREATE PARAMS
**********************************************************}
procedure TkpFolderTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := Style or WS_BORDER;
    ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

{**********************************************************
                SET PARENT
**********************************************************}
procedure TkpFolderTreeView.SetParent(AParent: TWinControl);
begin
   FIsSettingParent := True;
   inherited;     
   FIsSettingParent := False;
end;

{**********************************************************
                CREATE WND
**********************************************************}
procedure TkpFolderTreeView.CreateWnd;
{$J+}
const FIrstTime: Boolean = True;
    {----------- ADD DRIVES ------------------}
    procedure AddDrives;
    var DriveList: TFileList;
        i: integer;
        NewNode: TTreeNode;
        FileNotify: TFileNotify;
        Obj: PFileObject;
    begin
       DriveList := TFileList.Create;
    try
       DriveList.FillByDrives(FFreshDrives);
       for i:=0 to DriveList.Count-1 do begin
          Obj := PFileObject(DriveList[i]);
          if Obj^.DriveType in [DRIVE_FIXED,DRIVE_RAMDISK] then begin
             FileNotify := TFileNotify.Create(Self, [fnoFolder,fnoWatchSubTree]);
             FNotifyList.Add(FileNotify);
             with FileNotify do begin
                AsyncNotify := False;
                OnNotify := ChangeFileSystem;
                Folder := Obj^.ParsingName;
                Prepare;
             end;
             if FNotification and not (csDesigning in ComponentState) then
                  FileNotify.StartMonitoring
             else FileNotify.StopMonitoring;
             Obj^.FileNotify := FileNotify;
          end;

          NewNode:=Items.AddChildObject(Items[FMyComputerIndex],
                                        Obj^.DisplayName, TObject(Obj));
          NewNode.ImageIndex := Obj^.ImageIndex;
          NewNode.SelectedIndex := Obj^.ImageIndex;
          AddDummyChild(NewNode);
          DriveList[i] := nil;
       end;
    finally
       DriveList.Free;
    end;
    end;

begin
   inherited;

   if Items.Count = 0 then begin
      Images := TImageList(kpSmallImageList);
                  // Добавить "Рабочий стол"
      with Items.Add(nil,DesktopName) do begin
         ImageIndex := DesktopIconIndex;
         SelectedIndex := ImageIndex;
      end;
                  // Добавить "Мой компьютер"
      with Items.AddChild(TopItem,MyComputerName) do begin
         ImageIndex := MyComputerIconIndex;
         SelectedIndex := ImageIndex;
         FMyComputerIndex := Items.Count-1;
      end;

      AddDrives; // Получить иконки для всех дисков

      Items[0].Expand(False);
      Items[FMyComputerIndex].Expand(False);

   end;

   if FirstTime then begin
//      AddNetworkIcons;
      FirstTime := False;
   end;

   if IsDroppedDown and not (csDesigning in ComponentState) then begin
      Windows.SetParent(Handle, 0);
      CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
   end;

   if FDirectory = '' then SetDirectory('C:\');
   
end;

{**********************************************************
                SET NOTIFICATION
**********************************************************}
procedure TkpFolderTreeView.SetNotification(Value: Boolean);
var  DiskFileNotify: TFileNotify;
     DiskNode: TTreeNode;
     Obj: PFileObject;
begin
    if Value = FNotification then Exit;

    FNotification := Value;
    if csDesigning in ComponentState then Exit;

    DiskNode := Items[FMyComputerIndex].GetFirstChild;
    while DiskNode <> nil do begin
       Obj := PFileObject(DiskNode.Data);
       if Assigned(Obj) then begin
          DiskFileNotify := Obj^.FileNotify;
          if (DiskFileNotify <> nil) and DiskNode.Expanded then
             if FNotification and not (csDesigning in ComponentState) then
                  DiskFileNotify.StartMonitoring
             else DiskFileNotify.StopMonitoring;
       end;
       DiskNode := Items[MyComputerIconIndex].GetNextChild(DiskNode);
    end;
end;

{**********************************************************
                SET NET BROWSE
**********************************************************}
procedure TkpFolderTreeView.SetNetBrowse(Value: Boolean);
var NetRoot:  TTreeNode;
    NetObj: PFileObject;
    NewNode: TTreeNode;
begin
    if FNetBrowse = Value then Exit;

    if not FNetBrowse then begin  // Добавить "Сетевое окружение"
       NetObj := GetNetworkRoot;
       if Assigned(NetObj) then begin
          NewNode := Items.AddChildObject(Items[0], NethoodName, NetObj);
          NewNode.HasChildren := True;
          NewNode.ImageIndex := NethoodIconIndex;
          NewNode.SelectedIndex := NewNode.ImageIndex;
          FNethoodIndex := Items.Count-1;
          AddNetworkChildren(NewNode);  // добавить первый уровень сразу
          FNetBrowse := True;
       end;
    end
    else begin
       NetRoot := Items[FNethoodIndex];
       if NetRoot <> nil then Items.Delete(NetRoot);
       FNetBrowse := False;
    end;

    ChangeNotifyLinks;

end;

{**********************************************************
        WM_MOUSEMOVE
**********************************************************}
function PointInRect(const P: TPoint; const R: TRect): Boolean;
begin
  with R do
    Result := (Left <= P.X) and (Top <= P.Y) and
      (Right >= P.X) and (Bottom >= P.Y);
end;

procedure TkpFolderTreeView.WMMouseMove(var Msg: TWMMouseMove);
var HitTest:  THitTests;
begin
  if IsDroppedDown then begin
    if FMouseInControl and Enabled then begin
      HitTest := GetHitTestInfoAt(Msg.XPos, Msg.YPos);
      if htOnLabel in HitTest then
        Selected := GetNodeAt(Msg.XPos, Msg.YPos);
    end;
    Exit;
  end;
  inherited;
end;

{**********************************************************
        CM_MOUSEENTER
**********************************************************}
procedure TkpFolderTreeView.CMMouseEnter(var Msg: TMessage);
begin
  if IsDroppedDown then begin
     FMouseInControl := True;
     ReleaseCapture;
  end;
  inherited;
end;

{**********************************************************
        CM_MOUSELEAVE
**********************************************************}
procedure TkpFolderTreeView.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if IsDroppedDown then begin
     FMouseInControl := False;
     if Visible then SetCapture(Handle);
  end;
end;

{**********************************************************
        WM_LBUTTONDOWN
**********************************************************}
{$J+}
procedure TkpFolderTreeView.WMLButtonDown(var Msg: TWMLButtonDown);
const LastTime: DWORD = 0;
      OldX: integer = - 1;
      OldY: integer = - 1;
var ThisTime: DWORD;
    HitTest:  THitTests;
begin
  if IsDroppedDown then
     if not FMouseInControl then begin
         ReleaseCapture;
         Enabled := False;
         Visible := False;
         (Self.Parent as TkpFolderCombo).CancelBrowse;
         LastTime := 0;
         Exit;
     end
     else begin
          HitTest := GetHitTestInfoAt(Msg.XPos, Msg.YPos);
          if (htOnItem in HitTest) then begin
             ReleaseCapture;
             (Self.Parent as TkpFolderCombo).EndBrowse;
             Exit;
          end;
     end;

{     Этот кусок позволяет сохранить
      возможность раскрытия дерева двойным щелчком }
    ThisTime := GetTickCount;
    if (LastTime > 0) and (ThisTime - LastTime < GetDoubleClickTime) then begin
       with TMessage(Msg) do
         Perform(WM_LBUTTONDBLCLK, WParam, LParam);
       Msg.Result := 1;
       LastTime := 0;
       Exit;
    end;

    LastTime := ThisTime;
    OldX := Msg.XPos;
    OldY := Msg.YPos;

    inherited;
end;

{**********************************************************
         KEY DOWN
**********************************************************}
procedure TkpFolderTreeView.KeyDown(var Key: Word; Shift: TShiftState);
var Node: TTreeNode;
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_RIGHT) and
     ((ssCtrl in Shift) or (ssShift in Shift)) then begin
     with Selected do if not Expanded then Expand(False);
     Key := 0;
  end;

  if (Key = VK_LEFT) and
     ((ssCtrl in Shift) or (ssShift in Shift)) then begin
     with Selected do if Expanded then Collapse(True);
     Key := 0;
  end;

  if Key = VK_DOWN then begin
     if Selected = nil then Exit;
     with Selected do begin
        Node := GetNextVisible;
        if Node <> nil then begin
           if not IsDroppedDown then DelayedFreshing;
           Node.Selected := True;
        end;
     end;
     Key := 0;
  end;

  if Key = VK_UP then begin
     if Selected = nil then Exit;
     with Selected do begin
        Node := GetPrevVisible;
        if Node <> nil then begin
           if not IsDroppedDown then DelayedFreshing;
           Node.Selected := True;
        end;
     end;
     Key := 0;
  end;

end;

{**********************************************************
         IS ALT COLOR
**********************************************************}
function TkpFolderTreeView.IsAltColor(Node: TTreeNode): Boolean;
begin
   if Assigned(Node.Data) then
        Result := PFileObject(Node.Data)^.Attrs and faCompressed <> 0
   else Result := False;
end;

{**********************************************************
         GET CUSTOM DRAW COLORS
**********************************************************}
procedure TkpFolderTreeView.GetCustomDrawColors(Node: TTreeNode;
                   State: TCustomDrawState; var ColText, ColBk: TColor);
begin
   if not (cdsSelected in State) then
      if IsAltColor(Node) then ColText := FAltColor;
end;

{**********************************************************
         CN_NOTIFY
**********************************************************}
procedure TkpFolderTreeView.CNNotify(var msg: TWMNotify);
var TextCol: TColor;
    BackCol: TColor;
    State: TCustomDrawState;
    Node: TTreeNode;
begin
  if msg.NMHdr^.code = NM_CUSTOMDRAW then
    with PNMTVCustomDraw(msg.NMHdr)^ do begin
      if (nmcd.dwDrawStage = CDDS_PREPAINT) then
          msg.Result := CDRF_DODEFAULT or CDRF_NOTIFYITEMDRAW
      else
      if (nmcd.dwDrawStage and CDDS_ITEMPREPAINT) <> 0 then begin
          TextCol := TColor(clrText);
          BackCol := TColor(clrTextBk);
          Node := Items.GetNode(HTreeItem(nmcd.dwItemSpec));
          State := TCustomDrawState(Word(nmcd.uItemState));
          GetCustomDrawColors(Node, State, TextCol, BackCol);
          clrText  := ColorToRGB(TextCol);
          clrTextBk := ColorToRGB(BackCol);
          msg.Result:=CDRF_DODEFAULT;
          msg.Result:=msg.Result or CDRF_NOTIFYSUBITEMDRAW;
      end;
      Exit;
    end;

  inherited;
end;

{**********************************************************
                IS DROPPED DOWN
**********************************************************}
function TkpFolderTreeView.IsDroppedDown: Boolean;
begin
   Result := Parent is TkpFolderCombo;
end;

{**********************************************************
         SET ALT COLOR
**********************************************************}
procedure TkpFolderTreeView.SetAltColor(const Value: TColor);
begin
  FAltColor := Value;
  if Visible then Invalidate;
end;

{**********************************************************
                SET DIR LABEL
**********************************************************}
procedure TkpFolderTreeView.SetDirLabel (Value: TLabel);
begin
  FDirLabel := Value;
  if Value <> nil then begin
     Value.FreeNotification(Self);
     SetDirLabelCaption(Value, FDirectory, Width);
  end;
end;

{**********************************************************
                ADD/DELETE DUMMY CHILD
**********************************************************}
procedure TkpFolderTreeView.AddDummyChild(Node: TTreeNode);
begin
     Items.AddChild(Node, DummyChildName);
end;
procedure TkpFolderTreeView.DeleteDummyChild(Node: TTreeNode);
var DummyNode: TTreeNode;
begin
    DummyNode := Node.GetFirstChild;
    if (DummyNode <> nil) and (DummyNode.Text = DummyChildName) then
       Items.Delete(DummyNode);
end;

{**********************************************************
                BEGIN/END UPDATE
**********************************************************}
procedure TkpFolderTreeView.BeginUpdate;
begin
     if FUpdateCount = 0 then LockWindowUpdate(Handle);
     Inc(FUpdateCount);
end;
procedure TkpFolderTreeView.EndUpdate;
begin
     Dec(FUpdateCount);
     if FUpdateCount = 0 then LockWindowUpdate(0);
end;

{**********************************************************
                GET DIRECTORY
**********************************************************}
function TkpFolderTreeView.GetDirectory: string;
begin
  Result := PureDirectory(FDirectory);
end;

{**********************************************************
                FILL FILE LIST
**********************************************************}
function TkpFolderTreeView.FillFileList(Path: string; FileList: TFileList): Boolean;
var Child: TTreeNode;
    Obj: PFileObject;
begin
   Result := False;
   if (AnsiCompareFileName(Path, FDirectory) <> 0) then Exit;
   if Pos(SpecDirAlias,Path) = 0 then Exit;
   if (Selected = nil) or
      ((Selected <> Items[FMyComputerIndex])and (not Selected.Expanded)) then Exit;

   FileList.Clear;
   Child := Selected.GetFirstChild;
   while Child <> nil do begin
     Obj := PFileObject(Child.Data);
     FileList.Add( CopyFileObject(Obj) );
     Child := Selected.GetNextChild(Child);
   end;
   Result := True;
end;

{**********************************************************
                SET DIRECTORY
**********************************************************}
procedure TkpFolderTreeView.SetDirectory(NewDirectory: string);
begin
  FIsSettingPath := True;
  try
   if NewDirectory = '' then Exit;
   if (AnsiCompareFileName(NewDirectory, FDirectory) = 0) then Exit;
   if (Pos(ShareAlias,FDirectory) > 0) and
      (AnsiCompareFileName(NewDirectory, Copy(FDirectory,4,999)) = 0) then Exit;
   repeat
     FNeedRefresh := False;
     try ExpandPath(NewDirectory,False); except end;
     Change(Selected);
   until not FNeedRefresh;
 finally
   FIsSettingPath := False;
 end;
end;

{**********************************************************
                IS SPECIAL FOLDER
**********************************************************}
function TkpFolderTreeView.IsSpecialFolder: Boolean;
begin
   Result := (Pos(SpecDirAlias,FDirectory) > 0);
end;

{**********************************************************
                GET DISK PARENT NODE
**********************************************************}
function TkpFolderTreeView.GetDiskParentNode(Node: TTreeNode): TTreeNode;
begin
    Result := Node;
    while Result.Level > 2 do Result := Result.Parent;
end;

{**********************************************************
                IS NETWORK NODE
**********************************************************}
function TkpFolderTreeView.IsNetworkNode(Node: TTreeNode): Boolean;
var RootNode: TTreeNode;
begin
    Result := False;
    if not FNetBrowse then Exit;
    RootNode := Node;
    while RootNode.Level > 1 do RootNode := RootNode.Parent;
    Result := RootNode.ImageIndex = NethoodIconIndex;
end;

{**********************************************************
                IS NETWORK TYPE
**********************************************************}
function TkpFolderTreeView.IsNetworkEntity(Node: TTreeNode;
                           EntityType: DWORD): Boolean;
var Obj: PFileObject;
begin
    Result := False;
    Obj := PFileObject(Node.Data);
    if Assigned(Obj) then
       if Assigned(Obj^.NetResource) then
          Result := (Obj^.NetResource^.dwDisplayType = EntityType);
end;

function TkpFolderTreeView.IsNetworkNetwork(Node: TTreeNode): Boolean;
begin
   Result := IsNetworkEntity(Node,RESOURCEDISPLAYTYPE_NETWORK);
end;

function TkpFolderTreeView.IsNetworkDomain(Node: TTreeNode): Boolean;
begin
   Result := IsNetworkEntity(Node,RESOURCEDISPLAYTYPE_DOMAIN);
end;

function TkpFolderTreeView.IsNetworkServer(Node: TTreeNode): Boolean;
begin
   Result := IsNetworkEntity(Node,RESOURCEDISPLAYTYPE_SERVER);
end;

function TkpFolderTreeView.IsNetworkShare(Node: TTreeNode): Boolean;
begin
   Result := IsNetworkEntity(Node,RESOURCEDISPLAYTYPE_SHARE);
end;

function TkpFolderTreeView.IsNetworkContainer(Node: TTreeNode): Boolean;
var Obj: PFileObject;
begin
    Result := False;
    Obj := PFileObject(Node.Data);
    if Assigned(Obj) then
       if Assigned(Obj^.NetResource) then
          Result := (Obj^.NetResource^.dwDisplayType <> RESOURCEDISPLAYTYPE_SHARE);
end;


{**********************************************************
                GET NODE PATH
**********************************************************}
function TkpFolderTreeView.GetNodePath(Node: TTreeNode): string;
var TempNode: TTreeNode;
    Obj: PFileObject;
begin
    Result := '';
    if Node.Level < 2 then begin // специальные случаи
        if Node.Level = 0 then Result := DesktopAlias + DesktopName
        else
        if Node.ImageIndex = MyComputerIconIndex then
             Result := MyComputerAlias + MyComputerName
        else Result := NethoodAlias + NethoodName;
        Exit;
    end;

    if IsNetworkNode(Node) then begin
       Obj := PFileObject(Node.Data);
       if Assigned(Obj^.NetResource) and
          (Obj^.NetResource^.dwDisplayType <> RESOURCEDISPLAYTYPE_SHARE) then begin
          case Obj^.NetResource^.dwDisplayType of
             RESOURCEDISPLAYTYPE_NETWORK: Result := NetworkAlias;
             RESOURCEDISPLAYTYPE_DOMAIN:  Result := DomainAlias;
             RESOURCEDISPLAYTYPE_SERVER:  Result := ServerAlias;
             RESOURCEDISPLAYTYPE_SHARE:   Result := ShareAlias;
             else Result := '';
          end;
          if Obj^.NetResource^.dwDisplayType = RESOURCEDISPLAYTYPE_SERVER then
             Result := Result + '\\';
          Result := Result + Node.Text;
          Exit;
       end;
            // каталог на сервере
       TempNode := Node.Parent;
       Result := Node.Text+'\';
       while not IsNetworkServer(TempNode) do begin
          Result := TempNode.Text + '\' + Result;
          TempNode := TempNode.Parent;
       end;
       Result := '\\' + TempNode.Text + '\' + Result;
       Exit;
    end;

    if Node.Level = 2 then begin  // корневой каталог диска
       Result := PFileObject(Node.Data)^.ParsingName;
       Exit;
    end;

    Result := Node.Text+'\';
    TempNode := Node.Parent;
    while TempNode.Level > 2 do begin
       Result := TempNode.Text + '\' + Result;
       TempNode := TempNode.Parent;
    end;
    Result := PFileObject(TempNode.Data)^.ParsingName + Result;

end;

{**********************************************************
                ADD DIRECTORIES
**********************************************************}
function TkpFolderTreeView.AddDirectories(Node: TTreeNode): Boolean;
var NewNode: TTreeNode;
    i: integer;
    DirName: string;
    DirList: TFileList;
    OldCapture: HWND;
    Obj: PFileObject;
begin

    DirName := GetNodePath(Node);
    if Pos(ShareAlias,DirName) = 1 then
       DirName := Copy(DirName,4, 999);

    Result := False;

    if (csDesigning in ComponentState) and
       (IsNetworkNode(Node) or (DirName[1] < 'C')) then Exit;

    Screen.Cursor := crHourGlass;
    OldCapture := SetCapture(Handle);
    BeginUpdate;
  try

    FIsAddingTree := True;

    if IsNetworkContainer(Node) then
      Result := AddNetworkChildren(Node)
    else begin
      DirList := TFileList.Create;
      if not DirList.FillByFiles(DirName, [fotDirectory]) then begin
         if IsDroppedDown then begin
            Enabled := False;
            Visible := False;
            (Parent as TkpFolderCombo).CancelBrowse;
         end;
         MessageDlg(Format(SNoDiskInDrive,[DirName[1]]), mtError, [mbOK], 0);
      end;
      DirList.Sort;
      for i:=0 to DirList.Count-1 do begin
        Obj := PFileObject(DirList[i]);  
        NewNode:=Items.AddChildObject(Node, Obj^.DisplayName, Obj);
        NewNode.ImageIndex := FolderIconIndex;
        NewNode.SelectedIndex := OpenFolderIconIndex;
        if HasSubdirs(DirName+Obj^.DisplayName+'\') then
           AddDummyChild(NewNode);
        DirList[i] := nil;
      end;
      DirList.Free;
      Result := True;
    end;
  finally
    EndUpdate;
    Screen.Cursor := crDefault;
    SetCapture(OldCapture);
    FIsAddingTree := False;
  end;

end;

{**********************************************************
         DELAYED FRESHING
**********************************************************}
procedure TkpFolderTreeView.DelayedFreshing;
begin
  FTimer.Enabled := False;
  FTimer.Enabled := True;
end;

{**********************************************************
                ONE LEVEL UP
**********************************************************}
procedure TkpFolderTreeView.OneLevelUp;
var Node: TTreeNode;
begin
   Node := Selected;
   if Assigned(Node) and (Node.Level > 0) then begin
      Node.Parent.Selected := True;
      if AllChainVisible(Self) then SetFocus;
   end;
end;

{**********************************************************
       IS NODE EXPANDED
**********************************************************}
function IsNodeExpanded(Node: TTreeNode): Boolean;
var ChildNode : TTreeNode;
begin
    ChildNode := Node.GetFirstChild;
    Result := (ChildNode<>nil) and (ChildNode.Text<>DummyChildName);
end;

{**********************************************************
        REFRESH
**********************************************************}
procedure TkpFolderTreeView.Refresh;
begin
  ChangeFileSystem(Self, fnoFile);
end;

{**********************************************************
                CHANGE FILE SYSTEM
**********************************************************}
procedure TkpFolderTreeView.ChangeFileSystem(
            Sender :TObject; Action :TNotificationOption);
var  OldDir, NotifyDiskRoot, SelDiskRoot: string;
     {----------------- IS DRIVE EXPANDED ---------------------}
     function IsDriveExpanded(NotifyFrom: TFileNotify): Boolean;
     var DriveNode: TTreeNode;
         Obj: PFileObject;
     begin
       Result := False;
       DriveNode := Items[FMyComputerIndex].GetFirstChild;
       while DriveNode <> nil do begin
         Obj := PFileObject(DriveNode.Data);
         if Obj^.FileNotify = NotifyFrom then begin
            Result := IsNodeExpanded(DriveNode);
            NotifyDiskRoot := Obj^.ParsingName;
            Exit;
         end;
         DriveNode := Items[FMyComputerIndex].GetNextChild(DriveNode);
       end;
     end;
//-------------------------------------------------------------
begin
    if (Sender = Self) or IsDriveExpanded(TFileNotify(Sender)) then begin
       if not FIsSettingPath then begin
          OldDir := FDirectory;
          FDirectory := '';
            // чтобы работало даже в невидимом состоянии !!!
          try ExpandPath(OldDir,IsNodeExpanded(Selected)); except end;
          SelDiskRoot := Copy(GetNodePath(Selected), 1, 3);
//          if Sender = Self then NotifyDiskRoot := SelDiskRoot;
          if OldDir <> FDirectory then  // удален текущий узел
               Change(Selected)
          else if SelDiskRoot = NotifyDiskRoot then ChangeNotifyLinks;
       end
       else FNeedRefresh := True;
    end;
end;

{**********************************************************
                ADD NETWORK CHILDREN
**********************************************************}
function TkpFolderTreeView.AddNetworkChildren(Node: TTreeNode): Boolean;
var i: Integer;
    NetList: TFileList;
    RootNet: PNetResource;
    Obj: PFileObject;
    NewNode: TTreeNode;
begin

  NetList := TFileList.Create;
try
  Result := False;
  if not Assigned(Node.Data) then Exit;

  RootNet := PFileObject(Node.Data)^.NetResource;

  NetList.FillFromNethood(RootNet);
  if NetList.Count = 0 then begin
    if IsNetworkServer(Node) then begin
       Node.ImageIndex := ServerNoAccessIconIndex;
       Node.SelectedIndex := Node.ImageIndex;
    end;
    Node.HasChildren := False;
  end;

  for i:=0 to NetList.Count-1 do begin
      Obj := PFileObject(NetList[i]);
      NewNode := Items.AddChildObject(Node,
                 LastDirectory(Obj^.DisplayName), TObject(Obj));
      with NewNode do begin
         HasChildren := True;
         ImageIndex := Obj^.ImageIndex;
         if ImageIndex = FolderIconIndex then
              SelectedIndex := OpenFolderIconIndex
         else SelectedIndex := ImageIndex;
      end;
      NetList[i] := nil;
  end;

finally
  Result := NetList.Count > 0;
  NetList.Free;
end;
end;

{**********************************************************
                NEW ON DELETION
**********************************************************}
procedure TkpFolderTreeView.NewOnDeletion(Sender: TObject;
                                           Node: TTreeNode);
begin
     inherited;
     if (Node.Data <> nil) and (not FIsSettingParent) then
        FreeFileObject ( PFileObject(Node.Data) );
end;

{**********************************************************
                NEW ON EDITED
**********************************************************}
procedure TkpFolderTreeView.NewOnEdited(Sender: TObject;
                        Node: TTreeNode; var S: string);
type ChangeProc = procedure (Sender :TObject;Action :TNotificationOption) of object;
var OldPath, Path: string;
    i: integer;
    Method: TMethod;
    //-----------------------------------------------------
    function RenamedFolder(OldPath, NewName: string): string;
    var pos: integer;
    begin
       pos := Length(OldPath);
       if OldPath[pos] = '\' then Dec(pos);
       while (pos > 0)  and  (OldPath[pos] <> '\') do Dec(pos);
       Result := Copy(OldPath, 1, pos) + NewName;
    end;
    //-----------------------------------------------------
begin
    if Assigned(FUserOnEdited) then FUserOnEdited(Sender, Node, S);

    if (Node<>nil) then begin
       if FIsEditingNew then begin
          Node.Text := S;
          Path := GetNodePath(Node);
          if not CreateDir(Path) then begin
             MessageDlg(SCreateFailure + ' '+ Path, mtError, [mbOK], 0);
             Items.Delete(Node);
             S := '';
          end
          else begin
             Node.Parent.AlphaSort;
             if Selected <> Node then
                for i:=0 to FViewList.Count-1 do begin
                  Method.Data := TObject(FViewList[i]);
                  Method.Code := TObject(FViewList[i]).MethodAddress('ChangeFileSystem');
                  if Assigned(Method.Code) then ChangeProc(Method)(Self,fnoFolder);
                end;
             if Assigned(FOnFolderCreated) then FOnFolderCreated(Sender, S);
           end;
       end
       else begin  //--- change file name
          if Pos('?', FDirectory) > 0 then begin //--- special virtual folder or disk
               OldPath := Node.Text;
               Path := S;
               S := '';
          end
          else begin   //--- ordinary folder
               OldPath := GetNodePath(Node);
               i := Length(OldPath);
               if OldPath[i] = '\' then
                  OldPath := Copy(OldPath, 1, i-1); 
               Path := RenamedFolder(OldPath, S);
               if not RenameFile(OldPath, Path) then S := '';
          end;
          if Assigned(FOnFileRename) then
             FOnFileRename(Self, OldPath, Path, (S <> ''), True);
       end;
    end;
end;

{**********************************************************
                EDIT
**********************************************************}
procedure TkpFolderTreeView.Edit(const Item: TTVItem);
var
  S: string;    
  Node: TTreeNode;
  Action: Boolean;
begin
  with Item do begin
    { Get the node }
    if (state and TVIF_PARAM) <> 0 then
          Node := Pointer(lParam)
    else  Node := Items.GetNode(hItem);

    if pszText = nil then begin
      if FIsEditingNew then begin
        Action := True;
        if Assigned(FOnNewEditCancel) then FOnNewEditCancel(Self, Node, Action);
        if Action then Node.Destroy;
      end
      else
        if Assigned(FOnEditCancel) then
          FOnEditCancel(Self, Node);
    end
    else begin
//      inherited;
        with Item do
          if pszText <> nil then begin
             S := pszText;
             NewOnEdited(Self, Node, S);
             if (Node <> nil) and (S <> '') then Node.Text := S;
          end;
     end;
  end;
  FIsEditingNew := False;
  if Assigned(FOnEndCreating) then FOnEndCreating(Self,Node);
  Change ( Node );
end;

{**********************************************************
                CREATE FOLDER
**********************************************************}
procedure TkpFolderTreeView.CreateFolder(GoInside: Boolean);
var NewNode: TTreeNode;
begin

   if ReadOnly then Exit;

   if Assigned(FOnBeginCreating) then FOnBeginCreating(Self,Selected);

   NewNode := NewChildAndEdit(Selected, '-----');
   if (NewNode <> nil) and GoInside then Selected := NewNode;
end;

{**********************************************************
                NEW CHILD AND EDIT
**********************************************************}
function TkpFolderTreeView.NewChildAndEdit
                        (Node: TTreeNode; const S: String): TTreeNode;
var RootPath: string;
begin
    Result := nil;
    if (Node = nil) or (Node.Level < 2) then Exit;
    if IsNetworkContainer(Node) then Exit;

    RootPath := Copy(GetNodePath(Node),1,3);
    if GetDriveType(PChar(RootPath)) in [0,1,DRIVE_CDROM] then Exit;

    SetFocus;
    if (not Node.Expanded) and (Node.HasChildren) then Node.Expand(False);

    BeginUpdate;
  try
    Result := Items.AddChildFirst(Node,S);
    FIsEditingNew := True;
    with Result do begin
      ImageIndex := FolderIconIndex;
      SelectedIndex := OpenFolderIconIndex;
    end;
  finally
    EndUpdate;
  end;

    Include(TComponent_(Self).FComponentState, csDesigning);
    if (not Node.Expanded) then Node.Expand(False);
    Result.EditText;
    Exclude(TComponent_(Self).FComponentState, csDesigning);

    SetFocus;
end;

{**********************************************************
                CAN EXPAND
**********************************************************}
function TkpFolderTreeView.CanExpand(Node: TTreeNode): Boolean;
var DiskFileNotify: TFileNotify;
begin

    if (Node.Level > 1) and (not FIsSettingPath) and
       (csDesigning in ComponentState) then begin
        Result := False;
        Exit;
    end;

    Result := True;
    if Node.Level > 1 then
       with Node do begin
            BeginUpdate;
          try
            if ((csDesigning in ComponentState)
                and (not FIsSettingPath)) or FIsAddingTree then
                 Result := False
            else Result := FIsEditingNew or AddDirectories(Node);
            if Result then DeleteDummyChild(Node);
            if (Node.Level = 2) and (Node.GetFirstChild = nil) then begin
               AddDummyChild(Node);
               Result := False;
            end;
          finally
            EndUpdate;
          end;
        end;

    if Result then Result := inherited  CanExpand(Node);

    if Result then
      with Node do begin
         if ImageIndex = FolderIconIndex then
            ImageIndex := OpenFolderIconIndex;
         if not IsNetworkNode(Node) and  (Level = 2) then begin
            DiskFileNotify := PFileObject(Node.Data).FileNotify;
            if (DiskFileNotify <> nil) and FNotification
                and not (csDesigning in ComponentState) then
                DiskFileNotify.StartMonitoring;
         end;
      end;
end;

{**********************************************************
                EXPAND PATH
**********************************************************}
procedure TkpFolderTreeView.ExpandPath(Path: string; ExpandNode: Boolean);
var DirName:  string;
    LastNode, Node: TTreeNode;
    OldCapture: HWND;
{**********************************************************}
    function FindChild (Node: TTreeNode; NodeName: string;
                        Recursive: Boolean): TTreeNode;
    var TempNode:  TTreeNode;
        RealName:  string;
        InNetwork: Boolean;
    begin
        Result := nil;
        InNetwork := IsNetworkNode(Node);
        if Pos(':', NodeName) > 0 then  // диск
           NodeName := NodeName + '\';

        TempNode := Node.GetFirstChild;
        while TempNode<>nil do begin
           RealName := PFileObject(TempNode.Data)^.ParsingName;
           if Pos('\\',RealName) = 1 then
              while Pos('\',RealName) > 0 do
                 RealName := Copy(RealName,Pos('\',RealName)+1,999);
           if 0 = AnsiCompareFileName(RealName,NodeName) then begin
              Result := TempNode;
              if InNetwork and (not IsNetworkServer(TempNode))
                 and (not IsNetworkShare(TempNode))then
                   Result := nil
              else Exit;
           end;
           if Recursive and not IsNetworkServer(TempNode) then begin
              if not TempNode.Expanded then TempNode.Expand(False);
              Result := FindChild(TempNode,NodeName,True);
              if Result <> nil then Exit;
              TempNode.Collapse(True);
            end;
            TempNode := Node.GetNextChild(TempNode);
        end;
    end;
    {**********************************************************}
    function ExtractFolder(var DirPath: string): string;
    var p: integer;
    begin
        p := Pos('\', DirPath);
        if p = 0 then begin
           Result := Trim(DirPath);
           DirPath := '';
        end
        else begin
          Result := Copy(DirPath,1,p-1);
          DirPath := Copy(DirPath,p+1,999);
        end;
    end;
{**********************************************************}
begin
    FIsSettingPath := True;
    OldCapture := SetCapture(Handle);
    Screen.Cursor := crHourGlass;
    BeginUpdate;
try
         // найти узел, с которого начинается "разворачивание"
     if FNetBrowse  and  (Path[1]='\') then begin
        LastNode := Items[FNethoodIndex];
        if (Path[2]<>'\') or (LastNode = nil) then Exit;
        Path := Copy(Path,3,999);
                 // найти сервер
        DirName := ExtractFolder(Path);
        LastNode := FindChild(LastNode,DirName,True);
        if LastNode = nil then Exit;
                 // найти разделяемый ресурс
        DirName := ExtractFolder(Path);
        if not LastNode.Expanded then LastNode.Expand(False);
        if DirName <> '' then begin
           LastNode := FindChild(LastNode,DirName,False);
           if LastNode = nil then Exit;
        end;
    end
    else LastNode := Items[FMyComputerIndex];

//--------------------------------------------------------------

//    LastNode.Collapse(True);

    DirName := ExtractFolder(Path);

    while True do begin
          if Trim(DirName) = '' then break;
          if not LastNode.Expanded then LastNode.Expand(False);
          Node := FindChild(LastNode,DirName,False);
          if Node = nil then break;
          LastNode := Node;
          DirName := ExtractFolder(Path);
     end;

    LastNode.MakeVisible;
    LastNode.Selected := True;
    FDirectory := GetNodePath(LastNode);
    Change(LastNode);
    if ExpandNode then LastNode.Expand(False);

finally
    FIsSettingPath := False;
    EndUpdate;
    Screen.Cursor := crDefault;
    SetCapture(OldCapture);
    if AllChainVisible(Self) then SetFocus;
end;

end;

{**********************************************************
                COLLAPSE
**********************************************************}
procedure TkpFolderTreeView.Collapse(Node: TTreeNode);
var Child, NodeToDelete: TTreeNode;
    DiskFileNotify: TFileNotify;
begin
    BeginUpdate;
  try
    Child := Node.GetLastChild;
    while Child <> nil do begin
       Child.Collapse(True);
       NodeToDelete := Child;
       Child := Node.GetPrevChild(Child);
       if Node.Level > 1 then Items.Delete(NodeToDelete);
    end;
    if Node.Level > 1 then AddDummyChild(Node);
  finally
    EndUpdate;
  end;

    with Node do begin
       if ImageIndex = OpenFolderIconIndex then
            ImageIndex := FolderIconIndex;
       if not IsNetworkNode(Node) and  (Level = 2) then begin
          DiskFileNotify := PFileObject(Data)^.FileNotify;
          if (DiskFileNotify <> nil) and FNotification then
               DiskFileNotify.StopMonitoring;
       end;
    end;

    inherited;
end;

{**********************************************************
                CAN CHANGE
**********************************************************}
function TkpFolderTreeView.CanChange(Node: TTreeNode): Boolean;
type BoolFunc = function : Boolean of object;
var i: integer;
    Method: TMethod;
begin
   Result := False;
   for i:=0 to FViewList.Count-1 do begin
     Method.Data := FViewList[i];
     Method.Code := TObject(FViewList[i]).MethodAddress('IsRefreshing');
     if Assigned(Method.Code) and BoolFunc(Method) then Exit;
   end;
   Result := True;
end;

{**********************************************************
        FRESH LINKS
**********************************************************}
procedure TkpFolderTreeView.FreshLinks;
type SetDirProc = procedure ( NewDir: string ) of object;
var i: integer;
    Method: TMethod;
    DirName: string;
    Obj: PFileObject;
begin
   DirName := FDirectory;
   Obj := PFileObject(Selected.Data);
   if (Pos(SpecDirAlias, DirName) > 0) and Assigned(Obj) then
      DirName := DirName + SpecDirAlias + '$' +
                 IntToHex(integer(Obj^.NetResource),8);
   for i:=0 to FViewList.Count-1 do begin
     Method.Data := TObject(FViewList[i]);
     Method.Code := TObject(FViewList[i]).MethodAddress('SetDirectory');
     if Assigned(Method.Code) then SetDirProc(Method)(DirName);
   end;
end;

{**********************************************************
        CHANGE NOTIFY LINKS
**********************************************************}
procedure TkpFolderTreeView.ChangeNotifyLinks;
type ChangeProc = procedure (Sender :TObject;Action :TNotificationOption) of object;
var i: integer;
    Method: TMethod;
begin
   for i:=0 to FViewList.Count-1 do begin
     Method.Data := TObject(FViewList[i]);
     Method.Code := TObject(FViewList[i]).MethodAddress('ChangeFileSystem');
     if Assigned(Method.Code) then ChangeProc(Method)(Self,fnoFolder);
   end;
end;

{**********************************************************
                 ON TIMER
**********************************************************}
procedure TkpFolderTreeView.OnTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  if Selected <> nil then Change(Selected);
end;

{**********************************************************
                 CHANGE
**********************************************************}
procedure TkpFolderTreeView.Change(Node: TTreeNode);
begin
    if FTimer.Enabled then Exit;
    if Node.Text <> '' then begin // for the case of escaping when
                                  // creating a node  
       FDirectory := GetNodePath(Node);
       SetDirLabelCaption(FDirLabel, FDirectory, Width);
       if not FIsSettingPath and not IsDroppedDown then
          FreshLinks;
    end;
    inherited;
end;

{**********************************************************
                CAN EDIT
**********************************************************}
function TkpFolderTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
   Result := inherited CanEdit(Node);
   if Result then
      Result := FIsEditingNew or
                ((not ReadOnly) and (Pos('?', FDirectory) = 0) and
                 (Length(GetNodePath(Node)) > 3) ); // not a disk
end;

{**********************************************************
                ADD VIEW CHILD
**********************************************************}
procedure TkpFolderTreeView.AddViewChild(ViewObject: TComponent);
begin
  with FViewList do
     if IndexOf(ViewObject) < 0 then Add(Pointer(ViewObject));
end;

{**********************************************************
                REMOVE VIEW CHILD
**********************************************************}
procedure TkpFolderTreeView.RemoveViewChild(ViewObject: TComponent);
var Index: integer;
begin
    Index := FViewList.IndexOf(ViewObject);
    if Index >= 0 then FViewList.Delete(Index);
end;

{**********************************************************
                NOTIFICATION
**********************************************************}
procedure TkpFolderTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = FDirLabel then FDirLabel := nil;
    RemoveViewChild ( AComponent );
  end;
end;

{**********************************************************
                ALLOW CREATION
**********************************************************}
function  TkpFolderTreeView.AllowCreation: Boolean;
begin
    Result := False;
    if Selected <> nil then
        if IsNetworkNode(Selected) then
             Result := IsNetworkShare(Selected)
        else Result := Selected.Level > 1;
end;



{------------------------------------------------------
      CREATE
-------------------------------------------------------}
constructor TkpFolderCombo.Create(AOwner: TComponent);
begin
   inherited;
   FCanvas := TControlCanvas.Create;
   FCanvas.Control := self;

   Font.Size := 8;
   Font.Name := 'MS Sans Serif';
   Height := 21;
   ReadOnly := True;
   FAltColor := clBlue;

   FFreshDrives := False;
   FIndent := 19;
   FRightClickSelect := False;
   FShowButtons := True;
   FShowLines := True;
   Width := 150;

   FIsSettingParent := False;

   FBtn := TSpeedButton.Create(Self);
   with FBtn do begin
     Parent := Self;
     Top := 0;
     Width := 17;
     Height := 17;
     Cursor := crArrow;
     Down := False;
     OnClick := BtnClick;
     Glyph.Handle := LoadBitmap(0, PChar(OBM_COMBO));
     NumGlyphs := 1;
   end;

   FTree := TkpFolderTreeView.Create(Self);
   with FTree do begin
      Enabled := False;
      Visible := False;
      Parent := Self;
      Top := Self.Height;
      Height := 200;
      Width := Self.Width;
      DirLabel := Self.FDirLabel;
      NetBrowse := Self.FNetBrowse;
      AltColor := Self.FAltColor;
      Notify := Self.FNotify;
      RightClickSelect := Self.FRightClickSelect;
      ShowButtons := Self.FShowButtons;
      ShowLines := Self.FShowLines;
      FreshDrives := Self.FFreshDrives;
      SetDirectory(Self.FDirectory);
   end;

end;

{------------------------------------------------------
      CREATE PARAMS
-------------------------------------------------------}
procedure TkpFolderCombo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

{**********************************************************
                SET PARENT
**********************************************************}
procedure TkpFolderCombo.SetParent(AParent: TWinControl);
begin
   FIsSettingParent := True;
   if Assigned(FTree) then
      TkpFolderTreeView(FTree).FIsSettingParent := True;
   inherited;
   FIsSettingParent := False;
   if Assigned(FTree) then
      TkpFolderTreeView(FTree).FIsSettingParent := False;
end;

{------------------------------------------------------
      CREATE WND
-------------------------------------------------------}
procedure TkpFolderCombo.CreateWnd;
var Node: TTreeNode;
begin
   inherited CreateWnd;

   FTree.CreateWnd;
   FTree.Indent := Self.FIndent;

   if TTreeView(FTree).Items.Count > 0 then begin
      Node := FTree.Selected;
      if Node = nil then Node := TTreeView(FTree).Items[0];
      Self.ImageIndex := Node.ImageIndex;
      Self.Text := Node.Text;
      Self.FDirectory := FTree.FDirectory;
    end;
end;

{------------------------------------------------------
      DESTROY
-------------------------------------------------------}
destructor TkpFolderCombo.Destroy;
begin
   FCanvas.Free;
   inherited;
end;

{------------------------------------------------------
      SET BOUNDS
-------------------------------------------------------}
procedure TkpFolderCombo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var Loc: TRect;
begin
   if AHeight <> 21 then AHeight := 21;

   inherited;

   if Parent <> nil then begin
     if FBtn <> nil then begin
        FBtn.Left := Self.Width - FBtn.Width - 4;
        FBtn.Height := Self.Height - 4;
        SetRect(Loc, 23, 0, ClientWidth - FBtn.Width - 2, ClientHeight + 1);
        Perform(EM_SETRECTNP, 0, LongInt(@Loc));
     end;
     if FTree <> nil then begin
        FTree.Top := Self.Top + Self.Height;
        FTree.Left := Self.Left;
        FTree.Width := Self.Width;
     end;
  end;

end;

{**********************************************************
         END BROWSE
**********************************************************}
procedure TkpFolderCombo.EndBrowse;
var DiskFound: Boolean;
    NewDir, Info: string;
begin
   Info := SNoAccessToDrive +  ' ' + FDirectory+#13+#13+ SDriveNotReady;
   NewDir := FTree.FDirectory;
   if (FTree.Selected.Level = 1) and
         (GetDriveType(PChar(NewDir)) in [DRIVE_REMOVABLE,DRIVE_CDROM]) then
         while True do begin
            DiskFound := DiskInDrive(NewDir[1]);
            if DiskFound then break;

            FTree.Enabled := False;
            FTree.Visible := False;
            if (IDCANCEL = Windows.MessageBox(0, PChar(Info),
                  PChar(SErrorMsgCaption), MB_ICONERROR  or MB_RETRYCANCEL )) then begin
               CancelBrowse;
               Exit;
            end;
         end;
   ImageIndex := FTree.Selected.ImageIndex;
   Text := FTree.Selected.Text;
   FTree.Enabled := False;
   FTree.Visible := False;
   FTree.FreshLinks;
end;

{**********************************************************
         KEY DOWN
**********************************************************}
procedure TkpFolderCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if Key = VK_F4 then begin
     BtnClick(Self);
     Key := 0;
  end;

  if Key = VK_RETURN then EndBrowse;

  if FTree.Visible then begin
     if Key = VK_ESCAPE then begin
        FTree.Enabled := False;
        FTree.Visible := False;
        CancelBrowse;
        Key := 0;
     end
     else FTree.KeyDown(Key, Shift);
  end;

end;

{**********************************************************
        REFRESH
**********************************************************}
procedure TkpFolderCombo.Refresh;
var SelTreeNode: TTreeNode;
    Dir: string;
begin
   if Assigned(FTree) then begin
      SelTreeNode := FTree.Selected;
      if SelTreeNode <> nil then begin
         Dir := FTree.Directory;
         SelTreeNode.Parent.Collapse(True);
         FTree.Refresh;
         FTree.ExpandPath(Dir, True);
      end;
      {
      SelTreeNode := FTree.Selected;
      if SelTreeNode <> nil then begin
         SelTreeNode.Collapse(True);
         SelTreeNode.Expand(False);
         FTree.Selected := SelTreeNode;
      end;
      }
   end;
end;

{**********************************************************
                NOTIFICATION
**********************************************************}
procedure TkpFolderCombo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = FDirLabel then FDirLabel := nil;
    if AComponent = FTree then FTree := nil;
  end;
end;

{------------------------------------------------------
      WM PAINT
-------------------------------------------------------}
procedure TkpFolderCombo.WMPaint(var Msg: TWMPaint);
var YPos: integer;
    SaveColor: TColor;
    SaveOnChange: TNotifyEvent;
begin
  SaveColor := Font.Color;
  SaveOnChange := Font.OnChange;
  Font.OnChange := nil;
  if Assigned(FTree) and (FTree.Selected <> nil) and
     FTree.IsAltColor(FTree.Selected) then Font.Color := FAltColor;
  inherited PaintHandler(Msg);
  Font.Color := SaveColor;
  Font.OnChange := SaveOnChange;

  YPos := ClientHeight div 2 - 8;
  TTreeView(FTree).Images.Draw(FCanvas, 5, YPos, ImageIndex);
  HideCaret(Handle);
end;

{------------------------------------------------------
      BTN CLICK
-------------------------------------------------------}
procedure TkpFolderCombo.BtnClick(Sender: TObject);
var ScrPos: TPoint;
begin
   ScrPos := Point(Left, Top+Height);
   ScrPos := Parent.ClientToScreen(ScrPos);
   with FTree do
     if not Enabled then begin
        Left := ScrPos.X;
        Top := ScrPos.Y;
        with TTreeView(FTree).Items[0] do
           if not Expanded then Expand(False);
        if FTree.Selected = nil then
           TTreeView(FTree).Items[0].Selected := True;
        Enabled := True;
        Visible := True;
        Self.SetFocus;
        SetCapture(FTree.Handle);
     end;
end;

{------------------------------------------------------
      GET TEXT
-------------------------------------------------------}
function TkpFolderCombo.GetText: string;
begin
  Result := inherited Text;
end;

{------------------------------------------------------
      SET TEXT
-------------------------------------------------------}
procedure TkpFolderCombo.SetText(Value: string);
begin
   inherited Text := Value;
   FDirectory := FTree.FDirectory;
end;

{------------------------------------------------------
      CANCEL BROWSE
-------------------------------------------------------}
procedure TkpFolderCombo.CancelBrowse;
begin
  FTree.SetDirectory(FDirectory);   
end;

{**********************************************************
                GET DIRECTORY
**********************************************************}
function TkpFolderCombo.GetDirectory: string;
begin
  Result := PureDirectory(FDirectory);
end;

{------------------------------------------------------
      SET DIRECTORY
-------------------------------------------------------}
procedure TkpFolderCombo.SetDirectory(const Value: string);
begin
  if Assigned(FTree) then begin
     FTree.SetDirectory(Value);
     EndBrowse;
  end
  else FDirectory := Value;
end;

{**********************************************************
                IS SPECIAL FOLDER
**********************************************************}
function TkpFolderCombo.IsSpecialFolder: Boolean;
begin
   Result := (Pos(SpecDirAlias,FDirectory) > 0);
end;

{**********************************************************
         SET DIR LABEL
**********************************************************}
procedure TkpFolderCombo.SetDirLabel (Value: TLabel);
begin
  FDirLabel := Value;
  if Assigned(FTree) then FTree.DirLabel := Value;
end;

{**********************************************************
         SET NET BROWSE
**********************************************************}
procedure TkpFolderCombo.SetNetBrowse (Value: Boolean);
var OldDir: string;
begin
  FNetBrowse := Value;
  if Assigned(FTree) then begin
     OldDir := FTree.FDirectory;
     FTree.NetBrowse := Value;
     if OldDir <> FTree.FDirectory then
        FTree.FreshLinks;
     EndBrowse;
  end;
end;

{**********************************************************
         SET ALT COLOR
**********************************************************}
procedure TkpFolderCombo.SetAltColor(const Value: TColor);
begin
  FAltColor := Value;
  if Assigned(FTree) then FTree.AltColor := Value;
  if Visible then Invalidate;
end;

{**********************************************************
         SET NOTIFY
**********************************************************}
procedure TkpFolderCombo.SetNotification (Value: Boolean);
begin
  FNotify := Value;
  if Assigned(FTree) then FTree.Notify := Value;
end;

{**********************************************************
         SET RIGHT CLICK SELECT
**********************************************************}
procedure TkpFolderCombo.SetRightClickSelect(Value: Boolean);
begin
  FRightClickSelect := Value;
  if Assigned(FTree) then FTree.RightClickSelect := Value;
end;

{**********************************************************
         SET SHOW BUTTONS
**********************************************************}
procedure TkpFolderCombo.SetShowButtons(Value: Boolean);
begin
  FShowButtons := Value;
  if Assigned(FTree) then FTree.ShowButtons := Value;
end;

{**********************************************************
         SET SHOW LINES
**********************************************************}
procedure TkpFolderCombo.SetShowLines(Value: Boolean);
begin
  FShowLines := Value;
  if Assigned(FTree) then FTree.ShowLines := Value;
end;

{**********************************************************
         SET FLOPPY SEEK
**********************************************************}
procedure TkpFolderCombo.SetFreshDrives(Value: Boolean);
begin
  FFreshDrives := Value;
  if Assigned(FTree) then FTree.FreshDrives := Value;
end;

{**********************************************************
         SET INDENT
**********************************************************}
procedure TkpFolderCombo.SetIndent(Value: Integer);
begin
  FIndent := Value;
  if Assigned(FTree) then FTree.Indent := Value;
end;

{------------------------------------------------------
      HIDE CARET
-------------------------------------------------------}
procedure TkpFolderCombo.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  HideCaret(Handle);
end;

procedure TkpFolderCombo.WMNCHitTest(var Msg: TMessage);
begin
  inherited;
  HideCaret(Handle);
end;

procedure TkpFolderCombo.WMKeyDown(var Msg: TMessage);
begin
  inherited;
  HideCaret(Handle);
end;

{**********************************************************
                ONE LEVEL UP
**********************************************************}
procedure TkpFolderCombo.OneLevelUp;
begin
  FTree.OneLevelUp;
  EndBrowse;
end;


{**********************************************************
                FILE NOTIFICATION BLOCK
**********************************************************}
{
  Fnugry File Notify Component (slightly changed)
  Copyright (C) 1997 Gleb Yourchenko
  Version 1.0.1.1

  Contact:
  Gleb Yourchenko
  E-mail: eip__@hotmail.com
}
{ TFileNotify }

procedure TFileNotify.Prepare;
begin
        FThread := TNotificationThread.Create(Self);
end;

procedure TFileNotify.StartMonitoring;
begin
        with FThread do
        if Suspended then Resume;
end;

procedure TFileNotify.StopMonitoring;
begin
        with FThread do
        if not Suspended then Suspend;
end;

constructor TFileNotify.Create(
                AOwner :TCOmponent; AOptions :TNotificationOptions );
begin
  inherited Create;
  FOptions := AOptions; // [fnoFile, fnoFolder];
end;

destructor TFileNotify.Destroy;
begin
        if FThread <> Nil then
      with FThread do begin
        if Suspended then Resume;
        Free;
      end;
        inherited Destroy;
end;

procedure TFileNotify.Notify(Action :TNotificationOption);
begin
  if assigned(FOnNotify) then FOnNotify(Self, Action);
end;

procedure TFileNotify.SetOptions(const Value :TNotificationOptions);
begin
  if Value <> Options then begin
      FOptions := Value;
      if FThread <> Nil then FThread.Reset;
  end;
end;

procedure TFileNotify.SetRootFolder(const Value :String);
begin
  if Value <> FFolder then begin
      FFolder := Value;
      if FFolder[Length(FFolder)] = '\' then
          SetLength(FFolder, Length(FFolder)-1 );
      if FThread <> Nil then FThread.Reset;
  end;
end;

procedure TFileNotify.SetAsyncNotify(Value :Boolean);
begin
  FAsyncNotify := Value;
end;

{ TNotificationThread }

const

  HANDLE_FILE         = 0;
  HANDLE_DIR          = 1;
  HANDLE_ATTR         = 2;
  HANDLE_SIZE         = 3;
  HANDLE_TIME         = 4;

procedure TNotificationThread.Execute;
var R, I :Integer;
    CheckAgain: Boolean; // K. Polyakov
begin
  while not Terminated do
    if FHandleCount > 0 then begin
        R := WaitForMultipleObjects(FHandleCount, @FHandles, false, 200);
        repeat                                      // K. Polyakov
          CheckAgain := False;                      // K. Polyakov
          for I := 0 to FHandleCount-1 do
            if R = (WAIT_OBJECT_0 + I) then begin
               FActiveFilter := FHandleFilters[I];
               if FOwner.FAsyncNotify then Notify
               else Synchronize(Notify);
               repeat                                            // K. Polyakov
                 FindNextChangeNotification(FHandles[I]);        // K. Polyakov
                 R := WaitForMultipleObjects(FHandleCount, @FHandles, false, 0); // K. Polyakov
               until R <> (WAIT_OBJECT_0 + I);                   // K. Polyakov
               CheckAgain := True;                               // K. Polyakov
               break;
             end;
        until not CheckAgain;                                    // K. Polyakov
      end
    else sleep(0);
end;


procedure TNotificationThread.Notify;
begin
  FOwner.Notify(FActiveFilter);
end;

constructor TNotificationThread.Create(Owner :TFileNotify);
begin
  inherited Create(true);
  FOwner := Owner;
  FreeOnTerminate := False;
  Priority := tpLowest;
  Reset;
  Resume;
end;

procedure TNotificationThread.ReleaseHandle;
var I :Integer;
begin
  for I := 0 to FHandleCount-1 do
    if FHandles[I] <> 0 then FindCloseChangeNotification(FHandles[I]);
  fillchar(FHandles, SizeOf(FHandles), 0);
  fillchar(FHandleFilters, Sizeof(FHandleFilters), 0);
  FHandleCount := 0;
end;

destructor TNotificationThread.Destroy;
begin
  ReleaseHandle;
  inherited destroy;
end;

procedure TNotificationThread.AllocateHandle;

  function DoAllocate(Filter :Integer):THandle;
  begin
    if fnoWatchSubTree in FOwner.FOptions then
       result := FindFirstChangeNotification(
         PChar(FOwner.Folder), Bool(1), Filter)
    else
       result := FindFirstChangeNotification(
         PChar(FOwner.Folder), Bool(0), Filter);
    if result = INVALID_HANDLE_VALUE then
      raise EFileNotificationError.Create(SErrAllocHandle);
  end;

begin
  if (FOwner <> Nil) and (FOwner.FOptions <> []) then
    try
      fillchar(FHandles, SizeOf(FHandles), 0);
      FHandleCount := 0;
      if fnoFile in FOwner.FOptions then
        begin
          FHandles[FHandleCount] := DoAllocate(FILE_NOTIFY_CHANGE_FILE_NAME);
          FHandleFilters[FHandleCount] := fnoFile;
          inc(FHandleCount);
        end;
      if fnoFolder in FOwner.FOptions then
        begin
          FHandles[FHandleCount] := DoAllocate(FILE_NOTIFY_CHANGE_DIR_NAME);
          FHandleFilters[FHandleCount] := fnoFolder;
          inc(FHandleCount);
        end;
      if fnoSize in FOwner.FOptions then
        begin
          FHandles[FHandleCount] := DoAllocate(FILE_NOTIFY_CHANGE_SIZE);
          FHandleFilters[FHandleCount] := fnoSize;
          inc(FHandleCount);
        end;
      if fnoTime in FOwner.FOptions then
        begin
          FHandles[FHandleCount] := DoAllocate(FILE_NOTIFY_CHANGE_LAST_WRITE);
          FHandleFilters[FHandleCount] := fnoTime;
          inc(FHandleCount);
        end;
      if fnoAttr in FOwner.FOptions then
        begin
          FHandles[FHandleCount] := DoAllocate(FILE_NOTIFY_CHANGE_ATTRIBUTES);
          FHandleFilters[FHandleCount] := fnoAttr;
          inc(FHandleCount);
        end;
    except
      ReleaseHandle;
//      raise;
    end;
end;

procedure TNotificationThread.Reset;
begin
  ReleaseHandle;
  AllocateHandle;
end;

{**********************************************************
                REGISTER
**********************************************************}
procedure Register;
begin
  RegisterComponents('KP', [TkpFolderTreeView,TkpFileListBox,
                            TkpFileListView,TkpFolderCombo]);
end;

{**********************************************************
                INIT IMAGE LISTS
**********************************************************}
procedure InitImageLists;
var Fileinfo: TSHFileInfo;
    FindHandle: THandle;
    FindData: TWin32FindData;
    Path: string;
    {----------- ADD ICON TO LIST ------------------}
    procedure AddIconToList(Name: string; var IconIndex: integer);
    var Icon: TIcon;
    begin
        Icon := TIcon.Create;
      try
        Icon.Handle := LoadIcon(hInstance,PChar(Name));
        if Icon.Handle<>0 then begin
           kpSmallImageList.AddIcon(Icon);
           IconIndex := kpSmallImageList.Count-1;
        end;
        Name := Name + 'L';
        if kpLargeImageList.Width > 32 then begin
           Name := Name + 'X';
//           Icon.Handle := LoadImage(hInstance, PChar(Name), IMAGE_ICON,
//                                    48, 48, 0);
           Icon.Handle := LoadIcon(hInstance,PChar(Name));
        end
        else
           Icon.Handle := LoadIcon(hInstance,PChar(Name));
        if Icon.Handle<>0 then
           kpLargeImageList.AddIcon(Icon);
      except end;
        Icon.Free;
    end;
    {----------- ADD NETWORK ICON ------------------}
    procedure AddNetworkIcons;
    begin
      kpSmallImageList.Assign(SysSmallImageList);
      kpLargeImageList.Assign(SysLargeImageList);
      AddIconToList('ZZZ_NETICON',NetworkIconIndex);
      FirstExtraIndex := NetworkIconIndex;
      AddIconToList('ZZZ_DOMNICON',DomainIconIndex);
      AddIconToList('ZZZ_SERVICON',ServerIconIndex);
      AddIconToList('ZZZ_SERVNOICON',ServerNoAccessIconIndex);
      AddIconToList('ZZZ_DISK35', Disk35IconIndex);
      AddIconToList('ZZZ_DISK525', Disk525IconIndex);
      LastExtraIndex := Disk525IconIndex;
//      LastExtraIndex := ServerNoAccessIconIndex;
    end;

{**********************************************************}
begin
    FolderIconIndex := - 1;
    OpenFolderIconIndex := - 1;
    ServerIconIndex := - 1;
    ServerNoAccessIconIndex := - 1;
    NethoodIconIndex := - 1;
    NetworkIconIndex := - 1;
    DomainIconIndex := - 1;

                // Получить список системных маленьких и больших иконок
    SysSmallImageList := TImageList.Create(nil);
    with SysSmallImageList do begin
        Handle := SHGetFileInfo('', 0, Fileinfo, sizeof(TSHFileInfo),
                                SHGFI_SMALLICON or SHGFI_SYSICONINDEX );
        ShareImages := True;
    end;
    SysLargeImageList := TImageList.Create(nil);
    with SysLargeImageList do begin
        Handle := SHGetFileInfo('', 0, Fileinfo, sizeof(TSHFileInfo),
                                SHGFI_LARGEICON or SHGFI_SYSICONINDEX );
        ShareImages := True;
    end;

                // "Рабочий стол"
    DeskTopIconIndex := GetPIDLIconIndex(CSIDL_DESKTOP);
    DesktopName := GetPIDLDisplayName(CSIDL_DESKTOP);

                // "Мой компьютер"
    MyComputerIconIndex := GetPIDLIconIndex(CSIDL_DRIVES);
    MyComputerName := GetPIDLDisplayName(CSIDL_DRIVES);

                // "Сетевое окружение"
    NethoodIconIndex := GetPIDLIconIndex(CSIDL_NETWORK);
    NethoodName := GetPIDLDisplayName(CSIDL_NETWORK);

                // найти первую папку на диске 'C:'
                // и получить номера иконок для нее
    FindHandle := FindFirstFile(PChar('C:\*.*'), FindData);
    while FindHandle <> INVALID_HANDLE_VALUE do begin
       if (FILE_ATTRIBUTE_DIRECTORY and FindData.dwFileAttributes) <> 0 then begin
         Path := 'C:\' + FindData.cFileName;
         FolderIconIndex := GetFileSysIconIndex(Path, 0);
         OpenFolderIconIndex := GetFileSysIconIndex(Path, SHGFI_OPENICON);
         Windows.FindClose(FindHandle);
         break;
       end;
       if not FindNextFile(FindHandle, FindData) then begin
         Windows.FindClose(FindHandle);
         break;
       end;
    end;

    FolderDescription := StrFileDescription('Directory');

    kpSmallImageList := TkpImageList.Create(nil);
    kpLargeImageList := TkpImageList.Create(nil);

    AddNetworkIcons;
    
    ExtIcons := TStringList.Create;

end;

{**********************************************************
                INITIALIZATION
**********************************************************}
initialization
    InitImageLists;

{**********************************************************
                FINALIZATION
**********************************************************}
finalization
    SysSmallImageList.Free;
    SysLargeImageList.Free;
    kpSmallImageList.Free;
    kpLargeImageList.Free;
    ExtIcons.Free;
end.
