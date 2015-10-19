unit mpBackendForm;

interface

uses
  // delphi units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, ComCtrls, XPMan, StdCtrls, ImgList, CommCtrl,
  Menus, Grids, ValEdit, ShellAPI, Clipbrd, StrUtils,
  // indy components
  IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer,
  IdGlobal, IdSync,
  // mte components
  W7Taskbar, RttiJson, mteLogger, mteTracker, mteHelpers, mteTaskHandler,
  // mp units
  mpBackend, mpDictionaryForm, mpOptionsForm, mpEditForm;

type
  TBackendForm = class(TForm)
    // GENERIC
    XPManifest: TXPManifest;
    IconList: TImageList;
    TaskTimer: TTimer;
    TCPServer: TIdTCPServer;
    // QUICKBAR
    QuickBar: TPanel;
    ApproveButton: TSpeedButton;
    BuildButton: TSpeedButton;
    DictionaryButton: TSpeedButton;
    OptionsButton: TSpeedButton;
    UpdateButton: TSpeedButton;
    HelpButton: TSpeedButton;
    // MAIN PANEL
    MainPanel: TPanel;
    Splitter: TSplitter;
    DetailsPanel: TPanel;
    // PAGE CONTROL - UNAPPROVED/APPROVED/LOG
    PageControl: TPageControl;
    UnapprovedTabSheet: TTabSheet;
    ApprovedTabSheet: TTabSheet;
    LogTabSheet: TTabSheet;
    UnapprovedListView: TListView;
    ApprovedListView: TListView;
    LogListView: TListView;
    // DETAILS EDITOR
    DetailsLabel: TLabel;
    DetailsEditor: TValueListEditor;
    // UNAPPROVED POPUP MENU
    UnapprovedPopupMenu: TPopupMenu;
    ApproveReportItem: TMenuItem;
    DeleteReportItem: TMenuItem;
    // APPROVED POPUP MENU
    ApprovedPopupMenu: TPopupMenu;
    UnapproveReportItem: TMenuItem;
    EditReportItem: TMenuItem;
    ViewDictionaryEntryItem: TMenuItem;
    // LOG POPUP MENU
    LogPopupMenu: TPopupMenu;
    CopyToClipboardItem: TMenuItem;
    SaveAndClearItem: TMenuItem;
    FilterLabelItem: TMenuItem;
    FilterGroupItem: TMenuItem;
    // REBUILD POPUP MENU
    RebuildPopupMenu: TPopupMenu;
    ForceRebuildItem: TMenuItem;
    // STATUS PANEL
    StatusPanel: TPanel;
    StatusPanelMessage: TPanel;
    StatusPanelClients: TPanel;
    StatusPanelBackend: TPanel;
    StatusPanelFrontend: TPanel;
    StatusPanelReports: TPanel;
    StatusPanelUptime: TPanel;
    ToggleAutoScrollItem: TMenuItem;

    // MERGE FORM EVENTS
    procedure LogMessage(const group, &label, text: string);
    procedure UpdateStatusPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RefreshGUI;
    procedure OnTaskTimer(Sender: TObject);
    // DETAILS EDITOR EVENTS
    function AddDetailsItem(name, value: string): TItemProp;
    procedure AddDetailsList(name: string; sl: TStringList);
    procedure PageControlChange(Sender: TObject);
    procedure UpdateUnapprovedDetails;
    procedure UpdateApprovedDetails;
    procedure UpdateApplicationDetails;
    procedure DetailsEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // LIST VIEW EVENTS
    procedure UnapprovedListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure UnapprovedListViewColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure UnapprovedListViewData(Sender: TObject; Item: TListItem);
    procedure UnapprovedListViewDrawItem(Sender: TCustomListView;
      Item: TListItem; Rect: TRect; State: TOwnerDrawState);
    procedure ApprovedListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ApprovedListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ApprovedListViewData(Sender: TObject; Item: TListItem);
    procedure ApprovedListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure LogListViewData(Sender: TObject; Item: TListItem);
    procedure LogListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    // REBUILD POPUP MENU EVENTS
    procedure ForceRebuildItemClick(Sender: TObject);
    // APPROVED POPUP MENU EVENTS
    procedure ApprovedPopupMenuPopup(Sender: TObject);
    procedure EditReportItemClick(Sender: TObject);
    procedure UnapproveReportItemClick(Sender: TObject);
    procedure ViewDictionaryEntryItemClick(Sender: TObject);
    // UNAPPROVED POPUP MENU EVENTS
    procedure UnapprovedPopupMenuPopup(Sender: TObject);
    procedure DeleteReportItemClick(Sender: TObject);
    procedure ApproveReportItemClick(Sender: TObject);
    // LOG POPUP MENU EVENTS
    procedure LogPopupMenuPopup(Sender: TObject);
    procedure ToggleGroupFilter(Sender: TObject);
    procedure ToggleLabelFilter(Sender: TObject);
    procedure CopyToClipboardItemClick(Sender: TObject);
    procedure SaveAndClearLog;
    procedure SaveAndClearItemClick(Sender: TObject);
    procedure ToggleAutoScrollItemClick(Sender: TObject);
    // QUICKBAR BUTTON EVENTS
    procedure ApproveButtonClick(Sender: TObject);
    procedure RebuildButtonClick(Sender: TObject);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    // SERVER EVENTS
    procedure TCPServerConnect(AContext: TIdContext);
    procedure TCPServerDisconnect(AContext: TIdContext);
    procedure TCPServerException(AContext: TIdContext; AException: Exception);
    procedure SendResponse(var user: TUser; var AContext: TIdContext;
      id: integer; data: string; bLog: boolean = true);
    procedure HandleMessage(msg: TmpMessage; size: integer; AContext: TIdContext);
    procedure WriteMessage(msg: TmpMessage; ip: string);
    procedure TCPServerExecute(AContext: TIdContext);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BackendForm: TBackendForm;
  LastHint: string;
  bAutoScroll: boolean;
  LastURLTime, LastDailyTaskTime, LastRegTaskTime: double;
  TaskHandler: TTaskHandler;

implementation

{$R *.dfm}


{******************************************************************************}
{ Backend Form Events
  Events for the Backend Form.
  - LogMessage
  - ProgressMessage
  - FormCreate
  - FormShow
  - LoaderDone
  - FormClose
}
{******************************************************************************}

{ Prints a message to the log }
procedure TBackendForm.LogMessage(const group, &label, text: string);
var
  msg: TLogMessage;
begin
  msg := TLogMessage.Create(FormatDateTime('hh:nn:ss', Now), group, &label, text);
  BaseLog.Add(msg);
  if MessageEnabled(msg) then begin
    Log.Add(msg);
    LogListView.Items.Count := Log.Count;
    if bAutoScroll then begin
      LogListView.ClearSelection;
      LogListView.Items[Pred(LogListView.Items.Count)].MakeVisible(false);
      SendMessage(LogListView.Handle, WM_VSCROLL, SB_LINEDOWN, 0);
    end;
    CorrectListViewWidth(LogListView);
  end;
end;

procedure TBackendForm.UpdateStatusPanel;
var
  sessionUptime: TDateTime;
begin
  sessionUptime := GetSessionUptime;
  StatusPanelClients.Caption := 'Connected clients: '+IntToStr(slConnectedIPs.Count);
  StatusPanelReports.Caption := 'Pending reports: '+IntToStr(UnapprovedListView.Items.Count);
  StatusPanelUptime.Caption := 'Uptime: '+TimeStr(sessionUptime);
  StatusPanelFrontend.Caption := 'Frontend v'+status.programVersion;
  StatusPanelBackend.Caption := 'Backend v'+ProgramVersion;
end;

{ Initialize form, initialize TES5Edit API, and load plugins }
procedure TBackendForm.FormCreate(Sender: TObject);
begin
  InitializeTaskbarAPI;
  SetTaskbarProgressState(tbpsIndeterminate);
  try
    // INITIALIZE VARIABLES
    ProgramVersion := GetVersionMem;
    InitLog;
    bAutoScroll := true;
    wbStartTime := Now;
    LastDailyTaskTime := Trunc(Now);
    Logger.OnLogEvent := LogMessage;
    ProgramPath := ExtractFilePath(ParamStr(0));
    LogPath := ProgramPath + 'logs\';
    ForceDirectories(LogPath);
    status := TmpStatus.Create;
    status.Refresh;
    slConnectedIPs := TStringList.Create;

    // GUI ICONS
    Tracker.Write('Loading Icons');
    ApproveButton.Flat := true;
    BuildButton.Flat := true;
    DictionaryButton.Flat := true;
    OptionsButton.Flat := true;
    UpdateButton.Flat := true;
    HelpButton.Flat := true;
    IconList.GetBitmap(0, ApproveButton.Glyph);
    IconList.GetBitmap(1, BuildButton.Glyph);
    IconList.GetBitmap(2, DictionaryButton.Glyph);
    IconList.GetBitmap(3, OptionsButton.Glyph);
    IconList.GetBitmap(4, UpdateButton.Glyph);
    IconList.GetBitmap(5, HelpButton.Glyph);

    // CREATE AND LOAD DICTIONARIES
    TES5Dictionary := TList.Create;
    TES4Dictionary := TList.Create;
    FO3Dictionary := TList.Create;
    FNVDictionary := TList.Create;
    LoadDictionary(TES5Dictionary, slTES5Dictionary, 'TES5Dictionary.txt');
    LoadDictionary(TES4Dictionary, slTES4Dictionary, 'TES4Dictionary.txt');
    LoadDictionary(FO3Dictionary, slFO3Dictionary, 'FO3Dictionary.txt');
    LoadDictionary(FNVDictionary, slFNVDictionary, 'FNVDictionary.txt');

    // QUERY DATA FROM MYSQL
    DBQueryReports;
    DBQueryUsers;
    DBQueryBlacklist;

    // PREPARE LIST VIEWS
    LogListView.OwnerDraw := not settings.simpleLogView;
    LogListView.ShowColumnHeaders := settings.simpleLogView;
    ShowScrollBar(LogListView.Handle, SB_BOTH, true);
    UnapprovedListView.OwnerDraw := not settings.simpleReportsView;
    ApprovedListView.OwnerDraw := not settings.simpleReportsView;
    UnapprovedListView.Items.Count := UnapprovedReports.Count;
    ApprovedListView.Items.Count := ApprovedReports.Count;
  except
    on x: Exception do
      LogMessage('ERROR', 'Init', x.Message);
  end;
end;

procedure TBackendForm.FormShow(Sender: TObject);
begin
  // Correct list view widths
  PageControl.ActivePage := UnapprovedTabSheet;
  CorrectListViewWidth(UnapprovedListView);
  PageControl.ActivePage := ApprovedTabSheet;
  CorrectListViewWidth(ApprovedListView);
  PageControl.ActivePage := LogTabSheet;

  // Refresh GUI
  RefreshGUI;

  // initialize task handler
  TaskHandler := TTaskHandler.Create;
  TaskHandler.AddTask(TTask.Create('SQL Heartbeat', 10.0 * minutes, TTaskProcedures.SQLHeartbeat));
  TaskHandler.AddTask(TTask.Create('Refresh Blacklist', 1.0 * days, TTaskProcedures.RefreshBlacklist));
  TaskHandler.AddTask(TTask.Create('Rebuild Dictionaries', 1.0 * days, TTaskProcedures.RebuildDictionaries));
  TaskHandler.AddTask(TTask.Create('Refresh GUI', 3.0 * seconds, RefreshGUI));
  TaskHandler.AddTask(TTask.Create('Save and Clear Log', 10.0 * minutes, SaveAndClearLog));
end;

procedure TBackendForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  n: Integer;
  AContext: TIdContext;
begin
  // close all active connections
  Logger.Write('SERVER', 'Exit', 'Closing all active connections...');
  if TCPServer.Contexts <> nil then begin
    with TCPServer.Contexts.LockList do try
      for n := Pred(Count) downto 0 do begin
        AContext := TIdContext(Items[n]);
        if AContext.Connection.Connected then begin
          AContext.Connection.IOHandler.CloseGracefully;
          AContext.Connection.Disconnect;
        end;
      end;
    finally
      TCPServer.Contexts.UnlockList;
    end;
  end;

  // update statistics
  statistics.totalUptime := statistics.totalUptime + GetSessionUptime;
  statistics.totalBandwidth := statistics.totalBandwidth + sessionBandwidth;

  // save statistics, settings
  SaveStatistics;
  SaveSettings;

  // save log
  SaveLog(Log);
end;

procedure TBackendForm.RefreshGUI;
begin
  UpdateStatusPanel;
  if PageControl.ActivePageIndex = 2 then
    UpdateApplicationDetails;
end;

procedure TBackendForm.OnTaskTimer(Sender: TObject);
begin
  TaskHandler.ExecTasks;
end;


{******************************************************************************}
{ Details Editor Events
  Methods for helping with the DetailsEditor control.  Methods include:
  - AddDetailsItem
  - AddDetailsList
  - PageControlChange
  - UpdateApplicationDetails
}
{******************************************************************************}

{
   Adds a ListItem to DetailsView with @name and @value
}
function TBackendForm.AddDetailsItem(name, value: string): TItemProp;
var
  prop: TItemProp;
begin
  DetailsEditor.InsertRow(name, value, true);
  prop := DetailsEditor.ItemProps[DetailsEditor.RowCount - 1];
  prop.ReadOnly := true;
  Result := prop;
end;

{
  Add one or more ListItem to DetailsView with @name and the values
  in @sl
}
procedure TBackendForm.AddDetailsList(name: string; sl: TStringList);
var
  i: integer;
begin
  if sl.Count > 0 then begin
    AddDetailsItem(name, sl[0]);
    for i := 1 to Pred(sl.Count) do
      AddDetailsItem(' ', sl[i]);
  end
  else
    AddDetailsItem(name, ' ');
end;

{
  Switch details view when page control is changed
}
procedure TBackendForm.PageControlChange(Sender: TObject);
var
  ndx: integer;
begin
  ndx := TPageControl(Sender).ActivePageIndex;
  case ndx of
    0: begin
      UpdateUnapprovedDetails;
      CorrectListViewWidth(UnapprovedListView);
    end;
    1: begin
      UpdateApprovedDetails;
      CorrectListViewWidth(ApprovedListView);
    end;
    2: begin
      UpdateApplicationDetails;
      CorrectListViewWidth(LogListView);
    end;
  end;
end;

procedure TBackendForm.UpdateUnapprovedDetails;
var
  index: integer;
  report: TReport;
  sl: TStringList;
begin
  // don't do anything if no item selected
  if not Assigned(UnapprovedListView.Selected) then
    exit;

  // prepare list view for report information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := 'Report Details';

  // get report information
  index := UnapprovedListView.ItemIndex;
  report := TReport(UnapprovedReports[index]);

  // add details items
  AddDetailsItem('Game', report.game);
  AddDetailsItem('Username', report.username);
  AddDetailsItem('Filename', report.filename);
  AddDetailsItem('Hash', report.hash);
  AddDetailsItem('Record count', IntToStr(report.recordCount));
  AddDetailsItem('Rating', IntToStr(report.rating));
  AddDetailsItem('Merge version', report.mergeVersion);
  sl := TStringList.Create;
  sl.Text := report.notes;
  AddDetailsList('Notes', sl);
  sl.Free;
  AddDetailsItem('Date submitted', DateToStr(report.dateSubmitted));
end;

procedure TBackendForm.UpdateApprovedDetails;
var
  index: integer;
  report: TReport;
  sl: TStringList;
begin
  // don't do anything if no item selected
  if not Assigned(ApprovedListView.Selected) then
    exit;

  // prepare list view for report information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := 'Report Details';

  // get report information
  index := ApprovedListView.ItemIndex;
  report := TReport(ApprovedReports[index]);

  // add details items
  AddDetailsItem('Game', report.game);
  AddDetailsItem('Username', report.username);
  AddDetailsItem('Filename', report.filename);
  AddDetailsItem('Hash', report.hash);
  AddDetailsItem('Record count', IntToStr(report.recordCount));
  AddDetailsItem('Rating', IntToStr(report.rating));
  AddDetailsItem('Merge version', report.mergeVersion);
  sl := TStringList.Create;
  sl.Text := report.notes;
  AddDetailsList('Notes', sl);
  sl.Free;
  AddDetailsItem('Date submitted', DateToStr(report.dateSubmitted));
end;

procedure TBackendForm.UpdateApplicationDetails;
var
  sessionUptime: TDateTime;
begin
  // prepare list view for application information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := 'Application Details';

  // get current uptime
  sessionUptime := GetSessionUptime;

  // add details items
  AddDetailsItem('Application', 'Merge Plugins Backend');
  AddDetailsItem('Author', 'matortheeternal');
  AddDetailsItem('Version', ProgramVersion);
  AddDetailsItem('Date built', DateTimeToStr(GetLastModified(ParamStr(0))));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Times run', IntToStr(statistics.timesRun));
  AddDetailsItem('Unique users', IntToStr(Users.Count));
  AddDetailsItem('Blacklist size', IntToStr(Blacklist.Count));
  AddDetailsItem('Dictionary updates', IntToStr(statistics.dictionaryUpdates));
  AddDetailsItem('Program updates', IntToStr(statistics.programUpdates));
  AddDetailsItem('Reports recieved', IntToStr(statistics.reportsRecieved));
  AddDetailsItem('Reports approved', IntToStr(statistics.reportsApproved));
  AddDetailsItem('Reports denied', IntToStr(statistics.reportsDenied));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('TES5 logins', IntToStr(statistics.tes5Logins));
  AddDetailsItem('TES4 logins', IntToStr(statistics.tes4Logins));
  AddDetailsItem('FNV logins', IntToStr(statistics.fnvLogins));
  AddDetailsItem('FO3 logins', IntToStr(statistics.fo3Logins));
  AddDetailsItem('TES5 reports recieved', IntToStr(statistics.tes5Reports));
  AddDetailsItem('TES4 reports recieved', IntToStr(statistics.tes4Reports));
  AddDetailsItem('FNV reports recieved', IntToStr(statistics.fnvReports));
  AddDetailsItem('FO3 reports recieved', IntToStr(statistics.fo3Reports));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Session bandwidth', FormatByteSize(sessionBandwidth));
  AddDetailsItem('Session uptime', TimeStr(sessionUptime));
  AddDetailsItem('Total bandwidth', FormatByteSize(statistics.totalBandwidth + sessionBandwidth));
  AddDetailsItem('Total uptime', TimeStr(statistics.totalUptime + sessionUptime));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Website', 'http://www.nexusmods.com/skyrim/mods/37981');
  AddDetailsItem('API Credits', 'superobject, TurboPower Abbrevia, ZeosDBO');
end;

{ Handle user clicking URL }
procedure TBackendForm.DetailsEditorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: integer;
  value: string;
begin
  // only process left clicks
  if Button <> mbLeft then
    exit;
  DetailsEditor.MouseToCell(X, Y, ACol, ARow);
  try
    value := DetailsEditor.Cells[ACol, ARow];
    if Pos(' ', value) > 0 then
      value := Copy(value, 1, Pos(' ', value));
    if IsURL(value) and ((Now - LastURLTime) * 86400 > 1.0) then begin
      ShellExecute(0, 'open', PChar(value), '', '', SW_SHOWNORMAL);
      LastURLTime := Now;
    end;
  except
    // invalid cell
  end;
end;

{******************************************************************************}
{ List View events
  - NotesListViewData
  - CompareReports
  - UnapprovedListViewChange
  - UnapprovedListViewData
  - ApprovedListViewChange
  - ApprovedListViewColumnClick
  - ApprovedListViewData
}
{******************************************************************************}

procedure TBackendForm.LogListViewData(Sender: TObject; Item: TListItem);
var
  msg: TLogMessage;
begin
  if (Item.Index > Pred(Log.Count)) then
    exit;
  msg := TLogMessage(Log[Item.Index]);
  Item.Caption := msg.time;
  Item.SubItems.Add(msg.group);
  Item.SubItems.Add(msg.&label);
  Item.SubItems.Add(msg.text);

  // handle coloring
  if (msg.group = 'SERVER') then
    LogListView.Canvas.Font.Color := settings.serverMessageColor
  else if (msg.group = 'INIT') then
    LogListView.Canvas.Font.Color := settings.initMessageColor
  else if (msg.group = 'SQL') then
    LogListView.Canvas.Font.Color := settings.SQLMessageColor
  else if (msg.group = 'DATA') then
    LogListView.Canvas.Font.Color := settings.dataMessageColor
  else if (msg.group = 'TASK') then
    LogListView.Canvas.Font.Color := settings.taskMessageColor
  else if (msg.group = 'ERROR') then
    LogListView.Canvas.Font.Color := settings.errorMessageColor;
end;

procedure TBackendForm.LogListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i, x, y: integer;
  ListView: TListView;
  R: TRect;
  msg, FormatString: string;
  ItemArray: array of TVarRec;
  Column: TListColumn;
begin
  ListView := TListView(Sender);
  if Item.Selected then begin
    ListView.Canvas.Brush.Color := $FFEEDD;
    ListView.Canvas.FillRect(Rect);
  end;

  // prepare format string
  FormatString := '';
  for i := 0 to Pred(ListView.Columns.Count) do begin
    Column := ListView.Columns[i];
    if Column.Caption = 'Time' then
      FormatString := FormatString + '[%s] ';
    if Column.Caption = 'Group' then
      FormatString := FormatString + '(%s) ';
    if Column.Caption = 'Label' then
      FormatString := FormatString + '%s: ';
    if Column.Caption = 'Text' then
      FormatString := FormatString + '%s';
  end;

  // prepare item array
  SetLength(ItemArray, 1 + Item.SubItems.Count);
  ItemArray[0].VType := vtUnicodeString;
  ItemArray[0].VUnicodeString := Pointer(Item.Caption);
  for i := 0 to Pred(Item.SubItems.Count) do begin
    ItemArray[i + 1].VType := vtUnicodeString;
    ItemArray[i + 1].VUnicodeString := Pointer(Item.SubItems[i]);
  end;

  // prepare text rect
  R := Rect;
  R.Right := R.Left + ListView.Width - 3;
  x := Rect.Left + 3;
  y := (Rect.Bottom - Rect.Top - ListView.Canvas.TextHeight('Hg')) div 2 + Rect.Top;

  // draw message
  msg := Format(FormatString, ItemArray);
  ListView.Canvas.TextRect(R, x, y, msg);
end;

procedure TBackendForm.UnapprovedListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  UpdateUnapprovedDetails;
end;

procedure TBackendForm.UnapprovedListViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  bUnapprovedAscending := (aUnapprovedColumnToSort = Column.Index) and (not bApprovedAscending);
  aUnapprovedColumnToSort := Column.Index;
  UnapprovedReports.Sort(CompareUnapprovedReports);
  UnapprovedListView.Repaint;
  UpdateUnapprovedDetails;
end;

procedure TBackendForm.UnapprovedListViewData(Sender: TObject; Item: TListItem);
var
  report: TReport;
begin
  report := TReport(UnapprovedReports[Item.Index]);
  Item.Caption := report.game;
  Item.SubItems.Add(report.filename);
  Item.SubItems.Add(DateToStr(report.dateSubmitted));
  Item.SubItems.Add(report.username);
  Item.SubItems.Add(IntToStr(report.rating));
  UnapprovedListView.Canvas.Font.Color := GetRatingColor(report.rating);
  UnapprovedListView.Canvas.Font.Style := [fsBold];
end;

procedure TBackendForm.UnapprovedListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);

var
  ListView: TListView;
  R: TRect;
  x, y, i: integer;
begin
  ListView := TListView(Sender);
  if Item.Selected then begin
    ListView.Canvas.Brush.Color := $FFEEDD;
    ListView.Canvas.FillRect(Rect);
  end;

  R := Rect;
  R.Right := R.Left + ListView.Columns[0].Width - 3;
  x := Rect.Left + 3;
  y := (Rect.Bottom - Rect.Top - ListView.Canvas.TextHeight('Hg')) div 2 + Rect.Top;
  ListView.Canvas.TextRect(R, x, y, Item.Caption);
  for i := 0 to Item.SubItems.Count - 1 do begin
    R.Left := R.Right + 3;
    // fixes drawing error
    R.Right := R.Left + ListView_GetColumnWidth(ListView.Handle, ListView.Columns[i + 1].Index);
    x := R.Left;
    ListView.Canvas.TextRect(R, x, y, Item.SubItems[i]);
  end;
end;

procedure TBackendForm.ApprovedListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateApprovedDetails;
end;

procedure TBackendForm.ApprovedListViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  bApprovedAscending := (aApprovedColumnToSort = Column.Index) and (not bApprovedAscending);
  aApprovedColumnToSort := Column.Index;
  ApprovedReports.Sort(CompareApprovedReports);
  ApprovedListView.Repaint;
  UpdateApprovedDetails;
end;

procedure TBackendForm.ApprovedListViewData(Sender: TObject; Item: TListItem);
var
  report: TReport;
begin
  report := TReport(ApprovedReports[Item.Index]);
  Item.Caption := report.game;
  Item.SubItems.Add(report.filename);
  Item.SubItems.Add(DateToStr(report.dateSubmitted));
  Item.SubItems.Add(report.username);
  Item.SubItems.Add(IntToStr(report.rating));
  ApprovedListView.Canvas.Font.Color := GetRatingColor(report.rating);
  ApprovedListView.Canvas.Font.Style := [fsBold];
end;

procedure TBackendForm.ApprovedListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  ListView: TListView;
  R: TRect;
  x, y, i: integer;
begin
  ListView := TListView(Sender);
  if Item.Selected then begin
    ListView.Canvas.Brush.Color := $FFEEDD;
    ListView.Canvas.FillRect(Rect);
  end;

  R := Rect;
  R.Right := R.Left + ListView.Columns[0].Width - 3;
  x := Rect.Left + 3;
  y := (Rect.Bottom - Rect.Top - ListView.Canvas.TextHeight('Hg')) div 2 + Rect.Top;
  ListView.Canvas.TextRect(R, x, y, Item.Caption);
  for i := 0 to Item.SubItems.Count - 1 do begin
    R.Left := R.Right + 3;
    // fixes drawing error
    R.Right := R.Left + ListView_GetColumnWidth(ListView.Handle, ListView.Columns[i + 1].Index);
    x := R.Left;
    ListView.Canvas.TextRect(R, x, y, Item.SubItems[i]);
  end;
end;


{******************************************************************************}
{ Rebuild Popup Menu events
  - ForceRebuildItemClick
}
{******************************************************************************}

procedure TBackendForm.ForceRebuildItemClick(Sender: TObject);
begin
  bRebuildTES5 := true;
  bRebuildTES4 := true;
  bRebuildFO3 := true;
  bRebuildFNV := true;
  TTaskProcedures.RebuildDictionaries;
end;

{******************************************************************************}
{ Approved Popup Menu events
  - ApprovedPopupMenuPopup
  - EditReportItemClick
  - UnapproveReportItemClick
  - ViewDictionaryEntryItemClick
}
{******************************************************************************}

procedure TBackendForm.ApprovedPopupMenuPopup(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  bItemSelected: boolean;
begin
  bItemSelected := false;
  // process list items
  for i := 0 to Pred(ApprovedListView.Items.Count) do begin
    ListItem := ApprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    bItemSelected := true;
  end;

  EditReportItem.Enabled := bItemSelected;
  UnapproveReportItem.Enabled := bItemSelected;
  ViewDictionaryEntryItem.Enabled := bItemSelected;
end;

procedure TBackendForm.EditReportItemClick(Sender: TObject);
var
  EditForm: TEditForm;
  i: integer;
  ListItem: TListItem;
  report: TReport;
  WhereClause, SetClause: string;
begin
  // process list items
  for i := 0 to Pred(ApprovedListView.Items.Count) do begin
    ListItem := ApprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    // get report data
    report := TReport(ApprovedReports[i]);
    UpdateRebuildBooleans(report);
    WhereClause := ReportWhereClause(report);

    // show edit form for report
    EditForm := TEditForm.Create(Self);
    EditForm.report := report;
    EditForm.ShowModal;

    // get edited values
    report := EditForm.report;
    SetClause := ReportSetClause(report);
    ApprovedReports[i] := report;

    // update report in database
    DBUpdateReport(report, SetClause, WhereClause);

    // clean up
    UpdateApprovedDetails;
    EditForm.Free;
  end;
end;

procedure TBackendForm.UnapproveReportItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  report: TReport;
begin
  // process list items
  for i := Pred(ApprovedListView.Items.Count) downto 0 do begin
    ListItem := ApprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    report := TReport(ApprovedReports[i]);
    UpdateRebuildBooleans(report);
    // update internal lists
    UnapprovedReports.Add(report);
    UnapprovedListView.Items.Count := UnapprovedReports.Count;
    ApprovedListView.Items.Count := ApprovedReports.Count - 1;
    ApprovedReports.Remove(report);
    // update sql
    DBAddReport(report, 'unapproved_reports');
    DBRemoveReport(report, 'approved_reports');
  end;

  // update gui
  ApprovedListView.Repaint;
  UnapprovedListView.Repaint;
end;

procedure TBackendForm.ViewDictionaryEntryItemClick(Sender: TObject);
var
  report: TReport;
  DictionaryForm: TDictionaryForm;
begin
  report := TReport(ApprovedReports[ApprovedListView.ItemIndex]);
  DictionaryForm := TDictionaryForm.Create(Self);
  DictionaryForm.Game := report.game;
  DictionaryForm.FilterFilename := report.filename;
  DictionaryForm.ShowModal;
  DictionaryForm.Free;
end;

{******************************************************************************}
{ Umapproved Popup Menu events
  - UnapprovedPopupMenuPopup
  - ApproveReportItemClick
  - DeleteReportItemClick
}
{******************************************************************************}

procedure TBackendForm.UnapprovedPopupMenuPopup(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  bItemSelected: boolean;
begin
  bItemSelected := false;
   // process list items
  for i := 0 to Pred(UnapprovedListView.Items.Count) do begin
    ListItem := UnapprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    bItemSelected := true;
  end;

  DeleteReportItem.Enabled := bItemSelected;
  ApproveReportItem.Enabled := bItemSelected;
end;

procedure TBackendForm.DeleteReportItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  report: TReport;
begin
  // process list items
  for i := Pred(UnapprovedListView.Items.Count) downto 0 do begin
    ListItem := UnapprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    report := TReport(UnapprovedReports[i]);
    // update internal lists
    UnapprovedListView.Items.Count := UnapprovedReports.Count - 1;
    UnapprovedReports.Remove(report);
    // update sql
    DBRemoveReport(report, 'unapproved_reports');
    // update statistics
    Inc(statistics.reportsDenied);
  end;

  // update gui
  UnapprovedListView.Repaint;
end;

procedure TBackendForm.ApproveReportItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  report: TReport;
begin
  // process list items
  for i := Pred(UnapprovedListView.Items.Count) downto 0 do begin
    ListItem := UnapprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    report := TReport(UnapprovedReports[i]);
    UpdateRebuildBooleans(report);
    // replace reports from same user on same plugin
    if ReportExists(ApprovedReports, report) then begin
      ApprovedListView.Items.Count := ApprovedReports.Count - 1;
      RemoveExistingReports(ApprovedReports, report);
      DBRemoveReport(report, 'approved_reports');
    end;
    // update internal lists
    ApprovedReports.Add(report);
    ApprovedListView.Items.Count := ApprovedReports.Count;
    UnapprovedListView.Items.Count := UnapprovedReports.Count - 1;
    UnapprovedReports.Remove(report);
    // update sql
    DBAddReport(report, 'approved_reports');
    DBRemoveReport(report, 'unapproved_reports');
    // update statistics
    Inc(statistics.reportsApproved);
  end;

  // update gui
  ApprovedListView.Repaint;
  UnapprovedListView.Repaint;
end;


{******************************************************************************}
{ Log Popup Menu events
  - LogPopupMenuPopup
  - FilterInitItemClick
  - FilterSqlItemClick
  - FilterServerItemClick
  - FilterDataItemClick
  - FilterErrorItemClick
  - CopyToClipboardItemClick
  - SaveAndClearItemClick
}
{******************************************************************************}

function EnableStr(var b: boolean): string;
begin
  Result := IfThen(not b, 'Enable', 'Disable');
end;

procedure TBackendForm.LogPopupMenuPopup(Sender: TObject);
var
  i: Integer;
  item: TMenuItem;
  filter: TFilter;
begin
  // rebuild group filter items
  FilterGroupItem.Clear;
  for i := 0 to Pred(GroupFilters.Count) do begin
    filter := TFilter(GroupFilters[i]);
    item := TMenuItem.Create(FilterGroupItem);
    item.Caption := EnableStr(filter.enabled) + ' ' + filter.group;
    item.OnClick := ToggleGroupFilter;
    FilterGroupItem.Add(item);
  end;

  // rebuild label filter items
  FilterLabelItem.Clear;
  for i := 0 to Pred(LabelFilters.Count) do begin
    filter := TFilter(LabelFilters[i]);
    item := TMenuItem.Create(FilterLabelItem);
    item.Caption := EnableStr(filter.enabled) + ' ' + filter.group + ', ' + filter.&label;
    item.OnClick := ToggleLabelFilter;
    FilterLabelItem.Add(item);
  end;

  // toggle copy to clipboard item based on whether or not log items are selected
  CopyToClipboardItem.Enabled := Assigned(LogListView.Selected);

  // rename toggle auto scroll item based on whether or not auto scroll is enabled
  LogPopupMenu.Items[3].Caption := EnableStr(bAutoScroll) + ' auto scroll';
end;

procedure TBackendForm.ToggleGroupFilter(Sender: TObject);
var
  index: integer;
  filter: TFilter;
begin
  index := FilterGroupItem.IndexOf(TMenuItem(Sender));
  filter := GroupFilters[index];
  filter.enabled := not filter.enabled;
  LogListView.Items.Count := 0;
  RebuildLog;
  LogListView.Items.Count := Log.Count;
end;

procedure TBackendForm.ToggleLabelFilter(Sender: TObject);
var
  index: integer;
  filter: TFilter;
begin
  index := FilterLabelItem.IndexOf(TMenuItem(Sender));
  filter := LabelFilters[index];
  filter.enabled := not filter.enabled;
  LogListView.Items.Count := 0;
  RebuildLog;
  LogListView.Items.Count := Log.Count;
end;

procedure TBackendForm.CopyToClipboardItemClick(Sender: TObject);
var
  i: Integer;
  sl: TStringList;
  msg: TLogMessage;
begin
  sl := TStringList.Create;

  // put selected messages in stringlist
  for i := 0 to Pred(Log.Count) do begin
    if not LogListView.Items[i].Selected then
      continue;

    msg := TLogMessage(Log[i]);
    sl.Add(Format('[%s] (%s) %s: %s', [msg.time, msg.group, msg.&label, msg.text]));
  end;

  // put stringlist in clipboard, then free
  Clipboard.AsText := sl.Text;
  sl.Free;
end;

// Toggle auto scroll of the log
procedure TBackendForm.ToggleAutoScrollItemClick(Sender: TObject);
begin
  bAutoScroll := not bAutoScroll;
end;

procedure TBackendForm.SaveAndClearLog;
begin
  if BaseLog.Count > 2000 then
    SaveAndClearItemClick(nil);
end;

procedure TBackendForm.SaveAndClearItemClick(Sender: TObject);
begin
  SaveLog(BaseLog);
  LogListView.Items.Count := 0;
  BaseLog.Clear;
  Log.Clear;
  LogMessage('INIT', 'Log', 'Saved and cleared log.');
end;


{******************************************************************************}
{ TCP Server Events
  Events associated with the TCP server.
  - TCPServerConnect
  - TCPServerDisconnect
  - TCPServerException
  - TCPServerExecute
}
{******************************************************************************}

procedure TBackendForm.TCPServerConnect(AContext: TIdContext);
var
  ip: string;
  user: TUser;
begin
  ip := AContext.Connection.Socket.Binding.PeerIP;
  // handle blacklisted ip
  if IsBlacklisted(ip) then begin
    LogMessage('SERVER', 'Terminated', ip);
    AContext.Connection.Disconnect;
    exit;
  end;

  // handle connection
  LogMessage('SERVER', 'Connected', ip);
  slConnectedIPs.Add(ip);
  user := GetUser(ip);
  if not Assigned(user) then
    user := AddUser(ip);
  Inc(user.timesSeen);
end;

procedure TBackendForm.TCPServerDisconnect(AContext: TIdContext);
var
  ip: string;
  index: integer;
begin
  ip := AContext.Connection.Socket.Binding.PeerIP;
  LogMessage('SERVER', 'Disconnected', ip);
  index := slConnectedIPs.IndexOf(ip);
  if index > -1 then
    slConnectedIPs.Delete(index);
end;

procedure TBackendForm.TCPServerException(AContext: TIdContext;
  AException: Exception);
begin
  if AException.Message = 'Connection Closed Gracefully.' then
    exit;

  LogMessage('ERROR', 'Server', AContext.Connection.Socket.Binding.PeerIP+
    ' '+AException.Message);
end;

procedure TBackendForm.SendResponse(var user: TUser; var AContext: TIdContext;
  id: integer; data: string; bLog: boolean = true);
var
  json: string;
  response: TmpMessage;
begin
  response := TmpMessage.Create(id, '', '', data);
  json := TRttiJson.ToJson(response);
  Inc(user.download, Length(json));
  Inc(sessionBandwidth, Length(json));
  AContext.Connection.IOHandler.WriteLn(json);
  if bLog then LogMessage('SERVER', 'Response', response.data);
  response.Free;
end;

procedure TBackendForm.HandleMessage(msg: TmpMessage; size: integer;
  AContext: TIdContext);
var
  report: TReport;
  note, ip, WhereClause, SetClause: string;
  user: TUser;
  stream: TMemoryStream;
  bAuthorized: boolean;
  userStatistics: TUserStatistics;
begin
  // get ip, authorization
  ip := AContext.Connection.Socket.Binding.PeerIP;
  bAuthorized := Authorized(ip, msg.username, msg.auth);

  // get user and update values
  if bAuthorized then
    user := GetUser(ip, msg.username, msg.auth)
  else
    user := GetUser(ip);
  if not Assigned(user) then
    user := AddUser(ip);

  WhereClause := UserWhereClause(user);
  user.lastSeen := Now;
  Inc(user.upload, size);
  Inc(sessionBandwidth, size);

  // write message to log
  WriteMessage(msg, ip);

  // handle message by id
  case msg.id of
    MSG_NOTIFY: begin
      if msg.data = 'Authorized?' then begin
        note := 'No';
        if bAuthorized then
          note := 'Yes';
        // respond to user
        SendResponse(user, AContext, MSG_NOTIFY, note);
      end
      // game based logins
      else if msg.data = 'TES5' then
        Inc(statistics.tes5Logins)
      else if msg.data = 'TES4' then
        Inc(statistics.tes4Logins)
      else if msg.data = 'FNV' then
        Inc(statistics.fnvLogins)
      else if msg.data = 'FO3' then
        Inc(statistics.fo3Logins);
    end;

    MSG_REGISTER: begin
      note := 'Username unavailable';
      if UsernameAvailable(msg.username) then begin
        if msg.data = 'Check' then
          note := 'Available'
        else if msg.data = 'Register' then begin
          UpdateUser(ip, msg.username, msg.auth);
          note := 'Registered '+msg.username;
        end;
      end;
      // respond to user
      SendResponse(user, AContext, MSG_NOTIFY, note);
    end;

    MSG_AUTH_RESET:  begin
      note := 'Failed';
      if ResetAuth(ip, msg.username, msg.auth) then
        note := 'Success';
      // respond to user
      SendResponse(user, AContext, MSG_NOTIFY, note);
    end;

    MSG_STATISTICS:  begin
      note := 'Not authorized';
      if bAuthorized then try
        userStatistics := TUserStatistics(TRttiJson.FromJson(msg.data, TUserStatistics));
        user.updateStatistics(userStatistics);
        note := 'Statistics recieved';
        userStatistics.Free;
      except
        on x : Exception do begin
          LogMessage('ERROR', 'Server', 'Failed to load statistics '+x.Message);
          note := 'Failed to load statistics.';
        end;
      end;
      // respond to user
      SendResponse(user, AContext, MSG_NOTIFY, note);
    end;

    MSG_STATUS: begin
      SendResponse(user, AContext, MSG_STATUS, TRttiJson.ToJson(status), false);
      LogMessage('SERVER', 'Response', 'Current status');
    end;

    MSG_REQUEST:  begin
      if (Pos('Dictionary', msg.data) > 0) then begin
        if FileExists(msg.data) then begin
          stream := TMemoryStream.Create;
          stream.LoadFromFile(msg.data);
          AContext.Connection.IOHandler.LargeStream := True;
          AContext.Connection.IOHandler.Write(stream, 0, true);
          Inc(sessionBandwidth, stream.Size);
          Inc(user.download, stream.Size);
          Inc(statistics.dictionaryUpdates);
          LogMessage('SERVER', 'Response', 'Sent '+msg.data+' '+ GetDictionaryHash(msg.data));
          stream.Free;
        end;
      end
      else if msg.data = 'Program' then begin
        if FileExists('MergePlugins.zip') then begin
          stream := TMemoryStream.Create;
          stream.LoadFromFile('MergePlugins.zip');
          AContext.Connection.IOHandler.LargeStream := True;
          AContext.Connection.IOHandler.Write(stream, 0, true);
          Inc(sessionBandwidth, stream.Size);
          Inc(user.download, stream.Size);
          Inc(statistics.programUpdates);
          LogMessage('SERVER', 'Response', 'Sent '+msg.data+' '+ status.programVersion);
          stream.Free;
        end
        else
          LogMessage('ERROR', 'Server', 'User requested '+msg.data);
      end
      else if msg.data = 'Changelog' then begin
        if FileExists('changelog.txt') then begin
          stream := TMemoryStream.Create;
          stream.LoadFromFile('changelog.txt');
          AContext.Connection.IOHandler.LargeStream := True;
          AContext.Connection.IOHandler.Write(stream, 0, true);
          Inc(sessionBandwidth, stream.Size);
          Inc(user.download, stream.Size);
          LogMessage('SERVER', 'Response', 'Sent changelog');
          stream.Free;
        end
        else
          Logger.Write('ERROR', 'Server', 'Changelog missing');
      end
      else
        Logger.Write('ERROR', 'Server', 'Unknown request '+msg.data);
    end;

    MSG_REPORT: begin
      note := 'Not authorized';
      if bAuthorized then try
        report := TReport.Create;
        report := TReport(TRttiJson.FromJson(msg.data, TReport));
        report.username := msg.username;
        report.dateSubmitted := Now;
        report.notes := StringReplace(report.notes, '@13', #13#10, [rfReplaceAll]);
        report.notes := Wordwrap(report.notes, 70);
        LogMessage('SERVER', 'Message', Format('Recieved report %s %s %s',
          [report.game, report.username, report.filename]));

        // replace reports from same user on same plugin
        if ReportExists(UnapprovedReports, report) then begin
          UnapprovedListView.Items.Count := UnapprovedReports.Count - 1;
          RemoveExistingReports(UnapprovedReports, report);
          DBRemoveReport(report, 'unapproved_reports');
        end;

        // add report to gui
        UnapprovedReports.Add(report);
        UnapprovedListView.Items.Count := UnapprovedReports.Count;

        // add report to sql
        DBAddReport(report, 'unapproved_reports');
        note := 'Report accepted.';
        Inc(statistics.reportsRecieved);
        Inc(user.reportsSubmitted);

        // update statistics based on game
        if report.game = 'TES5' then
          Inc(statistics.tes5Reports)
        else if report.game = 'TES4' then
          Inc(statistics.tes4Reports)
        else if report.game = 'FNV' then
          Inc(statistics.fnvLogins)
        else if report.game = 'FO3' then
          Inc(statistics.fo3Reports);
      except
        on x : Exception do begin
          LogMessage('ERROR', 'Server', 'Failed to load report '+x.Message);
          note := 'Failed to load report.';
        end;
      end;

      // respond to user
      SendResponse(user, AContext, MSG_NOTIFY, note);
    end;
  end;

  // update user
  SetClause := UserSetClause(user);
  DBUpdateUser(SetClause, WhereClause);
end;

procedure TBackendForm.WriteMessage(msg: TmpMessage; ip: string);
begin
  LogMessage('SERVER', 'Message', ip+' ('+msg.username+')  '+MSG_STRINGS[msg.id]);
  if (Length(msg.data) > 1) and (Length(msg.data) < 60) then
    LogMessage('SERVER', 'Message', msg.data);
end;

procedure TBackendForm.TCPServerExecute(AContext: TIdContext);
var
  msg: TmpMessage;
  LLine: string;
  size: integer;
begin
  LLine := AContext.Connection.IOHandler.ReadLn(TIdTextEncoding.Default);
  size := Length(LLine);
  Inc(sessionBandwidth, size);
  if size < 3 then
    exit;

  msg := TmpMessage(TRttiJson.FromJson(LLine, TmpMessage));
  if (msg.id > 0) then
    HandleMessage(msg, size, AContext);

  // free message
  msg.Free;
end;

{******************************************************************************}
{ QuickBar Button Events
  Events involving buttons on the QuickBar.  Events include:
  - ApproveButtonClick
  - RebuildButtonClick
  - ReportButtonClick
  - OptionsButtonClick
  - DictionaryButtonClick
  - UpdateButtonClick
  - HelpButtonClick
}
{******************************************************************************}

procedure TBackendForm.ApproveButtonClick(Sender: TObject);
var
  i: integer;
  report: TReport;
begin
  // process list items
  for i := Pred(UnapprovedListView.Items.Count) downto 0 do begin
    report := TReport(UnapprovedReports[i]);
    UpdateRebuildBooleans(report);
    // update internal lists
    ApprovedReports.Add(report);
    ApprovedListView.Items.Count := ApprovedReports.Count;
    UnapprovedListView.Items.Count := UnapprovedReports.Count - 1;
    UnapprovedReports.Remove(report);
    // update sql
    DBAddReport(report, 'approved_reports');
    DBRemoveReport(report, 'unapproved_reports');
  end;

  // update gui
  ApprovedListView.Repaint;
  UnapprovedListView.Repaint;
end;

procedure TBackendForm.RebuildButtonClick(Sender: TObject);
begin
  TTaskProcedures.RebuildDictionaries;
end;

procedure TBackendForm.DictionaryButtonClick(Sender: TObject);
var
  DictionaryForm: TDictionaryForm;
begin
  DictionaryForm := TDictionaryForm.Create(Self);
  DictionaryForm.ShowModal;
  DictionaryForm.Free;
end;

procedure TBackendForm.OptionsButtonClick(Sender: TObject);
var
  OptionsForm: TOptionsForm;
begin
  OptionsForm := TOptionsForm.Create(Self);
  OptionsForm.ShowModal;
  OptionsForm.Free;
  // update listviews
  LogListView.OwnerDraw := not settings.simpleLogView;
  LogListView.Repaint;
  ApprovedListView.OwnerDraw := not settings.simpleReportsView;
  ApprovedListView.Repaint;
  UnapprovedListView.OwnerDraw := not settings.simpleReportsView;
  UnapprovedListView.Repaint;
end;

procedure TBackendForm.UpdateButtonClick(Sender: TObject);
begin
  status.Refresh;
end;

procedure TBackendForm.HelpButtonClick(Sender: TObject);
begin
  // comment
end;

end.
