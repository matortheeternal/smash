unit msSmashForm;

interface

uses
  // delphi units
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, ComCtrls, XPMan, StdCtrls, ImgList, CommCtrl, Menus, Grids,
  ValEdit, ShellAPI, StrUtils, Clipbrd,
  // indy units
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  // third party libraries
  superobject, W7Taskbar,
  // mte components
  mteHelpers, mteTracker, mteLogger, mteLogging, mteProgressForm,
  mteBase, mteTaskHandler, RttiTranslation,
  // ms units
  msCore, msConfiguration, msClient, msLoader, msThreads, msOptionsForm,
  msEditForm, msSettingsManager, msTagManager, msSplashForm,
  // tes5edit units
  wbBSA, wbHelpers, wbInterface, wbImplementation;

type
  TSmashForm = class(TForm)
    [FormPrefix('msMain')]
      XPManifest: TXPManifest;
      IconList: TImageList;
      TaskTimer: TTimer;
      [FormSection('QuickBar')]
        QuickBar: TPanel;
        NewButton: TSpeedButton;
        BuildButton: TSpeedButton;
        SubmitButton: TSpeedButton;
        ManageButton: TSpeedButton;
        DictionaryButton: TSpeedButton;
        OptionsButton: TSpeedButton;
        UpdateButton: TSpeedButton;
        HelpButton: TSpeedButton;
        bhBuild: TBalloonHint;
        bhNew: TBalloonHint;
        bhSubmit: TBalloonHint;
        bhManage: TBalloonHint;
        bhDict: TBalloonHint;
        bhOptions: TBalloonHint;
        bhUpdate: TBalloonHint;
        bhHelp: TBalloonHint;
      [FormSection('Main Panel')]
        MainPanel: TPanel;
        Splitter: TSplitter;
        PageControl: TPageControl;
        DetailsPanel: TPanel;
        [FormSection('Details Panel')]
          DetailsLabel: TLabel;
          DetailsGrid: TStringGrid;
          DetailsPopupMenu: TPopupMenu;
          DetailsCopyToClipboardItem: TMenuItem;
        [FormSection('Plugins Tab')]
          PluginsTabSheet: TTabSheet;
          PluginsListView: TListView;
          [FormSection('Plugins Popup Menu')]
            PluginsPopupMenu: TPopupMenu;
            AddToPatchItem: TMenuItem;
            RemoveFromPatchItem: TMenuItem;
            TagsItem: TMenuItem;
            ManageTagsItem: TMenuItem;
            ApplySettingTagsItem: TMenuItem;
            ClearTagsItem: TMenuItem;
            OpenPluginLocationItem: TMenuItem;
            SmashSettingItem: TMenuItem;
        [FormSection('Patches Tab')]
          PatchesTabSheet: TTabSheet;
          PatchesListView: TListView;
          [FormSection('Patches Popup Menu')]
            PatchesPopupMenu: TPopupMenu;
            ToggleRebuildItem: TMenuItem;
            OpenInExplorerItem: TMenuItem;
            BuildPatchItem: TMenuItem;
            EditPatchItem: TMenuItem;
            DeletePatchItem: TMenuItem;
            [FormSection('Move Submenu')]
              MoveItem: TMenuItem;
              UpItem: TMenuItem;
              DownItem: TMenuItem;
              ToTopItem: TMenuItem;
              ToBottomItem: TMenuItem;
        [FormSection('Log Tab')]
          LogTabSheet: TTabSheet;
          LogListView: TListView;
          [FormSection('Log Popup Menu')]
            LogPopupMenu: TPopupMenu;
            FilterGroupItem: TMenuItem;
            FilterLabelItem: TMenuItem;
            CopyToClipboardItem: TMenuItem;
            SaveAndClearItem: TMenuItem;
            ToggleAutoScrollItem: TMenuItem;
      [FormSection('Status Bar')]
        StatusPanel: TPanel;
        StatusPanelMessage: TPanel;
        StatusPanelBlocking: TPanel;
        StatusPanelProgram: TPanel;
        StatusPanelDictionary: TPanel;
        StatusPanelConnection: TPanel;
        StatusPanelLanguage: TPanel;
        StatusPanelVersion: TPanel;
        ImageBlocked: TImage;
        ImageDisconnected: TImage;
        ImageBuild: TImage;
        ImageDictionaryUpdate: TImage;
        ImageProgramUpdate: TImage;
        ImageConnected: TImage;
        bhLoader: TBalloonHint;
        bhLoadException: TBalloonHint;

    // SMASH FORM EVENTS
    procedure UpdateLog;
    procedure LogMessage(const group, &label, text: string);
    procedure ToggleFormState(bEnabled: boolean);
    procedure FormCreate(Sender: TObject);
    procedure InitDone;
    procedure FormShow(Sender: TObject);
    procedure LoaderStatus(s: string);
    procedure LoaderDone;
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SaveDone;
    procedure ConnectDone;
    procedure ProgressDone;
    procedure AutoUpdate;
    function ShouldDisplay(bh: TBalloonHint): boolean;
    procedure DisableHints;
    procedure HideHints;
    procedure DisplayHints;
    procedure Reconnect;
    procedure Heartbeat;
    procedure RefreshGUI;
    procedure OnTaskTimer(Sender: TObject);
    procedure ShowAuthorizationMessage;
    procedure UpdateStatusBar;
    procedure UpdateListViews;
    // DETAILS EDITOR EVENTS
    procedure AddDetailsItem(name, value: string);
    procedure AddDetailsList(name: string; sl: TStringList);
    procedure PageControlChange(Sender: TObject);
    procedure UpdateApplicationDetails;
    procedure DetailsCopyToClipboardItemClick(Sender: TObject);
    procedure DetailsPanelResize(Sender: TObject);
    procedure DetailsGridMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure DetailsGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DetailsGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    // PLUGINS LIST VIEW EVENTS
    procedure UpdatePluginDetails;
    procedure AddPluginsToPatch(var patch: TPatch);
    procedure ChangePatchSetting(aSetting: TSmashSetting);
    procedure PluginsListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure PluginsListViewData(Sender: TObject; Item: TListItem);
    procedure PluginsListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    // PLUGINS POPUP MENU EVENTS
    procedure PluginsPopupMenuPopup(Sender: TObject);
    procedure UpdatePluginsPopupMenu;
    procedure AddToNewPatchClick(Sender: TObject);
    procedure AddToPatchClick(Sender: TObject);
    procedure ChangeSettingClick(Sender: TObject);
    procedure RemoveFromPatchItemClick(Sender: TObject);
    procedure OpenPluginLocationItemClick(Sender: TObject);
    procedure ManageTagsItemClick(Sender: TObject);
    procedure ClearTagsItemClick(Sender: TObject);
    procedure ApplySettingTagsItemClick(Sender: TObject);
    // SMASH LIST VIEW EVENTS
    procedure UpdatePatchDetails;
    procedure UpdatePatches;
    function NewPatch: TPatch;
    procedure PatchesListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure PatchesListViewData(Sender: TObject; Item: TListItem);
    procedure PatchesListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure PatchesListViewDblClick(Sender: TObject);
    procedure PatchesListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    // PATCHES POPUP MENU EVENTS
    procedure PatchesPopupMenuPopup(Sender: TObject);
    procedure EditPatchItemClick(Sender: TObject);
    procedure BuildPatchItemClick(Sender: TObject);
    procedure RemovePluginsItemClick(Sender: TObject);
    procedure DeletePatchItemClick(Sender: TObject);
    procedure OpenInExplorerItemClick(Sender: TObject);
    procedure ToggleRebuildItemClick(Sender: TObject);
    procedure UpItemClick(Sender: TObject);
    procedure DownItemClick(Sender: TObject);
    procedure ToTopItemClick(Sender: TObject);
    procedure ToBottomItemClick(Sender: TObject);
    // LOG LIST VIEW EVENTS
    procedure LogListViewData(Sender: TObject; Item: TListItem);
    procedure LogListViewDrawItem(Sender: TCustomListView; Item: TListItem; Rect: TRect; State: TOwnerDrawState);
    // LOG POPUP MENU EVENTS
    procedure LogPopupMenuPopup(Sender: TObject);
    procedure ToggleGroupFilter(Sender: TObject);
    procedure ToggleLabelFilter(Sender: TObject);
    procedure CopyToClipboardItemClick(Sender: TObject);
    procedure SaveAndClearItemClick(Sender: TObject);
    // QUICKBAR EVENTS
    procedure UpdateQuickbar;
    procedure CreatePatchButtonClick(Sender: TObject);
    procedure BuildButtonClick(Sender: TObject);
    procedure SubmitButtonClick(Sender: TObject);
    procedure ManageButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ToggleAutoScrollItemClick(Sender: TObject);
    procedure ImageDisconnectedClick(Sender: TObject);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure RemoveUnloadedPluginsItemClick(Sender: TObject);
  protected
    procedure WMSize(var AMessage: TMessage); message WM_SIZE;
    procedure WMMove(var AMessage: TMessage); message WM_MOVE;
    procedure WMActivateApp(var AMessage: TMessage); message WM_ACTIVATEAPP;
  private
    { Private declarations }
    slDetails: TStringList;
  public
    { Public declarations }
  end;

const
  MessageDelay = (0.1 / 86400.0);

var
  splash: TSplashForm;
  SmashForm: TSmashForm;
  LastHint: string;
  LastURLTime, LastMessageTime, FormDisplayTime: double;
  bPatchesToBuild, bPatchesToCheck, bAutoScroll, bCreated, bClosing: boolean;
  pForm: TProgressForm;
  TaskHandler: TTaskHandler;

implementation

{$R *.dfm}


{******************************************************************************}
{ Patch Form Events
  Events for the Patch Form.
  - UpdateLog
  - LogMessage
  - ProgressMessage
  - FormCreate
  - FormShow
  - LoaderDone
  - FormClose
}
{******************************************************************************}

procedure TSmashForm.UpdateLog;
var
  bLogActive: boolean;
begin
  LogListView.Items.Count := Log.Count;
  bLogActive := PageControl.ActivePage = LogTabSheet;
  // autoscroll if active
  if bAutoScroll and bLogActive then begin
    //LogListView.ClearSelection;
    //LogListView.Items[Pred(LogListView.Items.Count)].MakeVisible(false);
    SendMessage(LogListView.Handle, WM_VSCROLL, SB_LINEDOWN, 0);
  end;
  // correct width if active
  if bLogActive then
    ListView_CorrectWidth(LogListView);
end;

{ Prints a message to the log }
procedure TSmashForm.LogMessage(const group, &label, text: string);
var
  msg: TLogMessage;
begin
  msg := TLogMessage.Create(
    FormatDateTime('hh:nn:ss', Now),
    FormatDateTime('hh:nn:ss', Now - AppStartTime),
    group, &label, text);
  BaseLog.Add(msg);

  // if message is enabled, add to log
  if MessageEnabled(msg) then begin
    Log.Add(msg);
    // if patch form is created, update log list view
    if bCreated then
      UpdateLog;
  end;
end;

procedure ProgressMessage(const s: string);
begin
  if s = '' then
    exit;
  Logger.Write(xEditLogGroup, xEditLogLabel, s);
end;

procedure InitLog;
begin
  BaseLog := TList.Create;
  Log := TList.Create;
  LabelFilters := TList.Create;
  GroupFilters := TList.Create;
  // INITIALIZE GROUP FILTERS
  GroupFilters.Add(TFilter.Create('GENERAL', true));
  GroupFilters.Add(TFilter.Create('LOAD', true));
  GroupFilters.Add(TFilter.Create('CLIENT', true));
  GroupFilters.Add(TFilter.Create('PATCH', true));
  GroupFilters.Add(TFilter.Create('PLUGIN', true));
  GroupFilters.Add(TFilter.Create('ERROR', true));
  // INITIALIZE LABEL FILTERS
  LabelFilters.Add(TFilter.Create('GENERAL', 'Game', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Status', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Path', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Definitions', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Dictionary', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Load Order', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Log', true));
  LabelFilters.Add(TFilter.Create('LOAD', 'Order', false));
  LabelFilters.Add(TFilter.Create('LOAD', 'Plugins', false));
  LabelFilters.Add(TFilter.Create('LOAD', 'Background', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Connect', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Login', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Response', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Update', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Report', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Status', false));
  LabelFilters.Add(TFilter.Create('PATCH', 'Create', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Edit', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Check', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Clean', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Delete', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Build', true));
  LabelFilters.Add(TFilter.Create('PATCH', 'Report', true));
  LabelFilters.Add(TFilter.Create('PLUGIN', 'Report', true));
  LabelFilters.Add(TFilter.Create('PLUGIN', 'Check', true));
  LabelFilters.Add(TFilter.Create('PLUGIN', 'Tags', false));
  LabelFilters.Add(TFilter.Create('PLUGIN', 'Settings', true));
end;

{ Initialize form, initialize TES5Edit API, and load plugins }
procedure TSmashForm.FormCreate(Sender: TObject);
begin
  // INITIALIAZE BASE
  bCreated := false;
  AppStartTime := Now;
  InitLog;
  Logger.OnLogEvent := LogMessage;
  //bAutoScroll := true;
  InitializeTaskbarAPI;
  SetTaskbarProgressState(tbpsIndeterminate);
  xEditLogGroup := 'LOAD';
  xEditLogLabel := 'Plugins';
  wbProgressCallback := ProgressMessage;
  StatusCallback := LoaderStatus;
  UpdateCallback := AutoUpdate;

  if not InitBase then begin
    ProgramStatus.bClose := true;
    exit;
  end;

  // CREATE SPLASH
  splash := TSplashForm.Create(nil);
  try
    InitCallback := InitDone;
    TInitThread.Create;
    splash.ShowModal;
  finally
    splash.Free;
  end;

  // do translation dump?
  if bTranslationDump then
    TRttiTranslation.Save('lang\english.lang', self);

  // load language
  TRttiTranslation.Load(language, self);

  // finalize
  bCreated := true;
end;

procedure TSmashForm.FormDestroy(Sender: TObject);
begin
  // free lists
  FreeList(GroupFilters);
  FreeList(LabelFilters);

  // free other items
  TryToFree(TCPClient);
  TryToFree(TaskHandler);
  TryToFree(slDetails);
end;

procedure TSmashForm.ToggleFormState(bEnabled: boolean);
begin
  // show/hide hints
  if bEnabled then
    DisplayHints
  else
    HideHints;

  // disable/enable form
  Enabled := bEnabled;
end;

procedure TSmashForm.WMSize(var AMessage: TMessage);
begin
  if bCreated and (Now - LastMessageTime > MessageDelay) then begin
    LastMessageTime := Now;
    if (AMessage.WParam <> SIZE_MINIMIZED) then
      DisplayHints;
  end;
  inherited;
end;

procedure TSmashForm.WMMove(var AMessage: TMessage);
begin
  if bCreated and (Now - LastMessageTime > MessageDelay) then begin
    LastMessageTime := Now;
    if (AMessage.WParam <> SIZE_MINIMIZED) then
      DisplayHints;
  end;
  inherited;
end;

procedure TSmashForm.WMActivateApp(var AMessage: TMessage);
begin
  if bCreated and (Now - LastMessageTime > MessageDelay) then begin
    LastMessageTime := Now;
    if AMessage.WParam = 1 then
      DisplayHints
    else
      HideHints;
  end;
  inherited;
end;

procedure TSmashForm.InitDone;
begin
  splash.ModalResult := mrOk;
end;

// Force PluginsListView to autosize columns
procedure TSmashForm.FormShow(Sender: TObject);
begin
  // CLOSE IF PLUGIN SELECTION CANCELED
  if ProgramStatus.bClose then begin
    bClosing := true;
    Close;
    exit;
  end;

  // HANDLE AUTO-UPDATE
  {if ProgramStatus.bInstallUpdate then begin
    Logger.Write('CLIENT', 'Disconnect', 'Disconnecting...');
    TCPClient.Disconnect;
    ProgramStatus.bClose := true;
    bClosing := true;
    Logger.Write('GENERAL', 'Update', 'Restarting.');
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);
    Close;
  end;}

  // DISABLE GUI IF INITIALIZATION EXCEPTION
  if ProgramStatus.bInitException then begin
    StatusPanelMessage.Caption := 'The application failed to initialize';
    Logger.Write('ERROR', 'Load', 'There was an exception initializing the application');
    Logger.Write('ERROR', 'Load', 'Review your log messages to resolve the issue');
    Logger.Write('ERROR', 'Load', 'You can also change the program''s settings, if necessary');
    PluginsTabSheet.Enabled := false;
    PluginsTabSheet.TabVisible := false;
    PatchesTabSheet.Enabled := false;
    PatchesTabSheet.TabVisible := false;
    PageControl.ActivePage := LogTabSheet;
    PageControlChange(PageControl);
  end;

  // QUICKBAR
  NewButton.Flat := true;
  BuildButton.Flat := true;
  SubmitButton.Flat := true;
  ManageButton.Flat := true;
  DictionaryButton.Flat := true;
  OptionsButton.Flat := true;
  UpdateButton.Flat := true;
  HelpButton.Flat := true;
  IconList.GetBitmap(0, NewButton.Glyph);
  IconList.GetBitmap(1, BuildButton.Glyph);
  IconList.GetBitmap(2, SubmitButton.Glyph);
  IconList.GetBitmap(3, ManageButton.Glyph);
  IconList.GetBitmap(4, DictionaryButton.Glyph);
  IconList.GetBitmap(5, OptionsButton.Glyph);
  IconList.GetBitmap(6, UpdateButton.Glyph);
  IconList.GetBitmap(7, HelpButton.Glyph);

  // STATUSBAR VALUES
  StatusPanelLanguage.Caption := settings.language;
  StatusPanelVersion.Caption := 'v'+LocalStatus.ProgramVersion;

  // UPDATE GUI
  slDetails := TStringList.Create;
  PluginsListView.OwnerDraw := not settings.simplePluginsView;
  PluginsListView.Items.Count := PluginsList.Count;
  UpdateLog;
  UpdatePatches;
  UpdatePluginsPopupMenu;
  UpdateStatusBar;
  UpdateQuickBar;

  if not ProgramStatus.bInitException then begin
    // ATTEMPT TO CONNECT TO SERVER
    {ConnectCallback := ConnectDone;
    if (not bConnecting) and (not TCPClient.Connected) then
      TConnectThread.Create; }

    // START BACKGROUND LOADER
    LoaderCallback := LoaderDone;
    SetTaskbarProgressState(tbpsIndeterminate);
    if settings.buildRefs then
      TLoaderThread.Create;

    // CORRECT LIST VIEW WIDTHS
    ListView_CorrectWidth(PatchesListView);
    ListView_CorrectWidth(PluginsListView);

    // LOAD AND DISPLAY HINTS
    if settings.buildRefs then
      StatusPanelMessage.Caption := GetLanguageString('msMain_LoaderInProgress');
    bhLoader.Title := GetLanguageString('msMain_LoaderInProgress');
    bhLoader.Description := GetLanguageString('msMain_LoaderLimitations');
    bhLoadException.Title := GetLanguageString('msMain_LoadException');
    bhLoadException.Description := GetLanguageString('msMain_PluginsNotLoaded');
    DisplayHints;

    // initialize task handler
    TaskHandler := TTaskHandler.Create;
    bLogTasks := false;
    TaskHandler.AddTask(TTask.Create('Disable Hints', 12.0 * seconds, DisableHints));
    {TaskHandler.AddTask(TTask.Create('Reconnect', 15.0 * seconds, Reconnect));
    TaskHandler.AddTask(TTask.Create('Heartbeat', 0.9 * seconds, Heartbeat));}
    TaskHandler.AddTask(TTask.Create('Refresh GUI', 3.0 * seconds, RefreshGUI));
    TaskTimer.Enabled := true;
  end;

  // ACTIVATE WINDOW
  FormDisplayTime := Now;
  ForceForeground(Handle);
  if not settings.buildRefs then
    LoaderDone;
end;

procedure TSmashForm.LoaderStatus(s: string);
begin
  StatusPanelMessage.Caption := s;
end;

procedure TSmashForm.LoaderDone;
begin
  wbLoaderDone := true;
  SetTaskbarProgressState(tbpsNone);
  xEditLogGroup := 'GENERAL';
  xEditLogLabel := 'xEdit';
  FlashWindow(Application.Handle, True);
  UpdateQuickbar;
end;

procedure TSmashForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := ProgramStatus.bClose;
  if not bClosing then begin
    bClosing := true;
    Enabled := false;

    // show progress form
    pForm := TProgressForm.Create(Self);
    pForm.pfLogPath := LogPath + 'save\';
    pForm.PopupParent := Self;
    pForm.Caption := GetLanguageString('msProg_Closing');
    pForm.SetMaxProgress(PluginsList.Count + PatchesList.Count + 2);
    pForm.Show;

    // start save thread
    TaskTimer.Enabled := false;
    SaveCallback := SaveDone;
    TSaveThread.Create;
  end;
end;

procedure TSmashForm.SaveDone;
begin
  // clean up pForm, close form
  pForm.SetProgress(pForm.ProgressBar.Max);
  pForm.SaveLog;
  pForm.Free;

  // restart program if update applied
  if ProgramStatus.bInstallUpdate then
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);
  // restart program if user wants patch profile change
  if ProgramStatus.bChangeProfile then
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);

  // allow close and close
  SaveLog(BaseLog);
  ProgramStatus.bClose := true;
  Close;
end;

procedure TSmashForm.ConnectDone;
begin
  // UPDATE QUICKBAR
  UpdateQuickbar;
end;

procedure TSmashForm.ProgressDone;
begin
  xEditLogGroup := 'GENERAL';
  pForm.SaveLog;
  pForm.Visible := false;
  FlashWindow(Application.Handle, True);
  pForm.ShowModal;
  pForm.Free;
  Enabled := true;
  ShowWindow(Application.Handle, SW_RESTORE);
  SetForegroundWindow(Application.Handle);

  // free lists
  if Assigned(timeCosts) then timeCosts.Free;
  if Assigned(pluginsToHandle) then pluginsToHandle.Free;
  if Assigned(patchesToBuild) then patchesToBuild.Free;

  // update patches and gui
  UpdateListViews;
  UpdatePatches;
  UpdateQuickbar;
  UpdatePluginsPopupMenu;
end;

procedure TSmashForm.AutoUpdate;
begin
  if settings.updateDictionary then begin
    // update dictionary
    if ProgramStatus.bDictionaryUpdate and UpdateDictionary then begin
      LocalStatus := TmsStatus.Create;
      CompareStatuses;
    end;
  end;
  if settings.updateProgram then begin
    // update program
    if ProgramStatus.bProgramUpdate and DownloadProgram then
      ProgramStatus.bInstallUpdate := UpdateProgram;
  end;
end;

function TSmashForm.ShouldDisplay(bh: TBalloonHint): boolean;
begin
  Result := (Now - FormDisplayTime) * 86400 < (bh.HideAfter / 1000);
end;

procedure TSmashForm.DisableHints;
begin
  HideHints;
  TaskHandler.RemoveTask('Disable Hints');
end;

procedure TSmashForm.HideHints;
begin
  bhLoader.HideHint;
  bhLoadException.HideHint;
end;

procedure TSmashForm.DisplayHints;
var
  pt: TPoint;
begin
  if ProgramStatus.bLoadException and ShouldDisplay(bhLoadException) then begin
    pt.X := 126;
    pt.Y := 16;
    pt := MainPanel.ClientToScreen(pt);
    bhLoadException.ShowHint(pt);
  end;

  if settings.buildRefs and ShouldDisplay(bhLoader) then begin
    pt.X := 8;
    pt.Y := 4;
    pt := ImageBlocked.ClientToScreen(pt);
    bhLoader.ShowHint(pt);
  end;
end;

procedure TSmashForm.Reconnect;
begin
  if not (TCPClient.Connected or ProgramStatus.bConnecting or bClosing) then
    TConnectThread.Create;
end;

procedure TSmashForm.RefreshGUI;
begin
  if not bClosing then UpdateStatusBar;
end;

procedure TSmashForm.Heartbeat;
begin
  try
    if TCPClient.IOHandler.Opened and
    not (ProgramStatus.bConnecting or bClosing or ServerAvailable) then
      raise Exception.Create('Connection unavailable');
  except
    on x : Exception do begin
      if Assigned(TCPClient) and Assigned(TCPClient.IOHandler) then begin
        Logger.Write('CLIENT', 'Connection', 'Connection to server lost.');
        TCPClient.IOHandler.CloseGracefully;
      end;
    end;
  end;
end;

procedure TSmashForm.OnTaskTimer(Sender: TObject);
begin
  if not bClosing then
    TaskHandler.ExecTasks;
end;

procedure TSmashForm.ShowAuthorizationMessage;
begin
  if ProgramStatus.bAuthorized then begin
    Logger.Write('CLIENT', 'Login', 'Authorized');
  end
  else begin
    Logger.Write('CLIENT', 'Login', 'Not authorized');
  end;
end;

procedure TSmashForm.UpdateStatusBar;
begin
  ImageBlocked.Visible := not (wbLoaderDone or ProgramStatus.bInitException);
  ImageConnected.Visible := TCPClient.Connected;
  ImageDisconnected.Visible := not TCPClient.Connected;
  ImageBuild.Visible := wbLoaderDone and bPatchesToBuild;
  ImageDictionaryUpdate.Visible := ProgramStatus.bDictionaryUpdate;
  ImageProgramUpdate.Visible := ProgramStatus.bProgramUpdate;
  StatusPanelLanguage.Caption := settings.language;
end;

procedure TSmashForm.UpdateListViews;
begin
  if PageControl.ActivePage = PluginsTabSheet then begin
    UpdatePluginDetails;
    PluginsListView.Repaint;
  end;
  if PageControl.ActivePage = PatchesTabSheet then begin
    UpdatePatchDetails;
    PatchesListView.Repaint;
  end;
  if PageControl.ActivePage = LogTabSheet then
    LogListView.Repaint;
end;

{******************************************************************************}
{ Details Editor Events
  Methods for helping with the DetailsGrid control.  Methods include:
  - AddDetailsItem
  - AddDetailsList
  - PageControlChange
  - UpdateApplicationDetails
}
{******************************************************************************}

{
   Adds a ListItem to DetailsView with @name and @value
}
procedure TSmashForm.AddDetailsItem(name, value: string);
begin
  slDetails.Add(name + '=' + value);
end;

{
  Add one or more ListItem to DetailsView with @name and the values
  in @sl
}
procedure TSmashForm.AddDetailsList(name: string; sl: TStringList);
var
  i: integer;
begin
  if sl.Count > 0 then begin
    for i := 0 to Pred(sl.Count) do
      slDetails.Add(Format('%s[%d]=%s', [name, i, sl[i]]));
  end
  else
    slDetails.Add(name + '= ');
end;

{
  Switch details view when page control is changed
}
procedure TSmashForm.PageControlChange(Sender: TObject);
var
  ndx: integer;
begin
  ndx := TPageControl(Sender).ActivePageIndex;
  case ndx of
    0: begin
      UpdatePluginDetails;
      ListView_CorrectWidth(PluginsListView);
    end;
    1: begin
      UpdatePatchDetails;
      ListView_CorrectWidth(PatchesListView);
    end;
    2: begin
      UpdateApplicationDetails;
      ListView_CorrectWidth(LogListView);
    end;
  end;
end;

procedure TSmashForm.UpdateApplicationDetails;
begin
  // prepare list view for application information
  slDetails.Clear;
  DetailsLabel.Caption := GetLanguageString('msMain_AppDetails');

  // add details items
  AddDetailsItem(GetLanguageString('msMain_Application'), 'Mator Smash');
  AddDetailsItem(GetLanguageString('msMain_Author'), 'matortheeternal');
  AddDetailsItem(GetLanguageString('msMain_Version'), LocalStatus.ProgramVersion);
  AddDetailsItem(GetLanguageString('msMain_DateBuilt'), DateTimeToStr(GetLastModified(ParamStr(0))));
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetLanguageString('msMain_GameMode'), wbGameName);
  AddDetailsItem(GetLanguageString('msMain_Language'), settings.language);
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetLanguageString('msMain_TimesRun'), IntToStr(statistics.timesRun + sessionStatistics.timesRun));
  AddDetailsItem(GetLanguageString('msMain_PatchesBuilt'), IntToStr(statistics.patchesBuilt + sessionStatistics.patchesBuilt));
  AddDetailsItem(GetLanguageString('msMain_PluginsSmashed'), IntToStr(statistics.pluginsPatched + sessionStatistics.pluginsPatched));
  AddDetailsItem(GetLanguageString('msMain_SettingsSubmitted'), IntToStr(statistics.settingsSubmitted + sessionStatistics.settingsSubmitted));
  AddDetailsItem(GetLanguageString('msMain_RecsSubmitted'), IntToStr(statistics.recsSubmitted + sessionStatistics.recsSubmitted));
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetLanguageString('msMain_Website'), '-');
  AddDetailsItem(GetLanguageString('msMain_ApiCredits'), 'superobject, TurboPower Abbrevia, xEdit');
  AddDetailsItem(GetLanguageString('msMain_xEditVersion'), xEditVersion);
  AddDetailsItem(GetLanguageString('msMain_xEditCredits'), 'zilav, hlp, Sharlikran, ElminsterAU');
  AddDetailsItem(GetLanguageString('msMain_Testers'), ProgramTesters);
  AddDetailsItem(GetLanguageString('msMain_Translators'), ProgramTranslators);

  // update gui
  StringGrid_CorrectWidth(DetailsGrid);
  DetailsGrid.RowCount := slDetails.Count;
  DetailsGrid.Repaint;
end;

procedure TSmashForm.DetailsCopyToClipboardItemClick(Sender: TObject);
var
  i: Integer;
  name, value, previousName, previousValue: string;
  sl: TStringList;
begin
  sl := TStringList.Create;

  // build stringlist of formatted name value pairs with special formatting for
  // empty names and empty values
  name := ' ';
  value := ' ';
  for i := 0 to Pred(slDetails.Count) do begin
    previousName := name;
    name := slDetails.Names[i];
    previousValue := value;
    value := slDetails.ValueFromIndex[i];
    if (name <> ' ') then
      sl.Add(Format('%s: %s', [name, value]))
    else if (value <> ' ') then begin
      if (previousName <> ' ') then begin
        sl[sl.Count - 1] := previousName + ':';
        sl.Add('- '+previousValue);
      end;
      sl.Add('- '+value);
    end
    else
      sl.Add(' ');
  end;

  // copy to clipboard
  Clipboard.AsText := sl.Text;
  sl.Free;
end;

{ Resize StringGrid columns when parent panel changes size }
procedure TSmashForm.DetailsPanelResize(Sender: TObject);
begin
  StringGrid_CorrectWidth(DetailsGrid);
end;

{ Make cursor pointer if mouse is over a URL }
procedure TSmashForm.DetailsGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ACol, ARow: integer;
  value: string;
begin
  // don't change cursor if in help mode
  if Screen.Cursor = crHelp then
    exit;

  DetailsGrid.MouseToCell(X, Y, ACol, ARow);
  // use default cursor on cells in column 0, or at an invalid cell
  if (ACol = 0) or (ARow > Pred(slDetails.Count)) then begin
    Screen.Cursor := crDefault;
    exit;
  end;

  // test if cell is a url
  value := slDetails.ValueFromIndex[ARow];
  if IsURL(value) then
    Screen.Cursor := crHandPoint
  else
    Screen.Cursor := crDefault;
end;

{ Handle user clicking URL }
procedure TSmashForm.DetailsGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: integer;
  value: string;
begin
  // only process left clicks
  if Button <> mbLeft then
    exit;

  DetailsGrid.MouseToCell(X, Y, ACol, ARow);
  // skip clicks on cells in column 0
  if ACol = 0 then
    exit;

  try
    value := slDetails.ValueFromIndex[ARow];
    if IsURL(value) and ((Now - LastURLTime) * 86400 > 1.0) then begin
      ShellExecute(0, 'open', PChar(value), '', '', SW_SHOWNORMAL);
      LastURLTime := Now;
    end;
  except
    // invalid cell
  end;
end;

{ Handle drawing of a cell }
procedure TSmashForm.DetailsGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  sText, sNextVal, sNextName: string;
  iHalfBottom, iPadding: Integer;
begin
  // initialize stuff
  sText := ' ';
  iPadding := (Rect.Bottom - Rect.Top) - DetailsGrid.Canvas.TextHeight('Hg');
  iHalfBottom := Rect.Top + (Rect.Bottom - Rect.Top) div 2;
  DetailsGrid.Font.Style := [];
  DetailsGrid.Font.Color := clBlack;

  // draw name
  if ACol = 0 then begin
    DetailsGrid.Canvas.Brush.Color := TColor($00f6f4f3);
    DetailsGrid.Canvas.Rectangle(Rect);
    DetailsGrid.Canvas.Brush.Color := clWindow;
    DetailsGrid.Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, iHalfBottom);

    if Assigned(slDetails) and (slDetails.Count > ARow) then
      sText := slDetails.Names[ARow];
    // handle lists
    sText := RemoveFromEnd(sText, '[0]');
    if StrEndsWith(sText, ']') then
      exit;

    DetailsGrid.Canvas.Brush.Style := bsClear;
    DetailsGrid.Canvas.TextOut(Rect.Left + 4, Rect.Top + (iPadding div 2), sText);
  end
  // draw value
  else if ACol = 1 then begin
    DetailsGrid.Canvas.Brush.Color := clWindow;
    DetailsGrid.Canvas.Rectangle(Rect);

    if Assigned(slDetails) and (slDetails.Count > ARow) then
      sText := slDetails.ValueFromIndex[ARow];
    // handle special drawing of urls and master files
    if (Pred(slDetails.Count) > ARow) then begin
      sNextVal := slDetails.ValueFromIndex[ARow + 1];
      sNextName := slDetails.Names[ARow + 1];
      // urls blue and underlined
      if IsURL(sNextVal) then begin
        DetailsGrid.Font.Style := [fsUnderline];
        DetailsGrid.Font.Color := clBlue;
      end
      // esps and esms red if not loaded
      else if Pos('Plugins[', sNextName) = 1 then begin
        if not Assigned(PluginByFileName(sNextVal)) then
          DetailsGrid.Font.Color := clRed;
      end;
    end;

    // draw text
    DetailsGrid.Canvas.Brush.Style := bsClear;
    DetailsGrid.Canvas.TextOut(Rect.Left + 4, Rect.Top + (iPadding div 2), sText);
  end;
end;

{******************************************************************************}
{ PluginsListView Events
  Events involving the PluginsListView control.  Events include:
  - UpdatePluginDetails
  - PluginsListViewChange
  - PluginsListViewData
  - FlagNotSafe
  - DrawFlag
  - DrawPluginFlags
  - PluginsListViewDrawItem
  - PluginsListViewMouseMove
}
{******************************************************************************}

procedure TSmashForm.UpdatePluginDetails;
var
  plugin: TPlugin;
  index: integer;
begin
  // don't do anything if no item selected
  if not Assigned(PluginsListView.Selected) then
    exit;

  // prepare list view for plugin information
  slDetails.Clear;
  DetailsLabel.Caption := GetLanguageString('msMain_PluginDetails');

  // get plugin information
  index := PluginsListView.ItemIndex;
  plugin := TPlugin(PluginsList[index]);
  if not plugin.hasData then plugin.GetData(PluginsList);

  // add details items
  AddDetailsItem(GetLanguageString('msMain_Filename'), plugin.filename);
  AddDetailsItem(GetLanguageString('msMain_Hash'), '$' + plugin.hash);
  AddDetailsItem(GetLanguageString('msMain_FileSize'), FormatByteSize(plugin.fileSize));
  AddDetailsItem(GetLanguageString('msMain_DateModified'), plugin.dateModified);
  AddDetailsItem(GetLanguageString('msMain_NumRecords'), IntToStr(plugin.numRecords));
  AddDetailsItem(GetLanguageString('msMain_NumOverrides'), IntToStr(plugin.numOverrides));
  AddDetailsItem(GetLanguageString('msMain_Author'), plugin.author);
  AddDetailsList(GetLanguageString('msMain_Description'), plugin.description);
  AddDetailsList(GetLanguageString('msMain_Masters'), plugin.masters);

  // update gui
  StringGrid_CorrectWidth(DetailsGrid);
  DetailsGrid.RowCount := slDetails.Count;
  DetailsGrid.Repaint;
end;

procedure TSmashForm.ChangePatchSetting(aSetting: TSmashSetting);
var
  i: Integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  // loop through plugins list, adding selected plugins to patch
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    plugin := TPlugin(PluginsList[i]);
    plugin.setting := aSetting.name;
    plugin.smashSetting := aSetting;
  end;

  // update and repaint
  UpdatePatches;
  UpdateListViews;
  UpdateQuickbar;
  UpdateStatusBar;
end;

procedure TSmashForm.AddPluginsToPatch(var patch: TPatch);
var
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  // loop through plugins list, adding selected plugins to patch
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    plugin := TPlugin(PluginsList[i]);
    if plugin.patch <> ' ' then
      continue;
    // add plugin to patch
    Logger.Write('PLUGIN', 'Patch', 'Added '+plugin.filename+' to patch '+patch.name);
    if not plugin.hasData then
      plugin.GetData(PluginsList);
    patch.plugins.AddObject(plugin.filename, TObject(i));
    plugin.patch := patch.name;
  end;

  // update and repaint
  UpdatePatches;
  UpdateListViews;
  UpdateQuickbar;
  UpdateStatusBar;
end;

procedure TSmashForm.PluginsListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdatePluginDetails;
end;

procedure TSmashForm.PluginsListViewData(Sender: TObject; Item: TListItem);
var
  plugin: TPlugin;
begin
  if Item.Index > Pred(PluginsList.Count) then
    exit;
  plugin := TPlugin(PluginsList[Item.Index]);
  Item.Caption := IntToHex(Item.Index, 2);
  Item.SubItems.Add(plugin.filename);
  Item.SubItems.Add(plugin.setting);
  Item.SubItems.Add(plugin.patch);
  if Assigned(plugin.smashSetting) then
    PluginsListView.Canvas.Font.Color := plugin.smashSetting.color
  else
    PluginsListView.Canvas.Font.Color := clGray;
  PluginsListView.Canvas.Font.Style := PluginsListView.Canvas.Font.Style + [fsBold];
end;

procedure TSmashForm.PluginsListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i, x, y: integer;
  ListView: TListView;
  R: TRect;
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
    // fixes drawing bug
    R.Right := R.Left + ListView_GetColumnWidth(ListView.Handle, ListView.Columns[i + 1].Index);
    x := R.Left;
    ListView.Canvas.TextRect(R, x, y, Item.SubItems[i]);
  end;
end;

{******************************************************************************}
{ Plugins Popup Menu methods
  Methods for dealing with the popup menu for the PluginsListView.
  - PluginsPopupMenuPopup
  - UpdatePluginsPopupMenu
  - AddToPatchClick
  - AddToNewPatchClick
  - CheckForErrorsClick
  - RemoveFromPatchClick
}
{******************************************************************************}

procedure TSmashForm.PluginsPopupMenuPopup(Sender: TObject);
var
  i: integer;
  bPluginInPatch, bHasSelection, bAllPluginsInPatch, bAllHaveTags: boolean;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  // initialize selection booleans
  bHasSelection := false;
  bPluginInPatch := false;
  bAllPluginsInPatch := true;
  bAllHaveTags := true;

  // loop through selection
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    plugin := PluginsList[i];
    bHasSelection := true;
    bAllHaveTags := bAllHaveTags and plugin.HasTags;
    bPluginInPatch := bPluginInPatch or plugin.IsInPatch;
    bAllPluginsInPatch := bAllPluginsInPatch and plugin.IsInPatch;
  end;

  // disable/enable menu items
  AddToPatchItem.Enabled := bHasSelection and not bPluginInPatch;
  SmashSettingItem.Enabled := bHasSelection;
  OpenPluginLocationItem.Enabled := bHasSelection;
  RemoveFromPatchItem.Enabled := bHasSelection and bAllPluginsInPatch;
  TagsItem.Enabled := bHasSelection;
  ClearTagsItem.Enabled := bHasSelection and bAllHaveTags;
end;

procedure TSmashForm.UpdatePluginsPopupMenu;
var
  i, index: Integer;
  patch: TPatch;
  aSetting: TSmashSetting;
  MenuItem, GroupItem: TMenuItem;
  sGroup: String;
begin
  // clear submenus
  AddToPatchItem.Clear;
  SmashSettingItem.Clear;

  // add <New Patch> option to Plugins popup menu
  MenuItem := TMenuItem.Create(AddToPatchItem);
  MenuItem.Caption := GetLanguageString('msMain_NewPatchItem_Caption');
  MenuItem.OnClick := AddToNewPatchClick;
  AddToPatchItem.Add(MenuItem);

  // add patches to submenu
  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    MenuItem := TMenuItem.Create(AddToPatchItem);
    MenuItem.Caption := patch.name;
    MenuItem.OnClick := AddToPatchClick;
    AddToPatchItem.Add(MenuItem);
  end;

  // add smash settings to submenu
  for i := 0 to Pred(SmashSettings.Count) do begin
    aSetting := TSmashSetting(SmashSettings[i]);

    // parse setting group
    sGroup := 'Ungrouped';
    index := Pos('.', aSetting.name);
    if (index > 0) and (index < 11) then
      sGroup := Copy(aSetting.name, 1, index - 1);

    // get group menu item or create it if missing
    GroupItem := SmashSettingItem.Find(sGroup);
    if not Assigned(GroupItem) then begin
      GroupItem := TMenuItem.Create(SmashSettingItem);
      GroupItem.Caption := sGroup;
      SmashSettingItem.Add(GroupItem);
    end;

    // create MenuItem
    MenuItem := TMenuItem.Create(GroupItem);
    MenuItem.Caption := aSetting.name;
    MenuItem.OnClick := ChangeSettingClick;
    GroupItem.Add(MenuItem);
  end;
end;

procedure TSmashForm.AddToPatchClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  patch: TPatch;
  name: String;
begin
  MenuItem := TMenuItem(Sender);
  name := StripHotkey(MenuItem.Caption);
  patch := TPatchHelpers.PatchByName(PatchesList, name);
  if Assigned(patch) then
    AddPluginsToPatch(patch)
  else begin
    ToggleFormState(false);
    ShowMessage('Error: Could not find patch '+name);
    ToggleFormState(true);
  end;

end;

procedure TSmashForm.ChangeSettingClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  aSetting: TSmashSetting;
  name: String;
begin
  MenuItem := TMenuItem(Sender);
  name := StripHotkey(MenuItem.Caption);
  aSetting := TSettingHelpers.SettingByName(name);
  if Assigned(aSetting) then
    ChangePatchSetting(aSetting)
  else begin
    ToggleFormState(false);
    ShowMessage('Error: Could not find setting '+name);
    ToggleFormState(true);
  end;
end;

procedure TSmashForm.AddToNewPatchClick(Sender: TObject);
var
  patch: TPatch;
begin
  patch := NewPatch;
  if Assigned(patch) then
    AddPluginsToPatch(patch);
end;

procedure TSmashForm.ManageTagsItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  tmForm: TTagManager;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    // only process selected list items
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    // create a tag manager instance for the plugin
    plugin := TPlugin(PluginsList[i]);
    tmForm := TTagManager.Create(self);
    try
      tmForm.plugin := plugin;
      tmForm.ShowModal;
    finally
      tmForm.Free;
    end;
  end;

  // update
  UpdatePatches;
  UpdateListViews;
  UpdateQuickbar;
  UpdateStatusBar;
end;

procedure TSmashForm.ApplySettingTagsItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    // only process selected list items
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    // create a tag manager instance for the plugin
    plugin := TPlugin(PluginsList[i]);
    plugin.ApplySettingTags;
  end;

  // update
  UpdatePatches;
  UpdateListViews;
  UpdateQuickbar;
  UpdateStatusBar;
end;

procedure TSmashForm.ClearTagsItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  pluginNames: String;
  plugin: TPlugin;
  pluginsToClear: TList;
  bApproved: Boolean;
  frmDialog: TForm;
begin
  // initialize variables
  bApproved := false;
  pluginsToClear := TList.Create;
  pluginNames := '';

  // use a try-finally to make certain the list is freed
  try
    // add selected list items to the list
    for i := 0 to Pred(PluginsListView.Items.Count) do begin
      ListItem := PluginsListView.Items[i];
      if ListItem.Selected then begin
        plugin := TPlugin(PluginsList[i]);
        pluginsToClear.Add(plugin);
        pluginNames := pluginNames + #13#10'    - ' + plugin.filename;
      end;
    end;

    // prompt user if a merge was selected
    if pluginsToClear.Count > 0 then begin
      frmDialog := CreateMessageDialog(GetLanguageString('msMain_ClearTags') +
        pluginNames, mtConfirmation, mbOKCancel, mbOk);
      frmDialog.PopupParent := self;
      ToggleFormState(false);
      bApproved := frmDialog.ShowModal = mrOk;
      ToggleFormState(true);
    end;

    // exit if user didn't approve clearing tags
    if not bApproved then
      exit;

    // clear tags on plugins in the list
    for i := 0 to Pred(pluginsToClear.Count) do begin
      plugin := TPlugin(pluginsToClear[i]);
      Logger.Write('PLUGIN', 'Tags', 'Clearing tags on '+plugin.filename);
      plugin.description.Text := ClearTags(plugin.description.Text);
      plugin.GetSettingTag;
      plugin.WriteDescription;
      // plugin.Save;
    end;

    // update
    UpdatePatches;
    UpdateListViews;
    UpdateQuickbar;
    UpdateStatusBar;
  finally
    pluginsToClear.Free;
  end;
end;

{ Remove from Patch }
procedure TSmashForm.RemoveFromPatchItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  pluginName, patchName: string;
  patch: TPatch;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    // only process selected list items
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    // get plugin associated with patch item and remove it from patch
    plugin := TPlugin(PluginsList[i]);
    pluginName := plugin.filename;
    patchName := plugin.patch;
    if patchName <> ' ' then begin
      patch := TPatchHelpers.PatchByName(PatchesList, patchName);
      if Assigned(patch) then
        patch.plugins.Delete(patch.plugins.IndexOf(pluginName));
    end;
    plugin.patch := ' ';
  end;

  // update
  UpdatePatches;
  UpdateListViews;
  UpdateQuickbar;
  UpdateStatusBar;
end;

procedure TSmashForm.OpenPluginLocationItemClick(Sender: TObject);
var
  i: integer;
  listItem: TListItem;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    // only process selected list items
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    // open plugin location in explorer if it exists
    plugin := TPlugin(PluginsList[i]);
    if DirectoryExists(plugin.dataPath) then
      ShellExecute(0, 'open', PChar(plugin.dataPath), '', '', SW_SHOWNORMAL);
  end;
end;

{******************************************************************************}
{ Patch List View Events
  Events involving the PatchesListView control.  Events include:
  - UpdatePatchDetails
  - UpdatePatches
  - PatchesListViewChange
  - PatchesListViewData
  - PatchesListViewDrawItem
  - SavePatchEdit
}
{******************************************************************************}

procedure TSmashForm.UpdatePatchDetails;
var
  patchItem: TListItem;
  patch: TPatch;
begin
  // don't do anything if no item selected
  patchItem := PatchesListView.Selected;
  if not Assigned(patchItem) then
    exit;

  // prepare list view for patch information
  slDetails.Clear;
  DetailsLabel.Caption := GetLanguageString('msMain_PatchDetails');

  // get patch information
  patch := PatchesList[PatchesListView.ItemIndex];
  AddDetailsItem(GetLanguageString('msMain_Status'), StatusArray[Ord(patch.status)].desc);
  AddDetailsItem(GetLanguageString('msMain_PatchName'), patch.name);
  AddDetailsItem(GetLanguageString('msMain_Filename'), patch.filename);
  AddDetailsItem(GetLanguageString('msMain_PluginCount'), IntToStr(patch.plugins.Count));
  AddDetailsItem(GetLanguageString('msMain_DateBuilt'), DateBuiltString(patch.dateBuilt));
  AddDetailsList(GetLanguageString('msMain_Plugins'), patch.plugins);
  AddDetailsItem(' ', ' ');
  if patch.fails.Count < 250 then
    AddDetailsList(GetLanguageString('msMain_Fails'), patch.fails)
  else
    AddDetailsItem(GetLanguageString('msMain_Fails'), GetLanguageString('msMain_TooManyFails'));

  // update gui
  StringGrid_CorrectWidth(DetailsGrid);
  DetailsGrid.RowCount := slDetails.Count;
  DetailsGrid.Repaint;
end;

procedure TSmashForm.UpdatePatches;
var
  i: integer;
  patch: TPatch;
begin
  // update patch count
  PatchesListView.Items.Count := PatchesList.Count;

  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    // sort plugins in patch
    patch.SortPlugins;
    // get status of each patch
    if not ((patch.status in ForcedStatuses)
    or (patch.status in FailedStatuses)) then
      patch.GetStatus;
  end;
end;

function TSmashForm.NewPatch: TPatch;
var
  patch: TPatch;
  EditPatch: TEditForm;
begin
  Result := nil;
  patch := TPatchHelpers.CreateNewPatch(PatchesList);

  // edit patch immediately after its creation
  EditPatch := TEditForm.Create(Self);
  EditPatch.patch := patch;
  if EditPatch.ShowModal = mrOk then begin
    patch := EditPatch.patch;
    LogMessage('PATCH', 'New', 'Created new patch '+patch.name);
    // add patch to list and update views
    PatchesList.Add(patch);
    UpdatePatches;
    PatchesListView.Repaint;
    UpdatePluginsPopupMenu;
    // set result
    Result := patch;
  end;

  // add and update patch
  EditPatch.Free;
end;

procedure TSmashForm.PatchesListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdatePatchDetails;
end;

procedure TSmashForm.PatchesListViewData(Sender: TObject; Item: TListItem);
var
  patch: TPatch;
begin
  if Item.Index > Pred(PatchesList.Count) then
    exit;
  patch := TPatch(PatchesList[Item.Index]);
  Item.Caption := IntToHex(Item.Index, 2);
  Item.SubItems.Add(patch.name);
  Item.SubItems.Add(patch.filename);
  Item.SubItems.Add(IntToStr(patch.plugins.count));
  Item.SubItems.Add(DateBuiltString(patch.dateBuilt));
  PatchesListView.Canvas.Font.Color := StatusArray[Ord(patch.status)].color;
  PatchesListView.Canvas.Font.Style := PatchesListView.Canvas.Font.Style + [fsBold];
end;

procedure TSmashForm.PatchesListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i, x, y: integer;
  ListView: TListView;
  R: TRect;
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
{ LogListView methods
}
{******************************************************************************}

procedure TSmashForm.LogListViewData(Sender: TObject; Item: TListItem);
var
  msg: TLogMessage;
begin
  if (Item.Index > Pred(Log.Count)) then
    exit;
  msg := TLogMessage(Log[Item.Index]);
  Item.Caption := msg.time;
  Item.SubItems.Add(msg.appTime);
  Item.SubItems.Add(msg.group);
  Item.SubItems.Add(msg.&label);
  Item.SubItems.Add(msg.text);

  // handle coloring
  if (msg.group = 'GENERAL') then
    LogListView.Canvas.Font.Color := settings.generalMessageColor
  else if (msg.group = 'LOAD') then
    LogListView.Canvas.Font.Color := settings.loadMessageColor
  else if (msg.group = 'CLIENT') then
    LogListView.Canvas.Font.Color := settings.clientMessageColor
  else if (msg.group = 'PATCH') then
    LogListView.Canvas.Font.Color := settings.patchMessageColor
  else if (msg.group = 'PLUGIN') then
    LogListView.Canvas.Font.Color := settings.pluginMessageColor
  else if (msg.group = 'ERROR') then
    LogListView.Canvas.Font.Color := settings.errorMessageColor;
end;

procedure TSmashForm.LogListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i, x, y: integer;
  ListView: TListView;
  R: TRect;
  msg: string;
  map: TStringList;
begin
  ListView := TListView(Sender);
  if Item.Selected then begin
    ListView.Canvas.Brush.Color := $FFEEDD;
    ListView.Canvas.FillRect(Rect);
  end;

  // prepare map
  map := TStringList.Create;
  map.Values[ListView.Columns[0].Caption] := Item.Caption;
  for i := 0 to Pred(Item.SubItems.Count) do
    map.Values[ListView.Columns[i + 1].Caption] := Item.SubItems[i];

  // prepare text rect
  R := Rect;
  R.Right := R.Left + ListView.Width - 3;
  x := Rect.Left + 3;
  y := (Rect.Bottom - Rect.Top - ListView.Canvas.TextHeight('Hg')) div 2 + Rect.Top;

  // draw message
  msg := ApplyTemplate(settings.logMessageTemplate, map);
  ListView.Canvas.TextRect(R, x, y, msg);

  // clean up
  map.Free;
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
  Result := IfThen(not b, GetLanguageString('msMain_Enable'), GetLanguageString('msMain_Disable'));
end;

procedure TSmashForm.LogPopupMenuPopup(Sender: TObject);
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
    item.Caption := Format('%s %s, %s', [EnableStr(filter.enabled), filter.group, filter.&label]);
    item.OnClick := ToggleLabelFilter;
    FilterLabelItem.Add(item);
  end;

  // toggle copy to clipboard item based on whether or not log items are selected
  CopyToClipboardItem.Enabled := Assigned(LogListView.Selected);

  // rename toggle auto scroll item based on whether or not auto scroll is enabled
  ToggleAutoScrollItem.Caption := Format('%s %s', [EnableStr(bAutoScroll), GetLanguageString('msMain_AutoScroll')]);
end;

// toggles a group filter for the LogListView
procedure TSmashForm.ToggleGroupFilter(Sender: TObject);
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
  ListView_CorrectWidth(LogListView);
end;

// toggles a label filter for the LogListView
procedure TSmashForm.ToggleLabelFilter(Sender: TObject);
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
  ListView_CorrectWidth(LogListView);
end;

// toggles auto scroll for the LogListView
procedure TSmashForm.ToggleAutoScrollItemClick(Sender: TObject);
begin
  bAutoScroll := not bAutoScroll;
end;

procedure TSmashForm.CopyToClipboardItemClick(Sender: TObject);
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

procedure TSmashForm.SaveAndClearItemClick(Sender: TObject);
begin
  SaveLog(BaseLog);
  LogListView.Items.Count := 0;
  BaseLog.Clear;
  Log.Clear;
  LogMessage('GENERAL', 'Log', 'Saved and cleared log.');
end;


{******************************************************************************}
{ PatchesPopupMenu methods
  Methods for dealing with the popup menu for the PatchesListView.
  - PatchesPopupMenuPopup
  - EditPatchItemClick
  - CheckPluginsForErrorsItemClick
  - DeletePatchItemClick
  - RebuildPatchItemClick
  - ReportOnPatchItemClick
  - OpenInExplorerItemClick
  - ForceRebuildItemClick
  - IgnoreRebuildItemClick
  - PatchesListViewDblClick
  - PatchesListViewKeyDown
}
{******************************************************************************}

procedure TSmashForm.PatchesPopupMenuPopup(Sender: TObject);
var
  bNeverBuilt, bHasBuildStatus, bHasUpToDateStatus, bHasSelection,
  bIsNotTop, bIsNotBottom: boolean;
  patch: TPatch;
  i, patchesSelected: Integer;
  sBuild, sRebuild: string;
begin
  bNeverBuilt := false;
  bHasBuildStatus := false;
  bHasUpToDateStatus := false;
  bIsNotTop := true;
  bIsNotBottom := true;
  patchesSelected := 0;

  // loop through list view to find selection
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);
    Inc(patchesSelected);
    // update booleans
    if i = 0 then bIsNotTop := false;
    if i = Pred(PatchesList.Count) then bIsNotBottom := false;
    bNeverBuilt := bNeverBuilt or (patch.dateBuilt = 0);
    bHasBuildStatus := bHasBuildStatus or (patch.status in BuildStatuses);
    bHasUpToDateStatus := bHasUpToDateStatus or (patch.status in UpToDateStatuses);
  end;

  bHasSelection := (patchesSelected > 0);
  // change enabled state of MergesPopupMenu items based on booleans
  EditPatchItem.Enabled := bHasSelection;
  DeletePatchItem.Enabled := bHasSelection;
  BuildPatchItem.Enabled := bHasSelection and bHasBuildStatus and wbLoaderDone;
  ToggleRebuildItem.Enabled := bHasSelection and not bNeverBuilt and
    (bHasUpToDateStatus or bHasBuildStatus);
  OpenInExplorerItem.Enabled := bHasSelection;
  // move submenu
  MoveItem.Enabled := bHasSelection;
  UpItem.Enabled := bHasSelection and bIsNotTop;
  DownItem.Enabled := bHasSelection and bIsNotBottom;
  ToTopItem.Enabled := bHasSelection and bIsNotTop;
  ToBottomItem.Enabled := bHasSelection and bIsNotBottom;

  // one or multiple patchs?
  if (patchesSelected = 1) then begin
    sBuild := 'msMain_BuildPatch';
    sRebuild := 'msMain_RebuildPatch';
  end
  else begin
    sBuild := 'msMain_BuildPatches';
    sRebuild := 'msMain_RebuildPatches';
  end;
  // handle build patchs menu item
  if bNeverBuilt then
    BuildPatchItem.Caption := GetLanguageString(sBuild)
  else if bHasBuildStatus then
    BuildPatchItem.Caption := GetLanguageString(sRebuild)
  else begin
    BuildPatchItem.Enabled := false;
    BuildPatchItem.Caption := GetLanguageString(sRebuild);
  end;
end;

procedure TSmashForm.EditPatchItemClick(Sender: TObject);
var
  EditPatch: TEditForm;
  i, j: integer;
  plugin: TPlugin;
  patch: TPatch;
begin
  // loop through patches
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);
    Logger.Write('PATCH', 'Edit', 'Editing '+patch.name);
    // create EditForm
    EditPatch := TEditForm.Create(Self);
    EditPatch.patch := patch;
    if EditPatch.ShowModal = mrOk then begin
      patch := EditPatch.patch;
      // update plugin.patch properties
      for j := 0 to Pred(patch.plugins.Count) do begin
        plugin := PluginByFilename(patch.plugins[j]);
        if Assigned(plugin) then
          plugin.patch := patch.name;
      end;
    end;

    // free and repaint
    EditPatch.Free;
    PatchesListView.Repaint;
  end;

  // update patch details and popup menu
  UpdatePatchDetails;
  UpdatePluginsPopupMenu;
end;

procedure TSmashForm.UpItemClick(Sender: TObject);
var
  i, max: Integer;
begin
  max := Pred(PatchesListView.Items.Count);
  // if patch at index 0 is selected, exit
  // we can't move it up!
  if PatchesListView.Items[0].Selected then
    exit;

  // loop through patches
  for i := 0 to max do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    PatchesList.Move(i, i - 1);
    PatchesListView.Items[i].Selected := false;
    PatchesListView.Items[i - 1].Selected := true;
  end;

  // update gui
  UpdateListViews;
end;

procedure TSmashForm.DownItemClick(Sender: TObject);
var
  i, max: Integer;
begin
  max := Pred(PatchesListView.Items.Count);
  // if patch at max index is selected, exit
  // we can't move it down!
  if PatchesListView.Items[max].Selected then
    exit;

  // loop through patches in reverse so we don't move the same patch
  // multiple times
  for i := max downto 0 do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    PatchesList.Move(i, i + 1);
    PatchesListView.Items[i].Selected := false;
    PatchesListView.Items[i + 1].Selected := true;
  end;

  // update gui
  UpdateListViews;
end;

procedure TSmashForm.ToTopItemClick(Sender: TObject);
var
  i, max, iIndex: Integer;
  tempList: TList;
begin
  max := Pred(PatchesListView.Items.Count);
  // if patch at index 0 is selected, exit
  // we can't move it up!
  if PatchesListView.Items[0].Selected then
    exit;

  // create tempList
  tempList := TList.Create;

  // loop through patches to build new list
  iIndex := 0;
  for i := 0 to max do begin
    if not PatchesListView.Items[i].Selected then begin
      tempList.Add(PatchesList[i]);
    end
    else begin
      tempList.Insert(iIndex, PatchesList[i]);
      Inc(iIndex);
    end;
  end;

  // set PatchesList to tempList
  PatchesList.Clear;
  for i := 0 to max do PatchesList.Add(tempList[i]);
  tempList.Free;

  // update selection
  for i := 0 to max do
    PatchesListView.Items[i].Selected := i < iIndex;

  // update gui
  UpdateListViews;
end;

procedure TSmashForm.ToBottomItemClick(Sender: TObject);
var
  i, max, iIndex: Integer;
  tempList: TList;
begin
  max := Pred(PatchesListView.Items.Count);
  // if patch at max index is selected, exit
  // we can't move it down!
  if PatchesListView.Items[max].Selected then
    exit;

  // create tempList
  tempList := TList.Create;

  // loop through patches to build new list
  iIndex := 0;
  for i := 0 to max do begin
    if not PatchesListView.Items[i].Selected then begin
      tempList.Insert(iIndex, PatchesList[i]);
      Inc(iIndex);
    end
    else begin
      tempList.Add(PatchesList[i]);
    end;
  end;

  // set PatchesList to tempList
  PatchesList.Clear;
  for i := 0 to max do PatchesList.Add(tempList[i]);
  tempList.Free;

  // update selection
  for i := 0 to max do
    PatchesListView.Items[i].Selected := i >= iIndex;

  // update gui
  UpdateListViews;
end;

{ Remove unloaded plugins and plugins with errors }
procedure TSmashForm.RemovePluginsItemClick(Sender: TObject);
var
  i, j: integer;
  plugin: TPlugin;
  patch: TPatch;
begin
  // loop through patches
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);
    Logger.Write('PATCH', 'Plugins', 'Removing plugins from '+patch.name);
    // remove plugins that aren't loaded or have errors
    for j := Pred(patch.plugins.Count) downto 0 do begin
      plugin := PluginByFilename(patch.plugins[j]);
      if not Assigned(plugin) then begin
        Logger.Write('PATCH', 'Plugins', 'Removing '+patch.plugins[j]+', plugin not loaded');
        patch.plugins.Delete(j);
        continue;
      end;
    end;
  end;

  // update
  UpdatePatches;
  UpdateListViews;
end;

procedure TSmashForm.RemoveUnloadedPluginsItemClick(Sender: TObject);
var
  i, j: integer;
  plugin: TPlugin;
  patch: TPatch;
begin
  // loop through patches
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);
    Logger.Write('PATCH', 'Plugins', 'Removing unloaded plugins from '+patch.name);
    // remove plugins that aren't loaded or have errors
    for j := Pred(patch.plugins.Count) downto 0 do begin
      plugin := PluginByFilename(patch.plugins[j]);
      if not Assigned(plugin) then begin
        Logger.Write('PATCH', 'Plugins', 'Removing '+patch.plugins[j]+', plugin not loaded');
        patch.plugins.Delete(j);
      end;
    end;
  end;

  // update
  UpdatePatches;
  UpdateListViews;
  UpdateQuickbar;
end;

procedure TSmashForm.DeletePatchItemClick(Sender: TObject);
var
  i, j: Integer;
  plugin: TPlugin;
  patch: TPatch;
  patchNames: string;
  bApproved: boolean;
  patchesToDelete: TList;
  frmDialog: TForm;
begin
  // see how many patches the user selected
  bApproved := false;
  patchesToDelete := TList.Create;
  patchNames := '';
  for i := 0 to Pred(PatchesListView.Items.Count) do
    if PatchesListView.Items[i].Selected then begin
      patch := TPatch(PatchesList[i]);
      patchesToDelete.Add(patch);
      PatchesListView.Items[i].Selected := false;
      patchNames := patchNames + #13#10'    - ' + patch.name;
    end;

  // show multi-patch prompt if multiple patches selected
  if patchesToDelete.Count > 0 then
    frmDialog := CreateMessageDialog(GetLanguageString('msMain_DeletePatches') + patchNames, mtConfirmation, mbOKCancel, mbOk);
    frmDialog.PopupParent := self;
    ToggleFormState(false);
    bApproved := frmDialog.ShowModal = mrOk;
    ToggleFormState(true);

  // exit if user didn't approve deletion
  if not bApproved then
    Exit;

  // clear details grid
  slDetails.Clear;

  // loop through patches
  for i := Pred(patchesToDelete.Count) downto 0 do begin
    patch := TPatch(patchesToDelete[i]);
    Logger.Write('PATCH', 'Delete', 'Deleting patch '+patch.name);
    PatchesListView.Items.Count := PatchesListView.Items.Count - 1;

    // remove patch from plugin patch properties
    for j := 0 to Pred(PluginsList.Count) do begin
      plugin := TPlugin(PluginsList[j]);
      if plugin.patch = patch.name then
        plugin.patch := ' ';
    end;

    // delete patch
    patchesToDelete.Delete(i);
    PatchesList.Delete(PatchesList.IndexOf(patch));
    patch.Free;
  end;

  // update patches
  UpdatePluginsPopupMenu;
  UpdateListViews;
  UpdateQuickbar;
  UpdateStatusBar;
end;

procedure TSmashForm.BuildPatchItemClick(Sender: TObject);
var
  timeCost, i: Integer;
  patch: TPatch;
begin
  timeCosts := TStringList.Create;
  patchesToBuild := TList.Create;

  // get timecosts
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);
    if not (patch.status in BuildStatuses) then
      continue;

    // else calculate time cost and build patch
    Logger.Write('PATCH', 'Build', 'Building '+patch.name);
    timeCost := patch.GetTimeCost * 2;
    timeCosts.Add(IntToStr(timeCost));
    patchesToBuild.Add(patch);
  end;

  // free and exit if no patches to check for errors
  if patchesToBuild.Count = 0 then begin
    timeCosts.Free;
    patchesToBuild.Free;
    exit;
  end;

  // Show progress form
  self.Enabled := false;
  xEditLogGroup := 'PATCH';
  pForm := TProgressForm.Create(Self);
  pForm.pfLogPath := LogPath + 'patch\';
  pForm.PopupParent := Self;
  pForm.Caption := GetLanguageString('msProg_Smashing');
  pForm.SetMaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pForm.Show;

  // start patch thread
  PatchCallback := ProgressDone;
  TPatchThread.Create;
end;

procedure TSmashForm.OpenInExplorerItemClick(Sender: TObject);
var
  i: Integer;
  path: string;
  patch: TPatch;
begin
  // loop through patches
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);

    // open in explorer
    path := patch.dataPath;
    ForceDirectories(path);
    ShellExecute(0, 'open', PChar(path), '', '', SW_SHOWNORMAL);
  end;
end;

procedure TSmashForm.ToggleRebuildItemClick(Sender: TObject);
var
  i: Integer;
  patch: TPatch;
begin
  // loop through patches
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);
    Logger.Write('PATCH', 'Status', 'Toggled rebuild status on '+patch.name);
    // if forced up to date, set to Ready to be rebuilt
    if patch.status = psUpToDateForced then
      patch.status := psRebuildReady
    // if normal up to date, set to Ready to rebuilt [forced]
    else if patch.status = psUpToDate then
      patch.Status := psRebuildReadyForced
    // if force rebuild, set to Up to date
    else if patch.status = psRebuildReadyForced then
      patch.status := psUpToDate
    // if normal rebuild, set to Up to date [Forced]
    else if patch.status = psRebuildReady then
      patch.Status := psUpToDateForced;
  end;

  // update
  UpdatePatches;
  UpdateListViews;
  UpdateQuickBar;
end;

procedure TSmashForm.ImageDisconnectedClick(Sender: TObject);
begin
  if (not TCPClient.Connected)
  and (ConnectionAttempts = MaxConnectionAttempts) then begin
    Logger.Write('CLIENT', 'Connect', 'Retrying connecting to the server.');
    ConnectionAttempts := 0;
  end;
end;

{ Double click to edit patch }
procedure TSmashForm.PatchesListViewDblClick(Sender: TObject);
begin
  EditPatchItemClick(nil);
end;

{ Shortcut to delete patches using the delete key }
procedure TSmashForm.PatchesListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if HiWord(GetKeyState(vk_Delete)) <> 0 then
    DeletePatchItemClick(nil);
end;

{******************************************************************************}
{ QuickBar Button Events
  Events involving buttons on the QuickBar.  Events include:
  - CreatePatchButtonClick
  - RebuildButtonClick
  - ReportButtonClick
  - OptionsButtonClick
  - DictionaryButtonClick
  - UpdateButtonClick
  - HelpButtonClick
}
{******************************************************************************}

procedure TSmashForm.UpdateQuickbar;
var
  i: Integer;
  patch: TPatch;
  sTitle: string;
begin
  // DISABLE ALL BUTTONS IF INITIALIZATION EXCEPTION
  if ProgramStatus.bInitException then begin
    NewButton.Enabled := false;
    BuildButton.Enabled := false;
    SubmitButton.Enabled := false;
    ManageButton.Enabled := false;
    DictionaryButton.Enabled := false;
    OptionsButton.Enabled := true;
    UpdateButton.Enabled := false;
    HelpButton.Enabled := false;
    exit;
  end;

  // BUILD BUTTON
  bPatchesToBuild := false;
  bPatchesToCheck := false;
  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    if (patch.status in BuildStatuses) then
      bPatchesToBuild := true;
  end;

  // enable build button if there are patches to build
  BuildButton.Enabled := bPatchesToBuild and wbLoaderDone;
  // swap hints
  sTitle := GetLanguageString('msMain_BuildButton_Hint');
  if not wbLoaderDone then
    BuildButton.Hint := sTitle + GetLanguageString('msMain_BuildPatches_Loader')
  else if not bPatchesToBuild then
    BuildButton.Hint := sTitle + GetLanguageString('msMain_NoPatches')
  else if bPatchesToCheck then
    BuildButton.Hint := sTitle + GetLanguageString('msMain_CheckPatches')
  else
    BuildButton.Hint := sTitle + GetLanguageString('msMain_BuildAllPatches');

  // UPDATE BUTTON
  UpdateButton.Enabled := ProgramStatus.bProgramUpdate or ProgramStatus.bDictionaryUpdate;
  sTitle := GetLanguageString('msMain_UpdateButton_Hint');
  if ProgramStatus.bProgramUpdate and ProgramStatus.bDictionaryUpdate then
    UpdateButton.Hint := sTitle + GetLanguageString('msMain_UpdateBoth')
  else if ProgramStatus.bProgramUpdate then
    UpdateButton.Hint := sTitle + GetLanguageString('msMain_UpdateProgram')
  else if ProgramStatus.bDictionaryUpdate then
    UpdateButton.Hint := sTitle + GetLanguageString('msMain_UpdateDictionary')
  else
    UpdateButton.Hint := sTitle + GetLanguageString('msMain_NoUpdates');

  // HELP BUTTON
  HelpButton.Enabled := false; // TODO: help file integration
end;

procedure TSmashForm.CreatePatchButtonClick(Sender: TObject);
begin
  NewPatch;
end;

procedure TSmashForm.BuildButtonClick(Sender: TObject);
var
  i, timeCost: Integer;
  patch: TPatch;
begin
  // exit if the loader isn't done
  if not wbLoaderDone then begin
    Logger.Write('ERROR', 'Patch', 'Loader not done, can''t patch yet!');
    exit;
  end;

  // exit if there are no patches
  if PatchesList.Count = 0 then begin
    Logger.Write('ERROR', 'Patch', 'There are no patches!');
    exit;
  end;

  // calculate time costs, prepare patches
  timeCosts := TStringList.Create;
  patchesToBuild := TList.Create;
  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    Logger.Write('PATCH', 'Build', 'Building '+patch.name);
    if not (patch.status in BuildStatuses) then
      continue;
    timeCost := patch.GetTimeCost;
    patchesToBuild.Add(patch);
    timeCosts.Add(IntToStr(timeCost));
  end;

  // exit if no patches to build
  if timeCosts.Count = 0 then begin
    Logger.Write('ERROR', 'Patch', 'No patches to build!');
    timeCosts.Free;
    patchesToBuild.Free;
    exit;
  end;

  // make and show progress form
  self.Enabled := false;
  xEditLogGroup := 'PATCH';
  pForm := TProgressForm.Create(Self);
  pForm.pfLogPath := LogPath + 'patch\';
  pForm.bDetailsVisible := false;
  pForm.PopupParent := Self;
  pForm.Caption := GetLanguageString('msProg_Smashing');
  pForm.SetMaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pForm.Show;

  // start patch thread
  PatchCallback := ProgressDone;
  TPatchThread.Create;
end;

{ Submit report }
procedure TSmashForm.SubmitButtonClick(Sender: TObject);
begin
  // ?
end;

{ Edit smash settings }
procedure TSmashForm.ManageButtonClick(Sender: TObject);
var
  smForm: TSettingsManager;
begin
  smForm := TSettingsManager.Create(self);
  smForm.ShowModal;
  smForm.Free;

  // update
  UpdatePluginsPopupMenu;
  UpdatePatches;
  UpdateListViews;
  UpdateQuickbar;
  UpdateStatusBar;
end;

{ View dictionary }
procedure TSmashForm.DictionaryButtonClick(Sender: TObject);
begin
  // ?
end;

{ Options }
procedure TSmashForm.OptionsButtonClick(Sender: TObject);
var
  OptionsForm: TOptionsForm;
  prevLanguage: string;
begin
  prevLanguage := settings.language;
  // Create and show options form
  OptionsForm := TOptionsForm.Create(Self);
  OptionsForm.ShowModal;
  OptionsForm.Free;

  // update owner draw if changed
  PluginsListView.OwnerDraw := not settings.simplePluginsView;

  // rebuild log because some messages may have been enabled/disabled
  RebuildLog;

  // initialize MO if usingMO changed
  if settings.usingMO then
    ModOrganizerInit;

  // if user changed language, update language displayed
  if settings.language <> prevLanguage then begin
    LoadLanguage;
    TRttiTranslation.Load(language, self);
  end;

  // update gui
  UpdatePatches;
  UpdateListViews;
  UpdateQuickBar;
  UpdateStatusBar;

  // if user selected to change game mode, close application
  if ProgramStatus.bChangeProfile then
    Close;

  // if user selected to update program, close application
  if ProgramStatus.bInstallUpdate then begin
    ProgramStatus.bInstallUpdate := UpdateProgram;
    if ProgramStatus.bInstallUpdate then
      Close;
  end;
end;

{ Update }
procedure TSmashForm.UpdateButtonClick(Sender: TObject);
begin
  // if not connected to server, don't try to update anything
  {if not TCPClient.Connected then
    exit;                                               

  // update program
  if bProgramUpdate and ChangeLogPrompt(self) and DownloadProgram then begin
    bInstallUpdate := UpdateProgram;
    if bInstallUpdate then
      Close;
  end;

  // update dictionary
  if bDictionaryUpdate and UpdateDictionary then begin
    status := TmpStatus.Create;
    CompareStatuses;
    UpdatePluginData;
    UpdateListViews;
  end; }
end;

{ Help }
procedure TSmashForm.HelpButtonClick(Sender: TObject);
begin
  //LogMessage(TButton(Sender).Hint+' clicked!');
end;

end.
