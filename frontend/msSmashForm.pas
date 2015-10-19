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
  mteHelpers, mteTracker, mteLogger, mteProgressForm, mteTaskHandler,
  RttiTranslation,
  // mp units
  msFrontend, msThreads, msOptionsForm,
  msSplashForm,
  // tes5edit units
  wbBSA, wbHelpers, wbInterface, wbImplementation;

type
  TPatchForm = class(TForm)
    [FormPrefix('mpMain')]
      XPManifest: TXPManifest;
      FlagList: TImageList;
    IconList: TImageList;
      TaskTimer: TTimer;
      [FormSection('QuickBar')]
        QuickBar: TPanel;
        NewButton: TSpeedButton;
        BuildButton: TSpeedButton;
    SubmitButton: TSpeedButton;
    SettingsButton: TSpeedButton;
        OptionsButton: TSpeedButton;
        UpdateButton: TSpeedButton;
        HelpButton: TSpeedButton;
        bhBuild: TBalloonHint;
        bhNew: TBalloonHint;
    bhSubmit: TBalloonHint;
    bhSet: TBalloonHint;
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
          DetailsEditor: TValueListEditor;
          DetailsPopupMenu: TPopupMenu;
          DetailsCopyToClipboardItem: TMenuItem;
        [FormSection('Plugins Tab')]
          PluginsTabSheet: TTabSheet;
          PluginsListView: TListView;
          [FormSection('Plugins Popup Menu')]
            PluginsPopupMenu: TPopupMenu;
            AddToPatchItem: TMenuItem;
            NewPatchItem: TMenuItem;
            RemoveFromPatchItem: TMenuItem;
            ReportOnPluginItem: TMenuItem;
            DoNotPatchItem: TMenuItem;
            OpenPluginLocationItem: TMenuItem;
            [FormSection('Errors Submenu')]
              ErrorsItem: TMenuItem;
              CheckForErrorsItem: TMenuItem;
              FixErrorsItem: TMenuItem;
              IgnoreErrorsItem: TMenuItem;
              ResetErrorsItem: TMenuItem;
        [FormSection('Patchs Tab')]
    PatchesTabSheet: TTabSheet;
    PatchesListView: TListView;
          [FormSection('Patchs Popup Menu')]
    PatchesPopupMenu: TPopupMenu;
            CreateNewPatchItem: TMenuItem;
            EditPatchItem: TMenuItem;
            DeletePatchItem: TMenuItem;
            BuildPatchItem: TMenuItem;
            ToggleRebuildItem: TMenuItem;
            OpenInExplorerItem: TMenuItem;
            [FormSection('Plugins Submenu')]
              PluginsItem: TMenuItem;
              ResolveIssuesItem: TMenuItem;
              CheckPluginsItem: TMenuItem;
              FixPluginsItem: TMenuItem;
              ReportOnPluginsItem: TMenuItem;
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
        StatusPanelPatchs: TPanel;
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

    // MERGE FORM EVENTS
    procedure UpdateLog;
    procedure LogMessage(const group, &label, text: string);
    procedure FormCreate(Sender: TObject);
    procedure InitDone;
    procedure FormShow(Sender: TObject);
    procedure LoaderStatus(s: string);
    procedure LoaderDone;
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
    function AddDetailsItem(name, value: string; editable: boolean = false):
      TItemProp;
    procedure AddDetailsList(name: string; sl: TStringList; editable: boolean = false);
    procedure PageControlChange(Sender: TObject);
    procedure UpdateApplicationDetails;
    procedure DetailsEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // PLUGINS LIST VIEW EVENTS
    procedure UpdatePluginDetails;
    procedure AddPluginsToPatch(var patch: TPatch);
    procedure PluginsListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure PluginsListViewData(Sender: TObject; Item: TListItem);
    procedure PluginsListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure PluginsListViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    // PLUGINS POPUP MENU EVENTS
    procedure PluginsPopupMenuPopup(Sender: TObject);
    procedure UpdatePluginsPopupMenu;
    procedure AddToNewPatchClick(Sender: TObject);
    procedure AddToPatchClick(Sender: TObject);
    procedure CheckForErrorsClick(Sender: TObject);
    procedure IgnoreErrorsItemClick(Sender: TObject);
    procedure DoNotPatchItemClick(Sender: TObject);
    procedure RemoveFromPatchItemClick(Sender: TObject);
    procedure OpenPluginLocationItemClick(Sender: TObject);
    procedure ReportOnPluginItemClick(Sender: TObject);
    procedure FixErrorsItemClick(Sender: TObject);
    procedure ResetErrorsItemClick(Sender: TObject);
    // MERGE LIST VIEW EVENTS
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
    // MERGES POPUP MENU EVENTS
    procedure PatchesPopupMenuPopup(Sender: TObject);
    procedure EditPatchItemClick(Sender: TObject);
    procedure BuildPatchItemClick(Sender: TObject);
    procedure CheckPluginsItemClick(Sender: TObject);
    procedure RemovePluginsItemClick(Sender: TObject);
    procedure ResolveIssuesItemClick(Sender: TObject);
    procedure DeletePatchItemClick(Sender: TObject);
    procedure ReportOnPluginsItemClick(Sender: TObject);
    procedure OpenInExplorerItemClick(Sender: TObject);
    procedure ToggleRebuildItemClick(Sender: TObject);
    procedure FixPluginsItemClick(Sender: TObject);
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
    procedure FindErrorsButtonClick(Sender: TObject);
    procedure SubmitButtonClick(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ToggleAutoScrollItemClick(Sender: TObject);
    procedure DetailsCopyToClipboardItemClick(Sender: TObject);
    procedure ImageDisconnectedClick(Sender: TObject);
  protected
    procedure WMSize(var AMessage: TMessage); message WM_SIZE;
    procedure WMMove(var AMessage: TMessage); message WM_MOVE;
    procedure WMActivateApp(var AMessage: TMessage); message WM_ACTIVATEAPP;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  MessageDelay = (0.1 / 86400.0);

var
  splash: TSplashForm;
  PatchForm: TPatchForm;
  LastHint: string;
  LastURLTime, LastMessageTime, FormDisplayTime: double;
  bPatchsToBuild, bPatchsToCheck, bAutoScroll, bCreated, bClosing: boolean;
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

procedure TPatchForm.UpdateLog;
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
    CorrectListViewWidth(LogListView);
end;

{ Prints a message to the log }
procedure TPatchForm.LogMessage(const group, &label, text: string);
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

{ Initialize form, initialize TES5Edit API, and load plugins }
procedure TPatchForm.FormCreate(Sender: TObject);
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

  // CREATE SPLASH
  splash := TSplashForm.Create(nil);
  try
    InitCallback := InitDone;
    UpdateCallback := AutoUpdate;
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

procedure TPatchForm.WMSize(var AMessage: TMessage);
begin
  if not bCreated then
    exit;
  if Now - LastMessageTime < MessageDelay then
    exit;
  LastMessageTime := Now;
  if (AMessage.WParam <> SIZE_MINIMIZED) then
    DisplayHints;
  inherited;
end;

procedure TPatchForm.WMMove(var AMessage: TMessage);
begin
  if not bCreated then
    exit;
  if Now - LastMessageTime < MessageDelay then
    exit;
  LastMessageTime := Now;
  DisplayHints;
  inherited;
end;

procedure TPatchForm.WMActivateApp(var AMessage: TMessage);
begin
  if not bCreated then
    exit;
  if Now - LastMessageTime < MessageDelay then
    exit;
  LastMessageTime := Now;
  if AMessage.WParam = 1 then
    DisplayHints
  else
    HideHints;
  inherited;
end;

procedure TPatchForm.InitDone;
begin
  splash.ModalResult := mrOk;
end;

// Force PluginsListView to autosize columns
procedure TPatchForm.FormShow(Sender: TObject);
begin
  // HANDLE AUTO-UPDATE
  if bInstallUpdate then begin
    Logger.Write('CLIENT', 'Disconnect', 'Disconnecting...');
    TCPClient.Disconnect;
    bAllowClose := true;
    bClosing := true;
    Logger.Write('GENERAL', 'Update', 'Restarting.');
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);
    Close;
  end;

  // DISABLE GUI IF INITIALIZATION EXCEPTION
  if bInitException then begin
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
  SettingsButton.Flat := true;
  OptionsButton.Flat := true;
  UpdateButton.Flat := true;
  HelpButton.Flat := true;
  IconList.GetBitmap(0, NewButton.Glyph);
  IconList.GetBitmap(1, BuildButton.Glyph);
  IconList.GetBitmap(2, SubmitButton.Glyph);
  IconList.GetBitmap(3, SettingsButton.Glyph);
  IconList.GetBitmap(4, OptionsButton.Glyph);
  IconList.GetBitmap(5, UpdateButton.Glyph);
  IconList.GetBitmap(6, HelpButton.Glyph);

  // STATUSBAR VALUES
  StatusPanelLanguage.Caption := settings.language;
  StatusPanelVersion.Caption := 'v'+ProgramVersion;

  // UPDATE GUI
  PluginsListView.OwnerDraw := not settings.simplePluginsView;
  PluginsListView.Items.Count := PluginsList.Count;
  UpdateLog;
  UpdatePatches;
  UpdatePluginsPopupMenu;
  UpdateStatusBar;
  UpdateQuickBar;

  if not bInitException then begin
    // ATTEMPT TO CONNECT TO SERVER
    {ConnectCallback := ConnectDone;
    if (not bConnecting) and (not TCPClient.Connected) then
      TConnectThread.Create; }

    // START BACKGROUND LOADER
    LoaderCallback := LoaderDone;
    SetTaskbarProgressState(tbpsIndeterminate);
    TLoaderThread.Create;

    // CORRECT LIST VIEW WIDTHS
    CorrectListViewWidth(PatchesListView);
    CorrectListViewWidth(PluginsListView);

    // LOAD AND DISPLAY HINTS
    StatusPanelMessage.Caption := GetString('mpMain_LoaderInProgress');
    bhLoader.Title := GetString('mpMain_LoaderInProgress');
    bhLoader.Description := GetString('mpMain_LoaderLimitations');
    bhLoadException.Title := GetString('mpMain_LoadException');
    bhLoadException.Description := GetString('mpMain_PluginsNotLoaded');
    DisplayHints;

    // initialize task handler
    {TaskHandler := TTaskHandler.Create;
    bLogTasks := false;
    TaskHandler.AddTask(TTask.Create('Disable Hints', 12.0 * seconds, DisableHints));
    TaskHandler.AddTask(TTask.Create('Reconnect', 15.0 * seconds, Reconnect));
    TaskHandler.AddTask(TTask.Create('Heartbeat', 0.9 * seconds, Heartbeat));
    TaskHandler.AddTask(TTask.Create('Refresh GUI', 3.0 * seconds, RefreshGUI));
    TaskTimer.Enabled := true;}
  end;

  // ACTIVATE WINDOW
  FormDisplayTime := Now;
  ForceForeground(Handle);
end;

procedure TPatchform.LoaderStatus(s: string);
begin
  StatusPanelMessage.Caption := s;
end;

procedure TPatchForm.LoaderDone;
begin
  SetTaskbarProgressState(tbpsNone);
  xEditLogGroup := 'GENERAL';
  xEditLogLabel := 'xEdit';
  FlashWindow(Application.Handle, True);
  UpdateQuickbar;
end;

procedure TPatchForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := bAllowClose;
  if not bClosing then begin
    bClosing := true;
    Enabled := false;

    // show progress form
    pForm := TProgressForm.Create(Self);
    pForm.LogPath := LogPath;
    pForm.PopupParent := Self;
    pForm.Caption := GetString('mpProg_Closing');
    pForm.MaxProgress(PluginsList.Count + PatchesList.Count + 2);
    pForm.Show;

    // start save thread
    SaveCallback := SaveDone;
    TSaveThread.Create;
  end;
end;

procedure TPatchForm.SaveDone;
begin
  // clean up pForm, close form
  pForm.SetProgress(pForm.ProgressBar.Max);
  pForm.SaveLog;
  pForm.Free;

  // restart program if update applied
  if bInstallUpdate then
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);
  // restart program if user wants patch profile change
  if bChangeProfile then
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);

  // allow close and close
  bAllowClose := true;
  Close;
end;

procedure TPatchForm.ConnectDone;
begin
  // UPDATE QUICKBAR
  UpdateQuickbar;
end;

procedure TPatchForm.ProgressDone;
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

procedure TPatchForm.AutoUpdate;
begin
  if settings.updateDictionary then begin
    // update dictionary
    if bDictionaryUpdate and UpdateDictionary then begin
      status := TmsStatus.Create;
      CompareStatuses;
    end;
  end;
  if settings.updateProgram then begin
    // update program
    if bProgramUpdate and DownloadProgram then
      bInstallUpdate := UpdateProgram;
  end;
end;

function TPatchForm.ShouldDisplay(bh: TBalloonHint): boolean;
begin
  Result := (Now - FormDisplayTime) * 86400 < (bh.HideAfter / 1000);
end;

procedure TPatchForm.DisableHints;
begin
  HideHints;
  TaskHandler.RemoveTask('Disable Hints');
end;

procedure TPatchForm.HideHints;
begin
  bhLoader.HideHint;
  bhLoadException.HideHint;
end;

procedure TPatchForm.DisplayHints;
var
  pt: TPoint;
begin
  if bLoadException and ShouldDisplay(bhLoadException) then begin
    pt.X := 126;
    pt.Y := 16;
    pt := MainPanel.ClientToScreen(pt);
    bhLoadException.ShowHint(pt);
  end;

  if ShouldDisplay(bhLoader) then begin
    pt.X := 8;
    pt.Y := 4;
    pt := ImageBlocked.ClientToScreen(pt);
    bhLoader.ShowHint(pt);
  end;
end;

procedure TPatchForm.Reconnect;
begin
  if not (TCPClient.Connected or bConnecting or bClosing) then
    TConnectThread.Create;
end;

procedure TPatchForm.RefreshGUI;
begin
  if not bClosing then UpdateStatusBar;
end;

procedure TPatchForm.Heartbeat;
begin
  try
    if TCPClient.IOHandler.Opened and
    not (bConnecting or bClosing or ServerAvailable) then
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

procedure TPatchForm.OnTaskTimer(Sender: TObject);
begin
  TaskHandler.ExecTasks;
end;

procedure TPatchForm.ShowAuthorizationMessage;
begin
  if bAuthorized then begin
    Logger.Write('CLIENT', 'Login', 'Authorized');
  end
  else begin
    Logger.Write('CLIENT', 'Login', 'Not authorized');
  end;
end;

procedure TPatchForm.UpdateStatusBar;
begin
  ImageBlocked.Visible := not (bLoaderDone or bInitException);
  ImageConnected.Visible := TCPClient.Connected;
  ImageDisconnected.Visible := not TCPClient.Connected;
  ImageBuild.Visible := bLoaderDone and bPatchsToBuild;
  ImageDictionaryUpdate.Visible := bDictionaryUpdate;
  ImageProgramUpdate.Visible := bProgramUpdate;
  StatusPanelLanguage.Caption := settings.language;
end;

procedure TPatchForm.UpdateListViews;
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
function TPatchForm.AddDetailsItem(name, value: string;
  editable: boolean = false): TItemProp;
var
  prop: TItemProp;
begin
  DetailsEditor.InsertRow(name, value, true);
  prop := DetailsEditor.ItemProps[DetailsEditor.RowCount - 1];
  prop.ReadOnly := not editable;
  Result := prop;
end;

{
  Add one or more ListItem to DetailsView with @name and the values
  in @sl
}
procedure TPatchForm.AddDetailsList(name: string; sl: TStringList;
  editable: boolean = false);
var
  i: integer;
begin
  sl.Text := Wordwrap(sl.Text, 80);
  if sl.Count > 0 then begin
    AddDetailsItem(name, sl[0], editable);
    for i := 1 to Pred(sl.Count) do
      AddDetailsItem(' ', sl[i], editable);
  end
  else
    AddDetailsItem(name, ' ', editable);
end;

{
  Switch details view when page control is changed
}
procedure TPatchForm.PageControlChange(Sender: TObject);
var
  ndx: integer;
begin
  ndx := TPageControl(Sender).ActivePageIndex;
  case ndx of
    0: begin
      UpdatePluginDetails;
      CorrectListViewWidth(PluginsListView);
    end;
    1: begin
      UpdatePatchDetails;
      CorrectListViewWidth(PatchesListView);
    end;
    2: begin
      UpdateApplicationDetails;
      CorrectListViewWidth(LogListView);
    end;
  end;
end;

procedure TPatchForm.UpdateApplicationDetails;
begin
  // prepare list view for application information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := GetString('mpMain_AppDetails');

  // add details items
  AddDetailsItem(GetString('mpMain_Application'), 'Mator Smash');
  AddDetailsItem(GetString('mpMain_Author'), 'matortheeternal');
  AddDetailsItem(GetString('mpMain_Version'), ProgramVersion);
  AddDetailsItem(GetString('mpMain_DateBuilt'), DateTimeToStr(GetLastModified(ParamStr(0))));
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetString('mpMain_GameMode'), wbGameName);
  AddDetailsItem(GetString('mpMain_Language'), settings.language);
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetString('mpMain_TimesRun'), IntToStr(statistics.timesRun + sessionStatistics.timesRun));
  AddDetailsItem(GetString('mpMain_PatchsBuilt'), IntToStr(statistics.patchesBuilt + sessionStatistics.patchesBuilt));
  AddDetailsItem(GetString('mpMain_PluginsPatchd'), IntToStr(statistics.pluginsPatched + sessionStatistics.pluginsPatched));
  AddDetailsItem(GetString('mpMain_ReportsSubmitted'), IntToStr(statistics.settingsSubmitted + sessionStatistics.settingsSubmitted));
  AddDetailsItem(GetString('mpMain_ReportsSubmitted'), IntToStr(statistics.recsSubmitted + sessionStatistics.recsSubmitted));
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetString('mpMain_Website'), '-');
  AddDetailsItem(GetString('mpMain_ApiCredits'), 'superobject, TurboPower Abbrevia, xEdit');
  AddDetailsItem(GetString('mpMain_xEditVersion'), xEditVersion);
  AddDetailsItem(GetString('mpMain_xEditCredits'), 'zilav, hlp, Sharlikran, ElminsterAU');
  AddDetailsItem(GetString('mpMain_Testers'), ProgramTesters);
  AddDetailsItem(GetString('mpMain_Translators'), ProgramTranslators);
end;

procedure TPatchForm.DetailsCopyToClipboardItemClick(Sender: TObject);
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
  for i := 0 to Pred(DetailsEditor.Strings.Count) do begin
    previousName := name;
    name := DetailsEditor.Strings.Names[i];
    previousValue := value;
    value := DetailsEditor.Strings.ValueFromIndex[i];
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

{ Handle user clicking URL }
procedure TPatchForm.DetailsEditorMouseUp(Sender: TObject; Button: TMouseButton;
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

procedure TPatchForm.UpdatePluginDetails;
var
  plugin: TPlugin;
  index: integer;
begin
  // don't do anything if no item selected
  if not Assigned(PluginsListView.Selected) then
    exit;

  // prepare list view for plugin information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := GetString('mpMain_PluginDetails');

  // get plugin information
  index := PluginsListView.ItemIndex;
  plugin := TPlugin(PluginsList[index]);
  if not plugin.hasData then plugin.GetData;

  // add details items
  AddDetailsItem(GetString('mpMain_Filename'), plugin.filename);
  AddDetailsItem(GetString('mpMain_Hash'), plugin.hash);
  AddDetailsItem(GetString('mpMain_FileSize'), FormatByteSize(plugin.fileSize));
  AddDetailsItem(GetString('mpMain_DateModified'), plugin.dateModified);
  AddDetailsItem(GetString('mpMain_PatchRating'), plugin.entry.rating);
  AddDetailsItem(GetString('mpMain_NumRecords'), plugin.numRecords);
  AddDetailsItem(GetString('mpMain_NumOverrides'), plugin.numOverrides);
  AddDetailsItem(GetString('mpMain_Author'), plugin.author);
  AddDetailsList(GetString('mpMain_Description'), plugin.description);
  AddDetailsList(GetString('mpMain_Masters'), plugin.masters);
end;

procedure TPatchForm.AddPluginsToPatch(var patch: TPatch);
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
    // add plugin to patch
    Logger.Write('PLUGIN', 'Patch', 'Added '+plugin.filename+' to patch '+patch.name);
    if not plugin.hasData then
      plugin.GetData;
    patch.plugins.AddObject(plugin.filename, TObject(i));
    plugin.patch := patch.name;
  end;

  // update and repaint
  UpdatePatches;
  UpdateListViews;
  UpdateQuickbar;
  UpdateStatusBar;
end;

procedure TPatchForm.PluginsListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdatePluginDetails;
end;

procedure TPatchForm.PluginsListViewData(Sender: TObject; Item: TListItem);
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
  PluginsListView.Canvas.Font.Color := GetRatingColor(StrToFloatDef(plugin.entry.rating, -2.0));
  PluginsListView.Canvas.Font.Style := PluginsListView.Canvas.Font.Style + [fsBold];
end;

procedure TPatchForm.PluginsListViewDrawItem(Sender: TCustomListView;
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

procedure TPatchForm.PluginsPopupMenuPopup(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  // ?
end;

procedure TPatchForm.UpdatePluginsPopupMenu;
var
  i: Integer;
  patch: TPatch;
  AddToPatchItem, MenuItem: TMenuItem;
begin
  // clear popup menu
  AddToPatchItem := PluginsPopupMenu.Items[0];
  AddToPatchItem.Clear;

  // add <New Patch> option to Plugins popup menu
  MenuItem := TMenuItem.Create(AddToPatchItem);
  MenuItem.Caption := GetString('mpMain_NewPatchItem_Caption');
  MenuItem.OnClick := AddToNewPatchClick;
  AddToPatchItem.Add(MenuItem);

  // add patches to plugins popup menu
  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    MenuItem := TMenuItem.Create(AddToPatchItem);
    MenuItem.Caption := patch.name;
    MenuItem.OnClick := AddToPatchClick;
    AddToPatchItem.Add(MenuItem);
  end;
end;

procedure TPatchForm.AddToPatchClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  patch: TPatch;
begin
  MenuItem := TMenuItem(Sender);
  patch := PatchesList[AddToPatchItem.IndexOf(MenuItem) - 1];
  AddPluginsToPatch(patch);
end;

procedure TPatchForm.AddToNewPatchClick(Sender: TObject);
var
  patch: TPatch;
begin
  patch := NewPatch;
  if Assigned(patch) then
    AddPluginsToPatch(patch);
end;

{ Remove from Patch }
procedure TPatchForm.RemoveFromPatchItemClick(Sender: TObject);
var
  i: integer;
  listItem: TListItem;
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
      patch := PatchByName(PatchesList, patchName);
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

procedure TPatchForm.OpenPluginLocationItemClick(Sender: TObject);
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
  - UpdatePatchs
  - PatchesListViewChange
  - PatchesListViewData
  - PatchesListViewDrawItem
  - SavePatchEdit
}
{******************************************************************************}

procedure TPatchForm.UpdatePatchDetails;
var
  patchItem: TListItem;
  patch: TPatch;
  sl: TStringList;
begin
  // don't do anything if no item selected
  patchItem := PatchesListView.Selected;
  if not Assigned(patchItem) then
    exit;

  // prepare list view for patch information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := GetString('mpMain_PatchDetails');

  // get patch information
  patch := PatchesList[PatchesListView.ItemIndex];
  AddDetailsItem(GetString('mpMain_Status'), StatusArray[Ord(patch.status)].desc, false);
  AddDetailsItem(GetString('mpMain_PatchName'), patch.name, true);
  AddDetailsItem(GetString('mpMain_Filename'), patch.filename, true);
  AddDetailsItem(GetString('mpMain_PluginCount'), IntToStr(patch.plugins.Count));
  AddDetailsItem(GetString('mpMain_DateBuilt'), DateBuiltString(patch.dateBuilt));
  AddDetailsList(GetString('mpMain_Plugins'), patch.plugins);
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetString('mpMain_PatchMethod'), patch.method, false);
  AddDetailsItem(GetString('mpMain_Renumbering'), patch.renumbering, false);
  if patch.files.Count < 250 then begin
    sl := TStringList.Create;
    sl.Text := StringReplace(patch.files.Text, settings.patchDirectory, '', [rfReplaceAll]);
    AddDetailsList(GetString('mpMain_Files'), sl);
    sl.Free;
  end
  else
    AddDetailsItem(GetString('mpMain_Files'), GetString('mpMain_TooManyFiles'));
  if patch.fails.Count < 250 then
    AddDetailsList(GetString('mpMain_Fails'), patch.fails)
  else
    AddDetailsItem(GetString('mpMain_Fails'), GetString('mpMain_TooManyFails'));
end;

procedure TPatchForm.UpdatePatches;
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
    if not (patch.status in ForcedStatuses) then
      patch.GetStatus;
  end;
end;

function TPatchForm.NewPatch: TPatch;
var
  patch: TPatch;
  EditPatch: TEditForm;
begin
  Result := nil;
  patch := CreateNewPatch(PatchesList);

  // edit patch immediately after its creation
  EditPatch := TEditForm.Create(Self);
  EditPatch.patch := patch;
  if EditPatch.ShowModal = mrOk then begin
    patch := EditPatch.patch;
    LogMessage('MERGE', 'New', 'Created new patch '+patch.name);
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

procedure TPatchForm.PatchesListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdatePatchDetails;
end;

procedure TPatchForm.PatchesListViewData(Sender: TObject; Item: TListItem);
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

procedure TPatchForm.PatchesListViewDrawItem(Sender: TCustomListView;
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

procedure TPatchForm.LogListViewData(Sender: TObject; Item: TListItem);
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
  else if (msg.group = 'MERGE') then
    LogListView.Canvas.Font.Color := settings.patchMessageColor
  else if (msg.group = 'PLUGIN') then
    LogListView.Canvas.Font.Color := settings.pluginMessageColor
  else if (msg.group = 'ERROR') then
    LogListView.Canvas.Font.Color := settings.errorMessageColor;
end;

procedure TPatchForm.LogListViewDrawItem(Sender: TCustomListView;
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
  Result := IfThen(not b, GetString('mpMain_Enable'), GetString('mpMain_Disable'));
end;

procedure TPatchForm.LogPopupMenuPopup(Sender: TObject);
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
  ToggleAutoScrollItem.Caption := Format('%s %s', [EnableStr(bAutoScroll), GetString('mpMain_AutoScroll')]);
end;

// toggles a group filter for the LogListView
procedure TPatchForm.ToggleGroupFilter(Sender: TObject);
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
  CorrectListViewWidth(LogListView);
end;

// toggles a label filter for the LogListView
procedure TPatchForm.ToggleLabelFilter(Sender: TObject);
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
  CorrectListViewWidth(LogListView);
end;

// toggles auto scroll for the LogListView
procedure TPatchForm.ToggleAutoScrollItemClick(Sender: TObject);
begin
  bAutoScroll := not bAutoScroll;
end;

procedure TPatchForm.CopyToClipboardItemClick(Sender: TObject);
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

procedure TPatchForm.SaveAndClearItemClick(Sender: TObject);
begin
  SaveLog(BaseLog);
  LogListView.Items.Count := 0;
  BaseLog.Clear;
  Log.Clear;
  LogMessage('GENERAL', 'Log', 'Saved and cleared log.');
end;


{******************************************************************************}
{ PatchPopupMenu methods
  Methods for dealing with the popup menu for the PatchesListView.
  - PatchsPopupMenuPopup
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

procedure TPatchForm.PatchesPopupMenuPopup(Sender: TObject);
var
  bNeverBuilt, bHasBuildStatus, bHasUpToDateStatus, bHasResolveStatus,
  bHasCheckStatus, bHasErrorStatus, bHasSelection, bHasPluginErrors,
  bIsNotTop, bIsNotBottom: boolean;
  patch: TPatch;
  i, patchesSelected: Integer;
  sBuild, sRebuild: string;
begin
  bNeverBuilt := false;
  bHasBuildStatus := false;
  bHasUpToDateStatus := false;
  bHasCheckStatus := false;
  bHasErrorStatus := false;
  bHasPluginErrors := false;
  bHasResolveStatus := false;
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
    bHasCheckStatus := bHasCheckStatus or (patch.status = msCheckErrors);
    bHasPluginErrors := bHasPluginErrors or (patch.status = msErrors);
    bHasErrorStatus := bHasErrorStatus or (patch.status in ErrorStatuses);
    bHasResolveStatus := bHasResolveStatus or (patch.status in ResolveStatuses);
  end;

  bHasSelection := (patchesSelected > 0);
  // change enabled state of PatchsPopupMenu items based on booleans
  EditPatchItem.Enabled := bHasSelection;
  DeletePatchItem.Enabled := bHasSelection;
  BuildPatchItem.Enabled := bHasSelection and bHasBuildStatus and bLoaderDone;
  ToggleRebuildItem.Enabled := bHasSelection and not bNeverBuilt and
    (bHasUpToDateStatus or bHasBuildStatus);
  OpenInExplorerItem.Enabled := bHasSelection;
  // plugins submenu
  PluginsItem.Enabled := bHasSelection;
  ResolveIssuesItem.Enabled := bHasSelection and bHasResolveStatus;
  CheckPluginsItem.Enabled := bHasSelection and bHasCheckStatus and bLoaderDone;
  FixPluginsItem.Enabled := bHasSelection and bHasPluginErrors and bLoaderDone;
  ReportOnPluginsItem.Enabled := bHasSelection and bHasUpToDateStatus;
  // move submenu
  MoveItem.Enabled := bHasSelection;
  UpItem.Enabled := bHasSelection and bIsNotTop;
  DownItem.Enabled := bHasSelection and bIsNotBottom;
  ToTopItem.Enabled := bHasSelection and bIsNotTop;
  ToBottomItem.Enabled := bHasSelection and bIsNotBottom;

  // one or multiple patches?
  if (patchesSelected = 1) then begin
    sBuild := 'mpMain_BuildPatch';
    sRebuild := 'mpMain_RebuildPatch';
  end
  else begin
    sBuild := 'mpMain_BuildPatchs';
    sRebuild := 'mpMain_RebuildPatchs';
  end;
  // handle build patches menu item
  if bNeverBuilt and not bHasErrorStatus then
    BuildPatchItem.Caption := GetString(sBuild)
  else if bHasBuildStatus then
    BuildPatchItem.Caption := GetString(sRebuild)
  else begin
    BuildPatchItem.Enabled := false;
    BuildPatchItem.Caption := GetString(sRebuild);
  end;
end;

procedure TPatchForm.EditPatchItemClick(Sender: TObject);
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
    Logger.Write('MERGE', 'Edit', 'Editing '+patch.name);
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

procedure TPatchForm.UpItemClick(Sender: TObject);
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

procedure TPatchForm.DownItemClick(Sender: TObject);
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

procedure TPatchForm.ToTopItemClick(Sender: TObject);
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

procedure TPatchForm.ToBottomItemClick(Sender: TObject);
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

{ Fix erorrs in plugins in patch }
procedure TPatchForm.FixPluginsItemClick(Sender: TObject);
var
  i, j: Integer;
  plugin: TPlugin;
  patch: TPatch;
begin
  timeCosts := TStringList.Create;
  pluginsToHandle := TList.Create;

  // get timecosts
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);
    if not (patch.status = msErrors) then
      continue;

    // else loop through plugins
    Logger.Write('MERGE', 'Plugins', 'Fixing erorrs in plugins in '+patch.name);
    for j := 0 to Pred(patch.plugins.Count) do begin
      plugin := PluginByFilename(patch.plugins[j]);
      // skip plugins that don't have errors
      if not plugin.HasErrors then continue;
      pluginsToHandle.Add(plugin);
      timeCosts.Add(plugin.numRecords);
    end;
  end;

  // free and exit if no plugins to fix errors in
  if pluginsToHandle.Count = 0 then begin
    timeCosts.Free;
    pluginsToHandle.Free;
    exit;
  end;

  // show progress form
  ShowProgressForm(self, pForm, GetString('mpProg_Fixing'));

  // start error checking thread
  ErrorFixCallback := ProgressDone;
  TErrorFixThread.Create;
end;

{ Check plugins in patch for errors }
procedure TPatchForm.CheckPluginsItemClick(Sender: TObject);
var
  i, j: Integer;
  plugin: TPlugin;
  patch: TPatch;
begin
  timeCosts := TStringList.Create;
  pluginsToHandle := TList.Create;

  // get timecosts
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);
    if not (patch.status = msCheckErrors) then
      continue;

    // else loop through plugins
    Logger.Write('MERGE', 'Plugins', 'Checking plugins in '+patch.name+' for errors');
    for j := 0 to Pred(patch.plugins.Count) do begin
      plugin := PluginByFilename(patch.plugins[j]);
      // skip plugins that have already been checked for errors
      if plugin.HasBeenCheckedForErrors then continue;
      pluginsToHandle.Add(plugin);
      timeCosts.Add(plugin.numRecords);
    end;
  end;

  // free and exit if no patches to check for errors
  if pluginsToHandle.Count = 0 then begin
    timeCosts.Free;
    pluginsToHandle.Free;
    exit;
  end;

  // Show progress form
  ShowProgressForm(self, pForm, GetString('mpProg_Checking'));

  // start error checking thread
  ErrorCheckCallback := ProgressDone;
  TErrorCheckThread.Create;
end;

procedure TPatchForm.ResolveIssuesItemClick(Sender: TObject);
var
  i: Integer;
  patch: TPatch;
begin
  // create resolve form
  self.Enabled := false;
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);
    if not (patch.status in ResolveStatuses) then
      continue;

    // create resolve form
    rForm := TResolveForm.Create(self);
    rForm.patch := patch;
    rForm.ShowModal;
    rForm.Free;
  end;

  // update patches and gui
  self.Enabled := true;
  UpdatePatches;
  UpdateQuickbar;
  UpdateStatusBar;
  UpdatePluginsPopupMenu;
  UpdateListViews;
end;

{ Remove unloaded plugins and plugins with errors }
procedure TPatchForm.RemovePluginsItemClick(Sender: TObject);
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
    Logger.Write('MERGE', 'Plugins', 'Removing plugins from '+patch.name);
    // remove plugins that aren't loaded or have errors
    for j := Pred(patch.plugins.Count) downto 0 do begin
      plugin := PluginByFilename(patch.plugins[j]);
      if not Assigned(plugin) then begin
        Logger.Write('MERGE', 'Plugins', 'Removing '+patch.plugins[j]+', plugin not loaded');
        patch.plugins.Delete(j);
        continue;
      end;
      if plugin.HasErrors and (not plugin.bIgnoreErrors) then begin
        Logger.Write('MERGE', 'Plugins', 'Removing '+patch.plugins[j]+', plugin has errors');
        patch.Remove(plugin);
      end;
      if plugin.bDisallowMerging then begin
        Logger.Write('MERGE', 'Plugins', 'Removing '+patch.plugins[j]+', plugin marked as do not patch');
        patch.Remove(plugin);
      end;
      if IS_BLACKLISTED in plugin.flags then begin
        Logger.Write('MERGE', 'Plugins', 'Removing '+patch.plugins[j]+', plugin marked as blacklisted');
        patch.Remove(plugin);
      end;
    end;
  end;

  // update
  UpdatePatches;
  UpdateListViews;
end;

procedure TPatchForm.DeletePatchItemClick(Sender: TObject);
var
  i, j: Integer;
  plugin: TPlugin;
  patch: TPatch;
  patchNames: string;
  bApproved: boolean;
  patchesToDelete: TList;
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
    bApproved := MessageDlg(GetString('mpMain_DeletePatchs') + patchNames, mtConfirmation,
      mbOKCancel, 0) = mrOk;

  // exit if user didn't approve deletion
  if not bApproved then
    exit;

  // clear details editor
  DetailsEditor.Strings.Clear;

  // loop through patches
  for i := Pred(patchesToDelete.Count) downto 0 do begin
    patch := TPatch(patchesToDelete[i]);
    Logger.Write('MERGE', 'Delete', 'Deleting patch '+patch.name);
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

procedure TPatchForm.BuildPatchItemClick(Sender: TObject);
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
    Logger.Write('MERGE', 'Build', 'Building '+patch.name);
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
  xEditLogGroup := 'MERGE';
  pForm := TProgressForm.Create(Self);
  pForm.LogPath := LogPath;
  pForm.PopupParent := Self;
  pForm.Caption := GetString('mpProg_Merging');
  pForm.MaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pForm.Show;

  // start patch thread
  PatchCallback := ProgressDone;
  TPatchThread.Create;
end;

procedure TPatchForm.ReportOnPluginsItemClick(Sender: TObject);
var
  i, j: Integer;
  patch: TPatch;
  pluginsList: TList;
  plugin: TPlugin;
  ReportForm: TReportForm;
  bReportsSent, bModalOK: boolean;
begin
  pluginsList := TList.Create;
  bModalOK := false;

  // loop through patches
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);
    Logger.Write('MERGE', 'Report', 'Reporting on '+patch.name);
    for j := 0 to Pred(patch.plugins.Count) do begin
      plugin := PluginByFilename(patch.plugins[j]);
      pluginsList.Add(plugin);
    end;
  end;

  // report on all patches
  ReportForm := TReportForm.Create(Self);
  if pluginsList.Count > 0 then begin
    ReportForm.pluginsToReport := pluginsList;
    ReportForm.AppName := wbAppName;
    bModalOK := ReportForm.ShowModal = mrOk;
  end;

  // Send reports to backend
  if bModalOK then begin
    bReportsSent := SendReports(ReportForm.reportsList);
    if not bReportsSent then begin
      Logger.Write('MERGE', 'Report', 'Saving reports locally');
      SaveReports(ReportForm.reportsList, ProgramPath + 'reports\');
    end
    else begin
      Logger.Write('MERGE', 'Report', 'Saving reports locally');
      SaveReports(ReportForm.reportsList, ProgramPath + 'reports\submitted\');
    end;
  end;

  // clean up
  ReportForm.Free;
  pluginsList.Free;
end;

procedure TPatchForm.OpenInExplorerItemClick(Sender: TObject);
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
    path := settings.patchDirectory + patch.name;
    ForceDirectories(path);
    ShellExecute(0, 'open', PChar(path), '', '', SW_SHOWNORMAL);
  end;
end;

procedure TPatchForm.ToggleRebuildItemClick(Sender: TObject);
var
  i: Integer;
  patch: TPatch;
begin
  // loop through patches
  for i := 0 to Pred(PatchesListView.Items.Count) do begin
    if not PatchesListView.Items[i].Selected then
      continue;
    patch := TPatch(PatchesList[i]);
    Logger.Write('MERGE', 'Status', 'Toggled rebuild status on '+patch.name);
    // if forced up to date, set to Ready to be rebuilt
    if patch.status = msUpToDateForced then
      patch.status := msRebuildReady
    // if normal up to date, set to Ready to rebuilt [forced]
    else if patch.status = msUpToDate then
      patch.Status := msRebuildReadyForced
    // if force rebuild, set to Up to date
    else if patch.status = msRebuildReadyForced then
      patch.status := msUpToDate
    // if normal rebuild, set to Up to date [Forced]
    else if patch.status = msRebuildReady then
      patch.Status := msUpToDateForced;
  end;

  // update
  UpdatePatches;
  UpdateListViews;
  UpdateQuickBar;
end;

procedure TPatchForm.ImageDisconnectedClick(Sender: TObject);
begin
  if (not TCPClient.Connected)
  and (ConnectionAttempts = MaxConnectionAttempts) then begin
    Logger.Write('CLIENT', 'Connect', 'Retrying connecting to the server.');
    ConnectionAttempts := 0;
  end;
end;

{ Double click to edit patch }
procedure TPatchForm.PatchesListViewDblClick(Sender: TObject);
begin
  EditPatchItemClick(nil);
end;

{ Shortcut to delete patches using the delete key }
procedure TPatchForm.PatchesListViewKeyDown(Sender: TObject; var Key: Word;
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

procedure TPatchForm.UpdateQuickbar;
var
  i, j: Integer;
  bUncheckedPlugins, bPatchsToReportOn: boolean;
  patch: TPatch;
  plugin: TPlugin;
  sTitle: string;
begin
  // DISABLE ALL BUTTONS IF INITIALIZATION EXCEPTION
  if bInitException then begin
    NewButton.Enabled := false;
    FindErrorsButton.Enabled := false;
    BuildButton.Enabled := false;
    ReportButton.Enabled := false;
    DictionaryButton.Enabled := false;
    OptionsButton.Enabled := true;
    UpdateButton.Enabled := false;
    HelpButton.Enabled := false;
    exit;
  end;

  // FIND ERRORS BUTTON
  bUncheckedPlugins := false;
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := TPlugin(PluginsList[i]);
    if (IS_BLACKLISTED in plugin.flags) then
      continue;
    if not plugin.HasBeenCheckedForErrors then
      bUncheckedPlugins := true;
  end;

  // enable find errors button if there are unchecked plugins
  FindErrorsButton.Enabled := bLoaderDone and bUncheckedPlugins;
  // swap hints
  sTitle := GetString('mpMain_FindErrorsButton_Hint');
  if not bLoaderDone then
    FindErrorsButton.Hint := sTitle + GetString('mpMain_FindErrors_Loader')
  else if not bUncheckedPlugins then
    FindErrorsButton.Hint := sTitle + GetString('mpMain_NoPluginsToCheck')
  else
    FindErrorsButton.Hint := sTitle + GetString('mpMain_CheckAllPlugins');

  // BUILD BUTTON
  bPatchsToBuild := false;
  bPatchsToCheck := false;
  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    if (patch.status in BuildStatuses) then
      bPatchsToBuild := true;
    if (patch.status = msCheckErrors) then
      bPatchsToCheck := true;
  end;

  // enable build button if there are patches to build
  BuildButton.Enabled := bPatchsToBuild and bLoaderDone;
  // swap hints
  sTitle := GetString('mpMain_BuildButton_Hint');
  if not bLoaderDone then
    BuildButton.Hint := sTitle + GetString('mpMain_BuildPatchs_Loader')
  else if not bPatchsToBuild then
    BuildButton.Hint := sTitle + GetString('mpMain_NoPatchs')
  else if bPatchsToCheck then
    BuildButton.Hint := sTitle + GetString('mpMain_CheckPatchs')
  else
    BuildButton.Hint := sTitle + GetString('mpMain_BuildAllPatchs');

  // REPORT BUTTON
  bPatchsToReportOn := false;
  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    if patch.status <> msUpToDate then continue;
    for j := 0 to Pred(patch.plugins.Count) do begin
      plugin := PluginByFilename(patch.plugins[j]);
      if not Assigned(plugin) then
        continue;
      if not ReportExistsFor(plugin) then
        bPatchsToReportOn := true;
    end;
  end;
  ReportButton.Enabled := bPatchsToReportOn;
  if not bPatchsToReportOn then
    ReportButton.Hint := GetString('mpMain_NoPatchsToReportOn')
  else
    ReportButton.Hint := GetString('mpMain_ReportButton_Hint');

  // DICTIONARY BUTTON
  DictionaryButton.Enabled := dictionary.Count > 0;
  if not DictionaryButton.Enabled then
    DictionaryButton.Hint := GetString('mpMain_NoDictionary')
  else
    DictionaryButton.Hint := GetString('mpMain_DictionaryButton_Hint');

  // UPDATE BUTTON
  UpdateButton.Enabled := bProgramUpdate or bDictionaryUpdate;
  sTitle := GetString('mpMain_UpdateButton_Hint');
  if bProgramUpdate and bDictionaryUpdate then
    UpdateButton.Hint := sTitle + GetString('mpMain_UpdateBoth')
  else if bProgramUpdate then
    UpdateButton.Hint := sTitle + GetString('mpMain_UpdateProgram')
  else if bDictionaryUpdate then
    UpdateButton.Hint := sTitle + GetString('mpMain_UpdateDictionary')
  else
    UpdateButton.Hint := sTitle + GetString('mpMain_NoUpdates');

  // HELP BUTTON
  HelpButton.Enabled := false; // TODO: help file integration
end;

procedure TPatchForm.CreatePatchButtonClick(Sender: TObject);
begin
  NewPatch;
end;

procedure TPatchForm.BuildButtonClick(Sender: TObject);
var
  i, timeCost: integer;
  patch: TPatch;
begin
  // exit if the loader isn't done
  if not bLoaderDone then begin
    Logger.Write('ERROR', 'Patch', 'Loader not done, can''t patch yet!');
    exit;
  end;

  // exit if there are no patches
  if PatchesList.Count = 0 then begin
    Logger.Write('ERROR', 'Patch', 'There are no patches!');
    exit;
  end;

  {// calculate time costs, prepare patches
  timeCosts := TStringList.Create;
  patchesToBuild := TList.Create;
  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    Logger.Write('MERGE', 'Build', 'Building '+patch.name);
    if not (patch.status in BuildStatuses) then
      continue;
    timeCost := patch.GetTimeCost * 2;
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
  xEditLogGroup := 'MERGE';
  pForm := TProgressForm.Create(Self);
  pForm.LogPath := LogPath;
  pForm.PopupParent := Self;
  pForm.Caption := GetString('mpProg_Merging');
  pForm.MaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pForm.Show;

  // start patch thread
  PatchCallback := ProgressDone;
  TPatchThread.Create; }
end;

{ Submit report }
procedure TPatchForm.SubmitButtonClick(Sender: TObject);
var
  i, j: Integer;
  patch: TPatch;
  pluginsList: TList;
  plugin: TPlugin;
  bModalOK, bReportsSent: boolean;
begin
  // initialize variables
  pluginsList := TList.Create;

  // loop through plugins in patches
  for i := 0 to Pred(PatchesList.Count) do begin
    patch := TPatch(PatchesList[i]);
    // skip patches that aren't up to date - only want user to submit
    // reports on plugins in patches they've built
    if patch.status <> msUpToDate then continue;
    // loop through plugins in patch
    for j := 0 to Pred(patch.plugins.Count) do begin
      plugin := PluginByFilename(patch.plugins[j]);
      // if we can't find the plugin, continue
      if not Assigned(plugin) then continue;
      // if the user doesn't have a local report for the plugin
      // we will add it to the list of plugins for them to report on
      if not ReportExistsFor(plugin) then
        pluginsList.Add(plugin);
    end;
  end;

  // create report form
  bModalOK := false;
  ReportForm := TReportForm.Create(Self);
  if pluginsList.Count > 0 then begin
    ReportForm.pluginsToReport := pluginsList;
    ReportForm.AppName := wbAppName;
    bModalOK := ReportForm.ShowModal = mrOk;
  end;

  // Send reports to backend
  if bModalOK then begin
    bReportsSent := SendReports(ReportForm.reportsList);
    if not bReportsSent then begin
      Logger.Write('MERGE', 'Report', 'Saving reports locally');
      SaveReports(ReportForm.reportsList, ProgramPath + 'reports\');
    end
    else begin
      Logger.Write('MERGE', 'Report', 'Saving reports locally');
      SaveReports(ReportForm.reportsList, ProgramPath + 'reports\submitted\');
    end;
  end;

  // clean up
  ReportForm.Free;
  pluginsList.Free;
end;

{ View the dictionary file }
procedure TPatchForm.SettingsButtonClick(Sender: TObject);
var
  DictionaryForm: TDictionaryForm;
begin
  //LogMessage(TButton(Sender).Hint+' clicked!');
  DictionaryForm := TDictionaryForm.Create(Self);
  DictionaryForm.ShowModal;
  DictionaryForm.Free;
end;

{ Options }
procedure TPatchForm.OptionsButtonClick(Sender: TObject);
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
  if bChangePatchProfile then
    Close;

  // if user selected to update program, close application
  if bInstallUpdate then begin
    bInstallUpdate := UpdateProgram;
    if bInstallUpdate then
      Close;
  end;
end;

{ Update }
procedure TPatchForm.UpdateButtonClick(Sender: TObject);
begin
  // if not connected to server, don't try to update anything
  if not TCPClient.Connected then
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
  end;
end;

{ Help }
procedure TPatchForm.HelpButtonClick(Sender: TObject);
begin
  //LogMessage(TButton(Sender).Hint+' clicked!');
end;

end.
