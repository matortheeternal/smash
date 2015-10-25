unit msSettingsManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, ValEdit, CommCtrl, Menus,
  // mte units
  mteHelpers, mteLogger, RttiTranslation,
  // ms units
  msFrontend, msSettingForm;

type
  TSettingsManager = class(TForm)
    [FormPrefix('msSet')]
      Splitter: TSplitter;
      [FormSection('Settings')]
        pnlEntries: TPanel;
        gbFiltering: TGroupBox;
        lblName: TLabel;
        edName: TEdit;
        lblRecords: TLabel;
        edRecords: TEdit;
        lvSettings: TListView;
        [FormSection('Settings Popup Menu')]
          SettingsPopupMenu: TPopupMenu;
          NewSettingItem: TMenuItem;
          EditSettingItem: TMenuItem;
          DeleteSettingItem: TMenuItem;
      [FormSection('Details')]
        pnlDetails: TPanel;
        pnlDictionaryInfo: TPanel;
        lblDictionary: TLabel;
        vl: TValueListEditor;
      [FormSection('Notes')]
        pnlReportNotes: TPanel;
        lblNotes: TLabel;
        meNotes: TMemo;
    CloneSettingItem: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvSettingsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvSettingsData(Sender: TObject; Item: TListItem);
    procedure UpdateSmashSettingsDetails;
    procedure vlDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure lvSettingsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvSettingsDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    function MatchesFilters(setting: TSmashSetting): boolean;
    procedure ApplyFiltering;
    procedure cbChange(Sender: TObject);
    procedure edExit(Sender: TObject);
    procedure edKeyPress(Sender: TObject; var Key: Char);
    procedure SplitterMoved(Sender: TObject);
    procedure NewSettingItemClick(Sender: TObject);
    procedure EditSettingItemClick(Sender: TObject);
    procedure DeleteSettingItemClick(Sender: TObject);
    procedure CloneSettingItemClick(Sender: TObject);
    procedure SettingsPopupMenuPopup(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FilterFilename: string;
  end;

const
  red = TColor($0000FF);
  yellow = TColor($00A8E5);
  green = TColor($009000);

var
  SettingsManager: TSettingsManager;
  columnToSort: integer;
  ascending: boolean;
  tempSmashSettings: TList;

implementation

{$R *.dfm}

procedure TSettingsManager.FormCreate(Sender: TObject);
begin
  // do a translation dump?
  if bTranslationDump then
    TRttiTranslation.Save('lang\english.lang', self);

  // load translation
  TRttiTranslation.Load(language, self);

  // initialize list view, SmashSettings list
  tempSmashSettings := TList.Create;
  columnToSort := -1;
  lvSettings.OwnerDraw := not settings.simpleDictionaryView;
  lvSettings.Items.Count := tempSmashSettings.Count;
end;

procedure TSettingsManager.FormShow(Sender: TObject);
begin
  // initialize list of entries
  edName.Text := FilterFilename;
  if FilterFilename <> '' then
    Self.FocusControl(lvSettings);
  ApplyFiltering;

// force lvEntries to autosize columns
  lvSettings.Width := lvSettings.Width - 1;
  lvSettings.Width := lvSettings.Width + 1;
end;

// custom ValueListEditor draw for unselectable cells
procedure TSettingsManager.vlDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
begin
  vl.Canvas.Brush.Color := clWhite;
  vl.Canvas.FillRect(Rect);
  Rect.Left := Rect.Left + 2;
  DrawText(vl.Canvas.Handle, PChar(vl.Cells[aCol, ARow]), -1, Rect,
    DT_SINGLELINE or DT_LEFT OR DT_VCENTER or DT_NOPREFIX);
end;

// refresh SmashSettings Details ValueListEditor
procedure TSettingsManager.UpdateSmashSettingsDetails;
begin
  vl.Strings.Clear;

  // initialize SmashSettings details
  vl.InsertRow(GetString('mpDct_NumSettings'), IntToStr(SmashSettings.Count), true);
  vl.InsertRow(GetString('mpDct_SettingsDisplayed'), IntToStr(tempSmashSettings.Count), true);
end;

// update meNotes when user changes entry
procedure TSettingsManager.lvSettingsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  setting: TSmashSetting;
begin
  if lvSettings.ItemIndex = -1 then begin
    meNotes.Text := '';
    exit;
  end;

  setting := TSmashSetting(tempSmashSettings[lvSettings.ItemIndex]);
  meNotes.Text := StringReplace(setting.description, '@13', #13#10, [rfReplaceAll]);
end;

function CompareAsFloat(s1, s2: string): Integer;
var
  f1, f2: Real;
begin
  try
    f1 := StrToFloat(s1);
  except on Exception do
    f1 := 0;
  end;
  try
    f2 := StrToFloat(s2);
  except on Exception do
    f2 := 0;
  end;

  if f1 = f2 then
    Result := 0
  else if f1 > f2 then
    Result := 1
  else
    Result := -1;
end;

function CompareEntries(P1, P2: Pointer): Integer;
var
  setting1, setting2: TSmashSetting;
begin
  Result := 0;
  setting1 := TSmashSetting(P1);
  setting2 := TSmashSetting(P2);

  if columnToSort = 0 then
    Result := AnsiCompareText(setting1.name, setting2.name)
  else if columnToSort = 1 then
    Result := AnsiCompareText(setting1.records, setting2.records);

  if ascending then
    Result := -Result;
end;

procedure TSettingsManager.lvSettingsColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  ascending := (columnToSort = Column.Index) and (not ascending);
  columnToSort := Column.Index;
  tempSmashSettings.Sort(CompareEntries);
  lvSettings.Repaint;
  lvSettingsChange(nil, nil, TItemChange(nil));
end;

procedure TSettingsManager.lvSettingsData(Sender: TObject; Item: TListItem);
var
  entry: TSmashSetting;
begin
  entry := TSmashSetting(tempSmashSettings[Item.Index]);
  Item.Caption := entry.name;
  Item.SubItems.Add(entry.records);
  //lvSettings.Canvas.Font.Color := GetRatingColor(StrToFloat(entry.rating));
  lvSettings.Canvas.Font.Style := [fsBold];
end;

procedure TSettingsManager.lvSettingsDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i, x, y: integer;
  lv: TListView;
begin
  lv := TListView(Sender);
  if Item.Selected then begin
    lv.Canvas.Brush.Color := $FFEEDD;
    lv.Canvas.FillRect(Rect);
  end;
  x := Rect.Left;
  y := (Rect.Bottom - Rect.Top - lv.Canvas.TextHeight('Hg')) div 2 + Rect.Top;
  lv.Canvas.TextOut(x, y, ' '+Item.Caption);
  for i := 0 to Item.SubItems.Count - 1 do begin
    Inc(x, ListView_GetColumnWidth(lv.Handle, lv.Columns[i].Index));
    lv.Canvas.TextOut(x, y, ' '+Item.SubItems[i]);
  end;
end;

{******************************************************************************}
{ SettingPopupMenu methods
  Methods for dealing with the popup menu for the settings list view.
  - NewSettingItemClick
  - EditSettingItemClick
  - DeleteSettingItemClick
}
{******************************************************************************}

procedure TSettingsManager.SettingsPopupMenuPopup(Sender: TObject);
var
  bHasSelection: boolean;
begin
  bHasSelection := Assigned(lvSettings.Selected);
  EditSettingItem.Enabled := bHasSelection;
  DeleteSettingItem.Enabled := bHasSelection;
  CloneSettingItem.Enabled := bHasSelection;
end;

procedure TSettingsManager.NewSettingItemClick(Sender: TObject);
var
  setting: TSmashSetting;
  SettingForm: TSettingForm;
begin
  setting := TSmashSetting.Create;
  SettingForm := TSettingForm.Create(self);
  SettingForm.mode := smNew;
  SettingForm.setting := setting;
  if SettingForm.ShowModal = mrOK then begin
    SmashSettings.Add(SettingForm.setting);
    tempSmashSettings.Add(SettingForm.setting);
    ApplyFiltering;
  end;
  SettingForm.Free;
end;

procedure TSettingsManager.EditSettingItemClick(Sender: TObject);
var
  i, index: Integer;
  setting: TSmashSetting;
  SettingForm: TSettingForm;
begin
  for i := 0 to Pred(lvSettings.Items.Count) do begin
    if not lvSettings.Items[i].Selected then
      continue;
    setting := TSmashSetting(tempSmashSettings[i]);
    index := SmashSettings.IndexOf(setting);
    SettingForm := TSettingForm.Create(self);
    SettingForm.mode := smEdit;
    SettingForm.setting := setting;
    if SettingForm.ShowModal = mrOK then begin
      SmashSettings[index] := SettingForm.setting;
      tempSmashSettings[i] := SettingForm.setting;
      ApplyFiltering;
    end;
    SettingForm.Free;
    break;
  end;
end;

procedure TSettingsManager.DeleteSettingItemClick(Sender: TObject);
var
  i: Integer;
  setting: TSmashSetting;
begin
  for i := Pred(lvSettings.Items.Count) downto 0 do begin
    if not lvSettings.Items[i].Selected then
      continue;
    lvSettings.Items.Count := lvSettings.Items.Count - 1;
    setting := TSmashSetting(tempSmashSettings[i]);
    tempSmashSettings.Delete(i);
    setting.Free;
  end;
  lvSettings.Repaint;
end;

procedure TSettingsManager.CloneSettingItemClick(Sender: TObject);
var
  setting, clonedSetting: TSmashSetting;
  SettingForm: TSettingForm;
begin
  clonedSetting := TSmashSetting.Create;
  setting := TSmashSetting(tempSmashSettings[lvSettings.Selected.Index]);
  SettingForm := TSettingForm.Create(self);
  SettingForm.mode := smClone;
  SettingForm.setting := clonedSetting.Clone(setting);
  if SettingForm.ShowModal = mrOK then begin
    SmashSettings.Add(SettingForm.setting);
    tempSmashSettings.Add(SettingForm.setting);
    ApplyFiltering;
  end;
  SettingForm.Free;
end;

{******************************************************************************}
{ Filtering methods:
  Methods for filtering the SmashSettings.
  - MatchesFilters
  - ApplyFilter
  - cbChange
  - edExit
  - edKeyDown
}
{******************************************************************************}

function TSettingsManager.MatchesFilters(setting: TSmashSetting): boolean;
var
  bFilenameMatch, bRecordsMatch: boolean;
begin
  // get filter results
  bFilenameMatch := (Length(Trim(edName.Text)) = 0)
    or (Pos(Lowercase(edName.Text), Lowercase(setting.name)) > 0);
  bRecordsMatch := (Length(Trim(edName.Text)) = 0)
    or (Pos(Lowercase(edRecords.Text), Lowercase(setting.records)) > 0);
  // must match all filters
  Result := bFilenameMatch and bRecordsMatch;
end;

// repaint when splitter is moved
procedure TSettingsManager.SplitterMoved(Sender: TObject);
begin
  Self.Repaint;
end;

procedure TSettingsManager.ApplyFiltering;
var
  i: Integer;
  setting: TSmashSetting;
begin
  // prepare to change the entries displayed
  lvSettings.Items.Count := 0;
  tempSmashSettings.Clear;

  // loop through the SmashSettings and add
  for i := 0 to Pred(SmashSettings.Count) do begin
    setting := TSmashSetting(SmashSettings[i]);
    if MatchesFilters(setting) then
      tempSmashSettings.Add(setting);
  end;

  // sort after filtering
  tempSmashSettings.Sort(CompareEntries);

  // update entries count and repaint
  lvSettings.Items.Count := tempSmashSettings.Count;
  lvSettingsChange(nil, nil, TItemChange(nil));
  lvSettings.Repaint;

  // update details
  UpdateSmashSettingsDetails;
end;

// re-filter when the user changes a filter TComboBox
procedure TSettingsManager.cbChange(Sender: TObject);
begin
  ApplyFiltering;
end;

// re-filter when the user exits a filter TEdit
procedure TSettingsManager.edExit(Sender: TObject);
begin
  ApplyFiltering;
end;

// re-filter when the user presses enter in a filter TEdit
procedure TSettingsManager.edKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    ApplyFiltering;
    Key := #0;
  end;
end;

end.
