unit mpDictionaryForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, ValEdit, CommCtrl,
  mpBackend, mteLogger, mteHelpers;

type
  TDictionaryForm = class(TForm)
    lvEntries: TListView;
    meNotes: TMemo;
    Splitter: TSplitter;
    pnlEntries: TPanel;
    pnlDetails: TPanel;
    pnlDictionaryInfo: TPanel;
    vlInfo: TValueListEditor;
    pnlReportNotes: TPanel;
    lblDictionary: TLabel;
    lblNotes: TLabel;
    gbFiltering: TGroupBox;
    lblFilename: TLabel;
    edFilename: TEdit;
    lblRecords: TLabel;
    cbVersion: TComboBox;
    edRating: TEdit;
    lblVersion: TLabel;
    cbReports: TComboBox;
    edRecords: TEdit;
    lblRating: TLabel;
    cbRecords: TComboBox;
    edVersion: TEdit;
    edReports: TEdit;
    cbRating: TComboBox;
    lblReports: TLabel;
    pnlSelectGame: TPanel;
    lblGame: TLabel;
    cbGame: TComboBox;
    procedure GetDictionary;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvEntriesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvEntriesData(Sender: TObject; Item: TListItem);
    procedure vlInfoDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure lvEntriesColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvEntriesDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    function MatchesFilters(entry: TEntry): boolean;
    procedure ApplyFiltering;
    procedure cbChange(Sender: TObject);
    procedure edExit(Sender: TObject);
    procedure edKeyPress(Sender: TObject; var Key: Char);
    procedure SplitterMoved(Sender: TObject);
    procedure cbGameChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FilterFilename: string;
    Game: string;
  end;

const
  red = TColor($0000FF);
  yellow = TColor($00A8E5);
  green = TColor($009000);

var
  DictionaryForm: TDictionaryForm;
  columnToSort: integer;
  ascending: boolean;
  tempDictionary, dictionary, pluginBlacklist: TList;

implementation

{$R *.dfm}

// returns the number of unique plugins in a dictionary
function PluginCount(var list: TList): integer;
var
  i: Integer;
  entry: TEntry;
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Sorted := true;
  sl.Duplicates := dupIgnore;
  for i := 0 to Pred(list.Count) do begin
    entry := TEntry(list[i]);
    sl.Add(entry.filename);
  end;
  Result := sl.Count;
  sl.Free;
end;

// return the number of reports in a dictionary
function ReportCount(var list: TList): integer;
var
  i: Integer;
  entry: TEntry;
begin
  Result := 0;
  for i := 0 to Pred(list.Count) do begin
    entry := TEntry(list[i]);
    Result := Result + StrToInt(entry.reports);
  end;
end;

procedure TDictionaryForm.GetDictionary;
var
  dictionaryFilename: string;
  sl: TStringList;
begin
  // prepare to reload
  vlInfo.Strings.Clear;
  lvEntries.Items.Count := 0;

  // initialize dictionary details
  dictionary := TList.Create;
  pluginBlacklist := TList.Create;
  sl := TStringList.Create;
  dictionaryFilename := cbGame.Text + 'Dictionary.txt';
  LoadDictionary(dictionary, sl, dictionaryFilename);
  LoadPluginBlacklist(pluginBlacklist, dictionary);
  vlInfo.InsertRow('Filename', dictionaryFilename, true);
  vlInfo.InsertRow('File size', FormatByteSize(GetFileSize(dictionaryFilename)), true);
  vlInfo.InsertRow('Date modified', DateTimeToStr(GetLastModified(dictionaryFilename)), true);
  vlInfo.InsertRow('Number of entries', IntToStr(dictionary.Count), true);
  vlInfo.InsertRow('Number of plugins', IntToStr(PluginCount(dictionary)), true);
  vlInfo.InsertRow('Number of reports', IntToStr(ReportCount(dictionary)), true);
  vlInfo.InsertRow('Blacklist size', IntToStr(pluginBlacklist.Count), true);

  // autosize filename column
  lvEntries.Columns[0].AutoSize := true;

  // free
  sl.Free;
end;

procedure TDictionaryForm.FormCreate(Sender: TObject);
begin
  // initialize list view, dictionary list
  cbGame.Text := game;
  tempDictionary := TList.Create;
  columnToSort := -1;
  lvEntries.OwnerDraw := true; //not settings.simpleDictionaryView;
end;

procedure TDictionaryForm.FormShow(Sender: TObject);
begin
  // initialize list of entries
  GetDictionary;
  edFilename.Text := FilterFilename;
  if FilterFilename <> '' then
    Self.FocusControl(lvEntries);
  ApplyFiltering;

// force lvEntries to autosize columns
  lvEntries.Width := lvEntries.Width - 1;
  lvEntries.Width := lvEntries.Width + 1;
end;

// custom ValueListEditor draw for unselectable cells
procedure TDictionaryForm.vlInfoDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
begin
  vlInfo.Canvas.Brush.Color := clWhite;
  vlInfo.Canvas.FillRect(Rect);
  Rect.Left := Rect.Left + 2;
  DrawText(vlInfo.Canvas.Handle, PChar(vlInfo.Cells[aCol, ARow]), -1, Rect,
    DT_SINGLELINE or DT_LEFT OR DT_VCENTER or DT_NOPREFIX);
end;

// update meNotes when user changes entry
procedure TDictionaryForm.lvEntriesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  entry: TEntry;
begin
  if lvEntries.ItemIndex = -1 then begin
    meNotes.Text := '';
    exit;
  end;

  entry := TEntry(tempDictionary[lvEntries.ItemIndex]);
  meNotes.Text := StringReplace(entry.notes, '@13', #13#10, [rfReplaceAll]);
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
  entry1, entry2: TEntry;
begin
  Result := 0;
  entry1 := TEntry(P1);
  entry2 := TEntry(P2);

  if columnToSort = 0 then
    Result := AnsiCompareText(entry1.filename, entry2.filename)
  else if columnToSort = 1 then
    Result := StrToInt(entry1.records) - StrToInt(entry2.records)
  else if columnToSort = 2 then
    Result := CompareAsFloat(entry1.version, entry2.version)
  else if columnToSort = 3 then
    Result := CompareAsFloat(entry1.rating, entry2.rating)
  else if columnToSort = 4 then
    Result := StrToInt(entry1.reports) - StrToInt(entry2.reports);

  if ascending then
    Result := -Result;
end;

procedure TDictionaryForm.lvEntriesColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  ascending := (columnToSort = Column.Index) and (not ascending);
  columnToSort := Column.Index;
  tempDictionary.Sort(CompareEntries);
  lvEntries.Repaint;
  lvEntriesChange(nil, nil, TItemChange(nil));
end;

procedure TDictionaryForm.lvEntriesData(Sender: TObject; Item: TListItem);
var
  entry: TEntry;
begin
  entry := TEntry(tempDictionary[Item.Index]);
  Item.Caption := entry.filename;
  Item.SubItems.Add(entry.records);
  Item.SubItems.Add(entry.version);
  Item.SubItems.Add(entry.rating);
  Item.SubItems.Add(entry.reports);
  lvEntries.Canvas.Font.Color := GetRatingColor(StrToFloat(entry.rating));
  lvEntries.Canvas.Font.Style := [fsBold];
end;

procedure TDictionaryForm.lvEntriesDrawItem(Sender: TCustomListView;
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
{ Filtering methods:
  Methods for filtering the dictionary.
  - IsNumericMatch
  - MatchesFilters
  - ApplyFilter
  - cbChange
  - edExit
  - edKeyDown
}
{******************************************************************************}

// evaluates whether @val @op @req is true.  E.g 130 > 100, or 1.75 = 1.75
function IsNumericMatch(op, req, val: string): boolean;
var
  f1, f2: Real;
  bGreater, bLess, bEqual: boolean;
begin
  try
    f1 := StrToFloat(req);
    f2 := StrToFloat(val);
    bGreater := (op = '>') and (f2 > f1);
    bLess := (op = '<') and (f2 < f1);
    bEqual := (op = '=') and (f2 = f1);
    Result := bGreater or bLess or bEqual;
  except on Exception do
    Result := true;
  end;
end;

function TDictionaryForm.MatchesFilters(entry: TEntry): boolean;
var
  bFilenameMatch, bRecordsMatch, bVersionMatch, bRatingMatch, bReportsMatch: boolean;
begin
  // get filter results
  bFilenameMatch := (Length(Trim(edFilename.Text)) = 0)
    or (Pos(Lowercase(edFilename.Text), Lowercase(entry.filename)) > 0);
  bRecordsMatch := (Length(Trim(edRecords.Text)) = 0)
    or IsNumericMatch(cbRecords.Text, edRecords.Text, entry.records);
  bVersionMatch := (Length(Trim(edVersion.Text)) = 0)
    or IsNumericMatch(cbVersion.Text, edVersion.Text, entry.version);
  bRatingMatch := (Length(Trim(edRating.Text)) = 0)
    or IsNumericMatch(cbRating.Text, edRating.Text, entry.rating);
  bReportsMatch := (Length(Trim(edReports.Text)) = 0)
    or IsNumericMatch(cbReports.Text, edReports.Text, entry.reports);
  // must match all filters
  Result := bFilenameMatch and bRecordsMatch and bVersionMatch
    and bRatingMatch and bReportsMatch;
end;

// repaint when splitter is moved
procedure TDictionaryForm.SplitterMoved(Sender: TObject);
begin
  Self.Repaint;
end;

procedure TDictionaryForm.ApplyFiltering;
var
  i: Integer;
  entry: TEntry;
begin
  // prepare to change the entries displayed
  lvEntries.Items.Count := 0;
  tempDictionary.Clear;

  // loop through the dictionary and add
  for i := 0 to Pred(dictionary.Count) do begin
    entry := TEntry(dictionary[i]);
    if MatchesFilters(entry) then
      TempDictionary.Add(entry);
  end;

  // sort after filtering
  tempDictionary.Sort(CompareEntries);

  // update entries count and repaint
  lvEntries.Items.Count := tempDictionary.Count;
  //Logger.Write(IntToStr(tempDictionary.Count) + ' entries found!');
  lvEntriesChange(nil, nil, TItemChange(nil));
  lvEntries.Repaint;
end;

// re-filter when the user changes a filter TComboBox
procedure TDictionaryForm.cbChange(Sender: TObject);
begin
  ApplyFiltering;
end;

procedure TDictionaryForm.cbGameChange(Sender: TObject);
begin
  // reload dictionary
  GetDictionary;
  // update ui
  ApplyFiltering;
end;

// re-filter when the user exits a filter TEdit
procedure TDictionaryForm.edExit(Sender: TObject);
begin
  ApplyFiltering;
end;

// re-filter when the user presses enter in a filter TEdit
procedure TDictionaryForm.edKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    ApplyFiltering;
    Key := #0;
  end;
end;

end.
