unit mtePluginSelectionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CommCtrl, Menus, ComCtrls, ImgList;

type
  TPluginListItem = class(TObject)
  public
    StateIndex: Integer;
    Fields: TStringList;
    constructor Create; virtual;
    destructor Destroy; override;
  end;
  TStringFunction = function(s: string): string of object;
  TStringListProcedure = procedure(fn: string; var sl: TStringList) of object;
  TPluginSelectionForm = class(TForm)
    lvPlugins: TListView;
    btnCancel: TButton;
    btnOK: TButton;
    PluginsPopupMenu: TPopupMenu;
    CheckAllItem: TMenuItem;
    UncheckAllItem: TMenuItem;
    ToggleAllItem: TMenuItem;
    StateImages: TImageList;
    MastersItem: TMenuItem;
    N1: TMenuItem;
    CheckMastersItem: TMenuItem;
    UncheckMastersItem: TMenuItem;
    CheckDependenciesItem: TMenuItem;
    UncheckDependenciesItem: TMenuItem;
    DependenciesItem: TMenuItem;
    procedure LoadFields(aListItem: TPluginListItem; sPlugin: string);
    procedure UpdateDisabled;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure CheckAllItemClick(Sender: TObject);
    procedure UncheckAllItemClick(Sender: TObject);
    procedure ToggleAllItemClick(Sender: TObject);
    procedure lvPluginsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvPluginsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvPluginsKeyPress(Sender: TObject; var Key: Char);
    procedure DrawCheckbox(aCanvas: TCanvas; var x, y: Integer; state: Integer);
    procedure DrawSubItems(ListView: TListView; var R: TRect; Item: TListItem);
    procedure DrawItem(ListView: TListView; var R: TRect; Item: TListItem);
    procedure lvPluginsDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure lvPluginsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    function GetMasterStatus(filename: string): Integer;
    procedure lvPluginsData(Sender: TObject; Item: TListItem);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckMastersItemClick(Sender: TObject);
    procedure UncheckMastersItemClick(Sender: TObject);
    procedure CheckDependenciesItemClick(Sender: TObject);
    procedure UncheckDependenciesItemClick(Sender: TObject);
    procedure PluginsPopupMenuPopup(Sender: TObject);
  private
    { Private declarations }
    slMasters, slDependencies, slMissing, slDisabled: TStringList;
    ListItems: TList;
    sLastHint: string;
    sBuffer: string;
    fLastBufferTime: TDateTime;
  public
    { Public declarations }
    GetPluginInfo: TStringFunction;
    GetPluginMasters: TStringListProcedure;
    GetPluginDependencies: TStringListProcedure;
    sColumns: string;
    slAllPlugins, slCheckedPlugins: TStringList;
  end;

var
  PluginSelectionForm: TPluginSelectionForm;

implementation

uses
  mteHelpers;

const
  // delay for clearing keystroke buffer when
  // performing a text search on a list view
  fBufferDelay = 1.1 * seconds;
  // checkbox states
  cChecked = 1;
  cUnChecked = 2;
  // master states
  mstNone = 0;
  mstMaster = 1;
  mstDependency = 2;
  mstBoth = 3;
  mstMissing = 4;
  mstDisabled = 5;

{$R *.dfm}

constructor TPluginListItem.Create;
begin
  StateIndex := cUnChecked;
  Fields := TStringList.Create;
end;

destructor TPluginListItem.Destroy;
begin
  Fields.Free;
end;

procedure TPluginSelectionForm.btnOKClick(Sender: TObject);
var
  i: Integer;
  ListItem: TListItem;
begin
  // clear checked plugins list
  slCheckedPlugins.Clear;
  // add checked plugins to slCheckedPlugins
  for i := 0 to Pred(lvPlugins.Items.Count) do begin
    ListItem := lvPlugins.Items[i];
    if ListItem.StateIndex = cChecked then
      slCheckedPlugins.Add(ListItem.Caption);
  end;
end;

procedure TPluginSelectionForm.LoadFields(aListItem: TPluginListItem;
  sPlugin: string);
var
  sl: TStringList;
  i: Integer;
begin
  // add plugin filename
  aListItem.fields.Add(sPlugin);

  // get comma separated plugin info in a TStringList
  sl := TStringList.Create;
  try
    sl.CommaText := GetPluginInfo(sPlugin);
    for i := 0 to Pred(sl.Count) do
      aListItem.Fields.Add(sl[i]);
  finally
    sl.Free;
  end;
end;

procedure TPluginSelectionForm.UpdateDisabled;
var
  i, j, index: Integer;
  filename: string;
  ListItem, MasterItem: TPluginListItem;
  sl: TStringList;
begin
  // update slDisabled
  slDisabled.Clear;
  sl := TStringList.Create;
  try
    for i := 0 to Pred(lvPlugins.Items.Count) do begin
      ListItem := TPluginListItem(ListItems[i]);
      filename := ListItem.Fields[0];
      // if unchecked, skip
      if ListItem.StateIndex = cUnChecked then
        continue;
      // if checked, make sure its masters are checked
      GetPluginMasters(filename, sl);
      for j := 0 to Pred(sl.Count) do begin
        index := slAllPlugins.IndexOf(sl[j]);
        // if master is not found, continue
        if (index = -1) then
          continue;
        // if master is unchecked, add to slDisabled
        MasterItem := TPluginListItem(ListItems[index]);
        if MasterItem.StateIndex = cUnChecked then
          slDisabled.Add(sl[j]);
      end;
      // clear masters
      sl.Clear;
    end;
  finally
    sl.Free;
  end;

  // disable OK button if there are any disabled masters
  btnOK.Enabled := slDisabled.Count = 0;
end;

procedure ToggleState(ListItem: TPluginListItem);
begin
  case ListItem.StateIndex of
    cChecked: ListItem.StateIndex := cUnChecked;
    cUnChecked: ListItem.StateIndex := cChecked;
  end;
end;

procedure TPluginSelectionForm.lvPluginsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  i: Integer;
  filename: string;
begin
  // update slMasters and slDependencies
  slMasters.Clear;
  slDependencies.Clear;
  for i := 0 to Pred(lvPlugins.Items.Count) do begin
    filename := TPluginListItem(ListItems[i]).Fields[0];
    with lvPlugins.Items[i] do
      if Selected then begin
        GetPluginMasters(filename, slMasters);
        GetPluginDependencies(filename, slDependencies);
      end;
  end;

  // repaint to update master/dependency colors
  lvPlugins.Repaint;
end;

function TPluginSelectionForm.GetMasterStatus(filename: string): Integer;
var
  bIsDependency, bIsMaster: boolean;
begin
  // if file has masters that are missing from slAllPlugins,
  // return mstMissing
  if slMissing.IndexOf(filename) > -1 then begin
    Result := mstMissing;
    exit;
  end;

  // if file has masters that are disabled,
  // return mstDisabled
  if slDisabled.IndexOf(filename) > -1 then begin
    Result := mstDisabled;
    exit;
  end;

  // compute master or dependency status based on selection
  bIsMaster := slMasters.IndexOf(filename) > -1;
  bIsDependency := slDependencies.IndexOf(filename) > -1;
  Result := IfThenInt(bIsMaster, 1, 0) + IfThenInt(bIsDependency, 2, 0);
end;

procedure TPluginSelectionForm.lvPluginsData(Sender: TObject; Item: TListItem);
var
  aListItem: TPluginListItem;
  MasterStatus: Integer;
  i: Integer;
begin
  // get item data
  aListItem := ListItems[Item.Index];
  Item.Caption := aListItem.Fields[0];
  Item.StateIndex := aListItem.StateIndex;
  // get subitems
  for i := 1 to Pred(aListItem.fields.Count) do
    Item.SubItems.Add(aListItem.fields[i]);

  // set font color based on master status of item
  lvPlugins.Canvas.Font.Style := [fsBold];
  MasterStatus := GetMasterStatus(Item.Caption);
  case MasterStatus of
    mstNone: begin
      lvPlugins.Canvas.Font.Style := [];
      lvPlugins.Canvas.Font.Color := clBlack;
    end;
    mstMaster: lvPlugins.Canvas.Font.Color := clGreen;
    mstDependency: lvPlugins.Canvas.Font.Color := clMaroon;
    mstBoth: lvPlugins.Canvas.Font.Color := clPurple;
    mstMissing: begin
      lvPlugins.Canvas.Font.Style := [fsItalic];
      lvPlugins.Canvas.Font.Color := clGray;
    end;
    mstDisabled: begin
      lvPlugins.Canvas.Font.Style := [fsItalic];
      lvPlugins.Canvas.Font.Color := clRed;
    end;
  end;
end;

procedure TPluginSelectionForm.DrawCheckbox(aCanvas: TCanvas; var x, y: Integer;
  state: Integer);
var
  icon: TIcon;
begin
  if state = 0 then
    exit;
  icon := TIcon.Create;
  StateImages.GetIcon(state, icon);
  aCanvas.Draw(x, y, icon);
  Inc(x, 17);
  icon.Free;
end;

procedure TPluginSelectionForm.DrawSubItems(ListView: TListView; var R: TRect;
  Item: TListItem);
var
  i: Integer;
begin
  for i := 0 to Pred(Item.SubItems.Count) do begin
    // redefine rect to draw in the space for the column
    // use trailing padding to keep items lined up on columns
    R.Left := R.Right;
    R.Right := R.Left + ListView_GetColumnWidth(ListView.Handle, i) - 3;

    // padding between items
    Inc(R.Left, 3);

    // draw text
    ListView.Canvas.TextRect(R, R.Left, R.Top, Item.SubItems[i]);
  end;
end;

procedure TPluginSelectionForm.DrawItem(ListView: TListView; var R: TRect;
  Item: TListItem);
begin
  // redefine rect to draw until the end of the first column
  // use trailing padding to keep items lined up on columns
  R.Right := R.Left + ListView.Columns[0].Width - 3;

  // draw the checkbox
  DrawCheckbox(ListView.Canvas, R.Left, R.Top, Item.StateIndex);

  // move text down 1 pixel
  Inc(R.Top, 1);
  // padding between checkbox and text
  Inc(R.Left, 6);

  // draw text
  ListView.Canvas.TextRect(R, R.Left, R.Top, Item.Caption);
end;

procedure TPluginSelectionForm.lvPluginsDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  ListView: TListView;
begin
  // draw background color
  ListView := TListView(Sender);
  if Item.Selected then begin
    ListView.Canvas.Brush.Color := $FFEEDD;
    ListView.Canvas.FillRect(Rect);
  end;

  // draw item
  DrawItem(ListView, Rect, Item);
  // draw subitem
  DrawSubItems(ListView, Rect, Item);
end;

procedure TPluginSelectionForm.lvPluginsKeyPress(Sender: TObject;
  var Key: Char);
var
  i, iFoundIndex: Integer;
  ListItem: TListItem;
  fBufferDiff: Real;
  sTempBuffer: string;
begin
  // Calculate time between current keystroke and last
  // keystroke we buffered
  fBufferDiff := Now - fLastBufferTime;

  // If we are within the buffer delay append the key to a
  // temporary buffer and search for next item matching the
  // buffer in the list view items.
  if fBufferDiff < fBufferDelay then begin
    fLastBufferTime := Now;
    sTempBuffer := sBuffer + Key;
    iFoundIndex := ListView_NextMatch(lvPlugins, sTempBuffer, 0);
    // If we found a match, handle it
    if iFoundIndex > -1 then begin
      ListView_HandleMatch(lvPlugins, iFoundIndex, sBuffer, sTempBuffer);
      Key := #0;
    end;
  end
  else begin
    // Allow user to use space to toggle checkbox state
    // for all selected items
    if Key = ' ' then begin
      for i := 0 to Pred(lvPlugins.Items.Count) do begin
        ListItem := lvPlugins.Items[i];
        if ListItem.Selected then
          if slMissing.IndexOf(slAllPlugins[i]) = -1 then
            ToggleState(TPluginListItem(ListItems[i]));
      end;
      // repaint to show updated checkbox state and exit
      UpdateDisabled;
      lvPlugins.Repaint;
      exit;
    end;

    // Restart buffering if we didn't have an active buffer
    // or press space
    fLastBufferTime := Now;
    sTempBuffer := Key;
    lvPlugins.ClearSelection;
    iFoundIndex := ListView_NextMatch(lvPlugins, sTempBuffer, 0);
    // If we found a match, handle it
    if iFoundIndex > -1 then begin
      ListView_HandleMatch(lvPlugins, iFoundIndex, sBuffer, sTempBuffer);
      Key := #0;
    end;
  end;
end;

function OnStateIcon(X, Y: Integer): Boolean;
begin
  Result := (x >= 2) and (x <= 14);
end;

procedure TPluginSelectionForm.lvPluginsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ListItem: TListItem;
begin
  // toggle checkbox state
  ListItem := lvPlugins.GetItemAt(X, Y);
  if OnStateIcon(X, Y) then begin
    if slMissing.IndexOf(slAllPlugins[ListItem.Index]) = -1 then
      ToggleState(TPluginListItem(ListItems[ListItem.Index]));

    // repaint to show updated checkbox state
    UpdateDisabled;
    lvPlugins.Repaint;
  end;
end;

procedure TPluginSelectionForm.lvPluginsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
  li : TListItem;
  hint: string;
  slTempMasters, slTempReq: TStringList;
begin
  // get list item at mouse position
  pt := lvPlugins.ScreenToClient(Mouse.CursorPos);
  li := lvPlugins.GetItemAt(pt.x, pt.y);
  // if mouse not over an item, exit
  if not Assigned(li) then
    exit;

  // get plugin masters and display them if they're present
  slTempMasters := TStringList.Create;
  try
    GetPluginMasters(li.Caption, slTempMasters);
    if slTempMasters.Count > 0 then
      hint := Format('Masters:'#13#10'%s'#13#10, [slTempMasters.Text]);
  finally
    slTempMasters.Free;
  end;

  // get plugin dependencies and display them if they're present
  slTempReq := TStringList.Create;
  try
    GetPluginDependencies(li.Caption, slTempReq);
    if slTempReq.Count > 0 then
      hint := hint + Format('Required By:'#13#10'%s', [slTempReq.Text]);
  finally
    slTempReq.Free;
  end;

  // trim the hint
  hint := Trim(hint);

  // activate hint if it differs from previously displayed hint
  if (hint <> sLastHint) then begin
    sLastHint := hint;
    lvPlugins.Hint := hint;
    Application.ActivateHint(Mouse.CursorPos);
  end;
end;

procedure TPluginSelectionForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  slMasters.Free;
  slDependencies.Free;
  slMissing.Free;
  slDisabled.Free;
  ListItems.Free;
end;

procedure TPluginSelectionForm.FormShow(Sender: TObject);
var
  i, j, iColumnSize: Integer;
  aListItem: TPluginListItem;
  sPlugin: string;
  sl: TStringList;
  aColumn: TListColumn;
begin
  // create lists
  slMasters := TStringList.Create;
  slDependencies := TStringList.Create;
  slMissing := TStringList.Create;
  slDisabled := TStringList.Create;
  ListItems := TList.Create;

  // create columns
  sl := TStringList.Create;
  try
    sl.CommaText := sColumns;
    iColumnSize := (lvPlugins.ClientWidth - 300) div (sl.Count - 1);
    for i := 0 to Pred(sl.Count) do begin
      aColumn := lvPlugins.Columns.Add;
      aColumn.Caption := sl[i];
      aColumn.Width := IfThenInt(i = 0, 300, iColumnSize);
    end;
    // make first column autosize
    lvPlugins.Columns[0].AutoSize := true;
  finally
    sl.Free;
  end;

  // add plugin items to list
  for i := 0 to Pred(slAllPlugins.Count) do begin
    sPlugin := slAllPlugins[i];
    aListItem := TPluginListItem.Create;
    // check ListItem if it's in the CheckedPlugins list
    if slCheckedPlugins.IndexOf(sPlugin) > -1 then
      aListItem.StateIndex := cChecked;
    // add merge subitems
    LoadFields(aListItem, sPlugin);
    ListItems.Add(aListItem);
  end;

  // determine which plugins can't be loaded because their masters
  // are missing
  sl := TStringList.Create;
  try
    for i := 0 to Pred(slAllPlugins.Count) do begin
      sPlugin := slAllPlugins[i];
      aListItem := TPluginListItem(ListItems[i]);
      GetPluginMasters(sPlugin, sl);
      for j := 0 to Pred(sl.Count) do
        if slAllPlugins.IndexOf(sl[j]) = -1 then begin
          slMissing.Add(sPlugin);
          aListItem.StateIndex := cUnChecked;
          break;
        end;
      sl.Clear;
    end;
  finally
    sl.Free;
  end;

  // set plugin count for display
  lvPlugins.Items.Count := slAllPlugins.Count;
  ListView_CorrectWidth(lvPlugins);

  // update disabled
  UpdateDisabled;
  lvPlugins.Repaint;
end;

procedure TPluginSelectionForm.PluginsPopupMenuPopup(Sender: TObject);
var
  bHasMasters, bHasDependencies: Boolean;
begin
  bHasMasters := slMasters.Count > 0;
  bHasDependencies := slDependencies.Count > 0;
  MastersItem.Enabled := bHasMasters;
  DependenciesItem.Enabled := bHasDependencies;
end;

procedure TPluginSelectionForm.CheckAllItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(lvPlugins.Items.Count) do
    if slMissing.IndexOf(slAllPlugins[i]) = -1 then
      TPluginListItem(ListItems[i]).StateIndex := cChecked;

  // repaint to show updated checkbox state
  UpdateDisabled;
  lvPlugins.Repaint;
end;

procedure TPluginSelectionForm.UncheckAllItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(lvPlugins.Items.Count) do
    TPluginListItem(ListItems[i]).StateIndex := cUnChecked;

  // repaint to show updated checkbox state
  UpdateDisabled;
  lvPlugins.Repaint;
end;

procedure TPluginSelectionForm.ToggleAllItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(lvPlugins.Items.Count) do
    if slMissing.IndexOf(slAllPlugins[i]) = -1 then
      ToggleState(TPluginListItem(ListItems[i]));

  // repaint to show updated checkbox state
  UpdateDisabled;
  lvPlugins.Repaint;
end;

procedure TPluginSelectionForm.CheckMastersItemClick(Sender: TObject);
var
  i, index: Integer;
begin
  // loop through masters of selected plugins
  for i := 0 to Pred(slMasters.Count) do begin
    index := slAllPlugins.IndexOf(slMasters[i]);
    // if the masters isn't loaded, skip it
    if index = -1 then
      continue;
    // else check it
    TPluginListItem(ListItems[index]).StateIndex := cChecked;
  end;

  // repaint to show updated checkbox state
  UpdateDisabled;
  lvPlugins.Repaint;
end;

procedure TPluginSelectionForm.UncheckMastersItemClick(Sender: TObject);
var
  i, index: Integer;
begin
  // loop through masters of selected plugins
  for i := 0 to Pred(slMasters.Count) do begin
    index := slAllPlugins.IndexOf(slMasters[i]);
    // if the masters isn't loaded, skip it
    if index = -1 then
      continue;
    // else uncheck it
    TPluginListItem(ListItems[index]).StateIndex := cUnChecked;
  end;

  // repaint to show updated checkbox state
  UpdateDisabled;
  lvPlugins.Repaint;
end;

procedure TPluginSelectionForm.CheckDependenciesItemClick(Sender: TObject);
var
  i, index: Integer;
begin
  // loop through dependencies of selected plugins
  for i := 0 to Pred(slDependencies.Count) do begin
    index := slAllPlugins.IndexOf(slDependencies[i]);
    // check it
    TPluginListItem(ListItems[index]).StateIndex := cChecked;
  end;

  // repaint to show updated checkbox state
  UpdateDisabled;
  lvPlugins.Repaint;
end;

procedure TPluginSelectionForm.UncheckDependenciesItemClick(Sender: TObject);
var
  i, index: Integer;
begin
  // loop through dependencies of selected plugins
  for i := 0 to Pred(slDependencies.Count) do begin
    index := slAllPlugins.IndexOf(slDependencies[i]);
    // uncheck it
    TPluginListItem(ListItems[index]).StateIndex := cUnChecked;
  end;

  // repaint to show updated checkbox state
  UpdateDisabled;
  lvPlugins.Repaint;
end;

end.
