{
  Mator Smash v0.9.6
  created by matortheeternal
  
  * DESCRIPTION *
  This script will make a patch similar to a bashed patch.
}

unit smash;

uses mteFunctions;

const
  vs = 'v0.9.6';
  settingsPath = scriptsPath + 'smash\settings\';
  dashes = '-----------------------------------------------------------';
  // these booleans control logging
  debugGetMaster = false;
  debugArrays = false;
  listOverrides = false;
  showChanges = false;
  showTraversal = false;
  showSkips = false;
  showTypeStrings = false;
  showRecTimes = false;
  verbose = false;
  disableStyles = false;
  // maximum records to be smashed
  maxRecords = 100000;
  splitChar = '#13';
 
var
  slRecords, slSettings, slOptions, slFiles, slSubrecords, 
  slGlobalSubrecords, recordTree, subrecordTree, include: TStringList;
  lstSettings: TList;
  userFile: IInterface;
  global_records, global_subrecords, global_recordMode, 
  global_subrecordMode, global_setting: string;
  makeNewLine: boolean;
  AssetPath: string;
  checkboxImages: TCustomImageList;
  


{===========================================================}
{ SETTING FORM }

const
  cChecked = 1;
  cUnChecked = 2;
  cPartiallyChecked = 3;

var
  sfrm: TForm;
  meInclude, meSubrecords: TMemo;
  tv: TTreeView;
  btnOk, btnCancel: TButton;
  

{
  SetChildren
  Sets the StateIndex attribute of all the children of @node 
  to @state.  Uses recursion.
}
procedure SetChildren(node: TTreeNode; state: Integer);
var
  tmp: TTreeNode;
begin
  // exit if we don't have a node to work with
  if not Assigned(node) then exit;

  // loop through children setting StateIndex to state
  // if child has children, recurse into that child
  tmp := node.getFirstChild;
  while Assigned(tmp) do begin
    tmp.StateIndex := state;
    if tmp.HasChildren then
      SetChildren(tmp, state);
    tmp := tmp.getNextSibling;
  end;
end;

{
  UpdateParent
  Calculates and sets the StateIndex attribute for @node based
  on the StateIndex values of its children.  Uses recursion to 
  update parents of the parent that was updated.
}
procedure UpdateParent(node: TTreeNode);
var
  tmp: TTreeNode;
  state: Integer;
begin
  // exit if we don't have a node to work with
  if not Assigned(node) then exit;

  // parent state is checked if all siblings are checked
  state := cChecked;
  tmp := node.getFirstChild;
  while Assigned(tmp) do begin
    if tmp.StateIndex <> cChecked then begin
      state := cPartiallyChecked;
      break;
    end;
    tmp := tmp.getNextSibling;
  end;

  // parent state is unchecked if all siblings are unchecked
  if state = cPartiallyChecked then begin
    state := cUnChecked;
    tmp := node.getFirstChild;
    while Assigned(tmp) do begin
      if tmp.StateIndex <> cUnChecked then begin
        state := cPartiallyChecked;
        break;
      end;
      tmp := tmp.getNextSibling;
    end;
  end;

  // set state, recurse to next parent
  node.StateIndex := state;
  tmp := node.Parent;
  UpdateParent(tmp);
end;

{
  CheckBoxManager
  Manages checkboxes in the TTreeView.  Changes the StateIndex 
  of the checkbox associated with @node.  Uses SetChildren and
  UpdateParent.  Called by tvClick and tvKeyDown.
}
procedure CheckBoxManager(node: TTreeNode);
begin
  // exit if we don't have a node to work with
  if not Assigned(node) then exit;
  
  // if unchecked or partially checked, set to checked and
  // set all children to checked, update parents
  if (node.StateIndex = cUnChecked)
  or (node.StateIndex = cPartiallyChecked) then begin
    node.StateIndex := cChecked;
    UpdateParent(node.Parent);
    SetChildren(node, cChecked);
  end
  // if checked, set to unchecked and set all children to
  // unchecked, update parents
  else if node.StateIndex = cChecked then begin
    node.StateIndex := cUnChecked;
    UpdateParent(node.Parent);
    SetChildren(node, cUnChecked);
  end;
end;

{
  tvClick
  Event handler for when the user clicks anywhere in the 
  TreeView.  If the user clicks on a checkbox it calls 
  CheckBoxManager to toggle the state of the selected
  checkbox.
}
procedure tvClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  HT: THitTests;
begin
  HT := tv.GetHitTestInfoAt(X, Y);
  if (HT - [htOnStateIcon] <> HT) then
    CheckBoxManager(tv.Selected);
end;

{
  tvKeyDown
  Event handler for when the user presses a key while the 
  TreeView is focused.  If the user presses space we call 
  CheckBoxManager to toggle the state of the selected 
  checkbox.
}
procedure tvKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) and Assigned(tv.Selected) then
    CheckBoxManager(tv.Selected);
end;

{
  tvExpanding
  Event handler for when the user expands a node in the
  TreeView.  If we haven't loaded the subrecords for the
  record represented by @node, we call LoadNodes.
}
procedure tvExpanding(Sender: TObject; node: TTreeNode; 
  var AllowExpansion: Boolean);
var
  i: integer;
begin
  if not Assigned(node.getFirstChild) then exit;
  
  // if we haven't loaded subrecord data yet, attempt to
  // load it from the subrecord stringlist
  if node.getFirstChild.Text = 'Elements' then begin
    // find record signature in subrecords stringlist
    // exit if not found
    i := subrecordTree.IndexOf(node.Text);
    if i = -1 then exit;
    
    // load nodes from the subrecords stringlist
    // starting at the first subrecord
    LoadNodes(subrecordTree, tv, node, i + 1, 2);
    
    // delete 'Elements' placeholder node
    node.getFirstChild.Delete;
  end;
end;

{
  LeadingSpaces
  Calculates the number of leading spaces in a PChar string
  and returns it as an integer.
}
function LeadingSpaces(var s: PChar): integer;
var
  i: integer;
begin
  Result := 0;
  i := 1;
  while (true) do begin
    if s[i] = ' ' then 
      Inc(Result)
    else
      exit;
    Inc(i);
  end;
end;

{
  LoadNodes
  Loads nodes from @sl into @tv as children of @node.  Starts
  at index @i in the stringlist and adds children which have
  leading spaces matching @cws.  If there are more leading 
  spaces than @cws, we use recursion to add the list item as
  a child of the current list item @curNode.  If there are
  fewer leading spaces than @cws we exit.
}
procedure LoadNodes(var sl: TStringList; tv: TTreeView; node: TTreeNode; 
  var i: integer; cws: integer);
var
  lws: integer;
  p: PChar;
  curNode: TTreeNode;
begin
  curNode := node;
  while (true) do begin
    // exit if we reached the end of the stringlist
    if i > sl.Count - 1 then exit; 
    p := sl[i];
    lws := LeadingSpaces(p);
    
    // if lws matches cws, add as child of current node
    // (we're on the right level)
    if lws = cws then begin
      curNode := tv.Items.AddChild(node, Trim(sl[i]));
      curNode.StateIndex := node.StateIndex;
    end 
    // if leading spaces of the current item exceeds cws,
    // recurse to add as child of the last added node
    // (we need to go down a level)
    else if lws > cws then begin
      LoadNodes(sl, tv, curNode, i, lws);
      continue;
    end
    // if cws exceeds lws, exit
    // (we need to go up a level)
    else
      exit;
    
    // go to next item in list
    Inc(i);
  end;
end;

{
  FindNode
  Finds a node matching @s as a child of @node.
}
function FindNode(node: TTreeNode; const s: string): TTreeNode;
var
  tmp: TTreeNode;
begin
  Result := nil;
  tmp := node.getFirstChild;
  while (Assigned(tmp)) do begin
    if (tmp.Text = s) then begin
      Result := tmp;
      exit;
    end;
    tmp := tmp.getNextSibling;
  end;
end;

{
  DisableNodes
  Disables nodes from @sl in @tv found as children of @node.  
  Starts at index @i in the stringlist and locates children 
  which have leading spaces matching @cws.  If there are more 
  leading spaces than @cws, we use recursion to find the list
  item in the current item @curNode.  If there are fewer leading 
  spaces than @cws we exit.
}
procedure DisableNodes(var sl: TStringList; tv: TTreeView; node: TTreeNode; 
  var i: integer; cws: integer);
var
  lws: integer;
  p: PChar;
  curNode: TTreeNode;
begin
  curNode := node;
  while (true) do begin
    // exit if we reached the end of the stringlist
    if i > sl.Count - 1 then exit; 
    p := sl[i];
    lws := LeadingSpaces(p);
    
    // if lws matches cws, find and disable
    if lws = cws then begin
      curNode := FindNode(node, Trim(sl[i]));
      if not Assigned(curNode) then begin
        Inc(i);
        continue;
      end;
      if i + 1 = sl.Count then begin
        curNode.StateIndex := cUnChecked;
      end
      else begin
        p := sl[i+1];
        lws := LeadingSpaces(p);
        if lws > cws then begin
          tvExpanding(nil, curNode, true);
          curNode.StateIndex := cPartiallyChecked;
        end
        else begin
          curNode.StateIndex := cUnChecked;
        end;
      end;
    end 
    // if leading spaces of the current item exceeds cws,
    // recurse to find in the last added node
    // (we need to go down a level)
    else if lws > cws then begin
      DisableNodes(sl, tv, curNode, i, lws);
      continue;
    end
    // if cws exceeds lws, exit
    // (we need to go up a level)
    else
      exit;
    
    // go to next item in list
    Inc(i);
  end;
end;

{
  MakeBold
  Changes the style of @lbl to bold.
}
procedure MakeBold(lbl: TLabel);
begin
  if not disableStyles then begin
    lbl.WordWrap := false;
    lbl.Font.Style := lbl.Font.Style + [fsBold];
  end;
end;

{
  SettingFormResize
  Event to fire when the setting form is resized
}
procedure SettingFormResize(Sender: TObject);
begin
  tv.Width := sfrm.Width - 48;
  tv.Height := sfrm.Height - 155;
  btnOk.Top := sfrm.Height - 86;
  btnOk.Left := sfrm.Width div 2 - btnOk.Width - 8;
  btnCancel.Top := btnOk.Top;
  btnCancel.Left := btnOk.Left + btnOk.Width + 16;
end;

{
  CreateIncludeList
  Recursively traverses a TreeView starting at @node adding node 
  text properties to @sl, with @depth concatenated before each text 
  item.
}
procedure CreateIncludeList(var sl: TStringList; node: TTreeNode; depth: string);
var
  tmp: TTreeNode;
begin
  if (not node.HasChildren) then exit;
  
  tmp := node.getFirstChild;
  while (Assigned(tmp)) do begin
    if (tmp.StateIndex = cUnChecked) then
      sl.Add(depth + tmp.Text)
    else if (tmp.HasChildren) and (tmp.StateIndex = cPartiallyChecked) then begin
      sl.Add(depth + tmp.Text);
      CreateIncludeList(sl, tmp, depth + '  ');
    end;
    tmp := tmp.getNextSibling;
  end;
end;

{
  SettingForm
  Used to create or edit setting presets.
}
procedure SettingForm(Sender: TObject);
var
  lblName, lblRecords, lblSubrecords: TLabel;
  edName: TEdit;
  ini, template: TMemIni;
  usingTemplate: boolean;
  caption: string;
  node: TTreeNode;
  ndx: integer;
begin
  // assign template
  include := TStringList.Create;
  caption := TButton(Sender).Caption;
  usingTemplate := caption <> 'New setting';
  if lst.ItemIndex > -1 then
    template := TMemIniFile(lstSettings[slSettings.IndexOf(lst.Items[lst.ItemIndex])])
  else
    usingTemplate := false;
  
  sfrm := TForm.Create(nil);
  try
    // set up form
    sfrm.Width := 400;
    sfrm.Height := 600;
    sfrm.Position := poScreenCenter;
    sfrm.Caption := 'Create new Smash Setting';
    sfrm.Constraints.MinHeight := 400;
    sfrm.Constraints.MinWidth := 286;
    
    // make label and edit for name
    lblName := cLabel(sfrm, sfrm, 16, 16, 0, 0, 'Name: ', '');
    edName := cEdit(sfrm, sfrm, lblName.Top, lblName.Left + lblName.Width + 8, 0, 200, '', '');
    
    // make tree view
    tv := TTreeView.Create(sfrm);
    tv.Parent := sfrm;
    tv.Left := lblName.Left;
    tv.Top := lblName.Top + lblName.Height + 20;
    tv.Width := sfrm.Width - 48;
    tv.Height := sfrm.Height - 155;
    tv.Indent := 19;
    tv.ReadOnly := true;
    tv.StateImages := checkboxImages;
    tv.OnMouseDown := tvClick;
    tv.OnKeyDown := tvKeyDown;
    tv.OnExpanding := tvExpanding;
    
    // construct ok and cancel buttons
    btnOk := TButton.Create(sfrm);
    btnOk.Parent := sfrm;
    btnOk.Left := sfrm.Width div 2 - btnOk.Width - 8;
    btnOk.Top := sfrm.Height - 86;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOK;
    
    btnCancel := TButton.Create(sfrm);
    btnCancel.Parent := sfrm;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := btnOk.Top;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    
    // load record nodes
    node := tv.Items.Add(nil, 'Records');
    if usingTemplate then
      node.StateIndex := cChecked
    else 
      node.StateIndex := cUnChecked;
    LoadNodes(recordTree, tv, node, 0, 0);
    
    // if using template, load values from it for form
    if usingTemplate then begin
      edName.Caption := template.ReadString('Setting', 'Name', '');
      if caption = 'Copy setting' then begin
        sfrm.Caption := 'Copy Smash Setting';
        edName.Caption := 'Copy of '+edName.Caption;
      end
      else if caption = 'Edit setting' then begin
        sfrm.Caption := 'Edit Smash Setting';
        edName.Enabled := false;
      end;
      
      include.LoadFromFile(settingsPath + edName.Caption + '.ini');
      ndx := include.IndexOf('[Tree]') + 1;
      if (ndx < include.Count) then begin
        if (include[ndx] <> '') then begin
          DisableNodes(include, tv, node, ndx, 0);
          node.StateIndex := cPartiallyChecked;
        end;
      end;
    end;
    
    // set onresize event
    sfrm.OnResize := SettingFormResize;
    
    // if user clicks ok, save to ini and update lists
    if sfrm.ShowModal = mrOk then begin
      ini := TMemIniFile.Create(settingsPath + edName.Caption + '.ini');
      ini.WriteString('Setting', 'Name', edName.Caption);
      ini.UpdateFile;
      
      include.LoadFromFile(settingsPath + edName.Caption + '.ini');
      ndx := include.IndexOf('[Tree]');
      if (ndx = -1) then
        include.Add('[Tree]')
      else begin
        Inc(ndx);
        while (include.Count > ndx) do
          include.Delete(ndx);
      end;
      CreateIncludeList(include, node, '');
      include.SaveToFile(settingsPath + edName.Caption + '.ini');
      
      if (slSettings.IndexOf(edName.Caption) = -1) then begin
        lstSettings.Add(ini);
        slSettings.Add(edName.Caption);
        lst.Items.Add(edName.Caption);
      end
      else
        lstSettings[slSettings.IndexOf(edName.Caption)] := ini;
    end;
  finally
    sfrm.Free;
  end;
end;


{===========================================================}
{ SETTING MANAGER FORM }

var
  btnDetails, btnEdit, btnCopy, btnDel: TButton;
  gear: TPicture;
  lst: TListBox;

  
{
  ToggleButtons
  Toggles btnEdit, btnCopy, btnDelete.
}
procedure ToggleButtons(Sender: TObject);
var
  b: boolean;
begin
  b := (TListBox(Sender).ItemIndex > -1);
  btnEdit.Enabled := b; btnCopy.Enabled := b; btnDel.Enabled := b;
end;

{
  DeleteSetting
  Deletes the currently selected setting.
}
procedure DeleteSetting(Sender: TObject);
var
  s: string;
begin
  if lst.ItemIndex > -1 then begin
    s := lst.Items[lst.ItemIndex];
    lstSettings.Delete(slSettings.IndexOf(s));
    slSettings.Delete(slSettings.IndexOf(s));
    lst.Items.Delete(lst.ItemIndex);
    lst.ItemIndex := -1;
    DeleteFile(settingsPath + s + '.ini');
  end;
end;

{
  UpdateSettings
  Updates the setting comboboxes for OptionsForm
}
procedure UpdateSettings;
var
  i, ndx: integer;
  cb: TComboBox;
  s: string;
begin
  for i := 0 to pnlCount - 1 do begin
    cb := TComboBox(pnlArray[i].Components[1]);
    s := cb.Items[cb.ItemIndex];
    cb.Items.Text := slSettings.Text;
    if cb.Items.IndexOf(s) > -1 then
      cb.ItemIndex := cb.Items.IndexOf(s)
    else
      cb.ItemIndex := 0;
  end;
  
  // update global setting combobox
  s := gscb.Items[gscb.ItemIndex];
  gscb.Items.Text := slSettings.Text;
  if gscb.Items.IndexOf(s) > -1 then
    gscb.ItemIndex := gscb.Items.IndexOf(s)
  else
    gscb.ItemIndex := 0;
end;

{
  SettingManager
  Used by the user to view, edit, and create settings.
}
procedure SettingManager;
var
  ofrm: TForm;
  i, h: integer;
  btnNew, btnOk : TButton;
begin
  ofrm := TForm.Create(nil);
  try
    ofrm.Caption := 'Advanced Options';
    ofrm.Width := 400;
    ofrm.Position := poScreenCenter;
    ofrm.Height := 300;
    
    h := slSettings.Count * 15 + 85;
    if h > 500 then
      h := 500;
    if h > ofrm.Height then
      ofrm.Height := h;
    
    // list box of settings
    lst := TListBox.Create(ofrm);
    lst.Parent := ofrm;
    lst.Top := 8;
    lst.Left := 8;
    lst.Height := ofrm.Height - 105;
    lst.Width := ofrm.Width - 145;
    for i := 0 to slSettings.Count - 1 do
      lst.Items.Add(slSettings[i]);
    lst.OnClick := ToggleButtons;
    
    // new setting button
    btnNew := cButton(ofrm, ofrm, 8, lst.Left + lst.Width + 8, 0, 100, 'New setting');
    btnNew.OnClick := SettingForm;
    // edit setting button
    btnEdit := cButton(ofrm, ofrm, btnNew.Top + btnNew.Height + 8, btnNew.Left, 0, 100, 'Edit setting');
    btnEdit.OnClick := SettingForm;
    btnEdit.Enabled := false;
    // copy setting button
    btnCopy := cButton(ofrm, ofrm, btnEdit.Top + btnEdit.Height + 8, btnNew.Left, 0, 100, 'Copy Setting');
    btnCopy.OnClick := SettingForm;
    btnCopy.Enabled := false;
    // delete setting button
    btnDel := cButton(ofrm, ofrm, btnCopy.Top + btnCopy.Height + 8, btnNew.Left, 0, 100, 'Delete setting');
    btnDel.OnClick := DeleteSetting;
    btnDel.Enabled := false;
    // OK button
    btnOk := cButton(ofrm, ofrm, ofrm.Height - 80, ofrm.Width div 2 - 40, 0, 0, 'OK');
    btnOk.ModalResult := mrOk;
    
    ofrm.ShowModal;
  finally
    ofrm.free;
  end;
  UpdateSettings;
end;


{===========================================================}
{ PLUGIN DETAILS FORM }


{
  GetGroupOverrides
}
function GetGroupOverrides(f: IInterface): string;
var
  i: integer;
  e: IInterface;
begin
  for i := 0 to ElementCount(f) - 1 do begin
    e := ebi(f, i);
    if Signature(e) = 'TES4' then continue;
    Result := Result + GroupSignature(e) + ': '+IntToStr(OverrideRecordCount(e))+' overrides'#13#10;
  end;
end;

{
  PluginDetails
  Form which shows advanced details on a plugin
}
procedure PluginDetails(Sender: TObject);
var
  f, e: IInterface;
  i: integer;
  fn, author, records, overrides, desc, masters, groups: string;
  pfrm: TForm;
  lbl: TLabel;
  sb: TScrollBox;
  memo: TMemo;
begin
  // find file
  fn := TLabel(Sender).Caption;
  fn := Copy(fn, Pos(']', fn) + 2, Length(fn));
  f := FileByName(fn);
  
  // get data
  author := geev(ebi(f, 0), 'CNAM');
  records := IntToStr(RecordCount(f));
  overrides := IntToStr(OverrideRecordCount(f));
  desc := geev(ebi(f, 0), 'SNAM');
  e := ebn(ebi(f, 0), 'Master Files');
  for i := 0 to ElementCount(e) - 1 do
    masters := masters + geev(ebi(e, i), 'MAST') + #13#10;
  groups := GetGroupOverrides(f);
    
  // display form
  pfrm := TForm.Create(nil);
  try
    pfrm.Caption := fn;
    pfrm.Width := 400;
    pfrm.Height := 600;
    pfrm.Position := poScreenCenter;
    
    lbl := cLabel(pfrm, pfrm, 8, 8, 0, 150, 'Filename:', '');
    MakeBold(lbl);
    lbl := cLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, fn, '');
    lbl := cLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Author:', '');
    MakeBold(lbl);
    lbl := cLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, author, '');
    lbl := cLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Number of records:', '');
    MakeBold(lbl);
    lbl := cLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, records, '');
    lbl := cLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Number of overrides:', '');
    MakeBold(lbl);
    lbl := cLabel(pfrm, pfrm, lbl.Top, 160, 0, 200, overrides, '');
    lbl := cLabel(pfrm, pfrm, lbl.Top + 22, 8, 0, 150, 'Description:', '');
    MakeBold(lbl);
    memo := cMemo(pfrm, pfrm, lbl.Top + 22, 16, 100, 348, true, true, ssVertical, desc);
    lbl := cLabel(pfrm, pfrm, memo.Top + memo.Height + 16, 8, 0, 150, 'Masters:', '');
    MakeBold(lbl);
    memo := cMemo(pfrm, pfrm, lbl.Top + 22, 16, 100, 348, true, true, ssVertical, masters);
    lbl := cLabel(pfrm, pfrm, memo.Top + memo.Height + 16, 8, 0, 150, 'Record groups:', '');
    MakeBold(lbl);
    memo := cMemo(pfrm, pfrm, lbl.Top + 22, 16, 150, 348, true, true, ssVertical, groups);
    
    pfrm.ShowModal;
  finally
    pfrm.Free;
  end;
end;


{===========================================================}
{ OPTIONS FORM }

var
  frm: TForm;
  gscb: TComboBox;
  pnlArray: Array[0..255] of TPanel;
  pnlCount: integer;

  
{
  OptionsForm
  Form from which the user can access the setting manager,
  plugin details, and set the settings to be used when smashing
  the loaded plugins.
}
function OptionsForm: boolean;
var
  i, height, m: integer;
  btnSmash, btnCancel: TButton;
  optionslbl, fnlbl, gslbl: TLabel;
  cb: TComboBox;
  f: IInterface;
  fn, author, s: string;
  imgOptions: TImage;
  pnl: TPanel;
  holder: TObject;
  sb: TScrollBar;
begin
  Result := false;
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Mator Smash Options';
    frm.Width := 550;
    frm.Position := poScreenCenter;
    frm.Height := 400;
    for i := 0 to FileCount - 1 do begin
      f := FileByIndex(i);
      fn := GetFileName(f);
      author := geev(ebi(f, 0), 'CNAM');
      if (Pos(fn, bethesdaFiles) > 0) or (Pos('Mator Smash', author) > 0) then Continue;
      Inc(m);
    end;
    height := m*40 + 170;
    if height > (Screen.Height - 100) then begin
      frm.Height := Screen.Height - 100;
      sb := TScrollBox.Create(frm);
      sb.Parent := frm;
      sb.Height := Screen.Height - 210;
      sb.Width := frm.Width - 20;
      sb.Align := alTop;
      holder := sb;
    end
    else begin
      frm.Height := height;
      holder := frm;
    end;
    
    optionslbl := cLabel(frm, holder, 8, 8, 0, 450, 
      'Set the options you want to use for smashing the following plugins:', '');
    
    pnlCount := 0;
    for i := 0 to FileCount - 1 do begin
      f := FileByIndex(i);
      fn := GetFileName(f);
      author := geev(ebi(f, 0), 'CNAM');
      if Pos(fn, bethesdaFiles) > 0 then
        continue;
      if Pos('Mator Smash', author) > 0 then
        continue;
      
      pnlArray[pnlCount] := TPanel.Create(frm);
      pnlArray[pnlCount].Parent := holder;
      pnlArray[pnlCount].Left := 0;
      pnlArray[pnlCount].Top := 30 + pnlCount*40;
      pnlArray[pnlCount].Width := holder.Width - 25;
      pnlArray[pnlCount].BevelOuter := bvNone;
      pnlArray[pnlCount].BevelInner := bvNone; // or bvLowered
      pnlArray[pnlCount].BorderStyle := bsNone; // or bsSingle
      
      fnlbl := cLabel(pnlArray[pnlCount], pnlArray[pnlCount], 14, 24, 0, 0, '['+IntToHex(i - 1, 2)+'] '+fn, '');
      fnlbl.OnClick := PluginDetails;
      MakeBold(fnlbl);
      
      cb := TComboBox.Create(pnlArray[pnlCount]);
      cb.Parent := pnlArray[pnlCount];
      cb.Style := csDropDownList;
      cb.Items := slSettings;
      cb.ItemIndex := 0;
      if slSettings.IndexOf('default') > -1 then
        cb.ItemIndex := slSettings.IndexOf('default');
      cb.Top := 12;
      cb.Width := 150;
      cb.Left := holder.Width - cb.Width - 40;
      
      slFiles.Add(fn);
      Inc(pnlCount);
    end;
    
    // create global setting controls
    gslbl := cLabel(frm, holder, pnlArray[pnlCount - 1].Top + pnlArray[pnlCount - 1].Height + 16,
      optionslbl.Left, 0, 70, 'Global setting: ', '');
    gscb := TComboBox.Create(frm);
    gscb.Parent := holder;
    gscb.Top := gslbl.Top;
    gscb.Left := gslbl.Left + gslbl.Width + 16;
    gscb.Width := 150;
    gscb.Style := csDropDownList;
    gscb.Items := slSettings;
    gscb.ItemIndex := 0;
    if slSettings.IndexOf('default') > -1 then
      gscb.ItemIndex := slSettings.IndexOf('default');
    
    pnl := TPanel.Create(frm);
    pnl.Parent := frm;
    pnl.BevelOuter := bvNone;
    pnl.Align := alBottom;
    pnl.Height := 50;
    
    imgOptions := cImage(pnl, pnl, pnl.Height - 40, frm.Width - 50, 24, 24, gear, 'Advanced Options');
    imgOptions.OnClick := SettingManager;
    
    // create ok/cancel buttons
    btnSmash := cButton(frm, pnl, pnl.Height - 40, frm.Width div 2 - 88, 0, 0, 'Smash!');
    btnSmash.ModalResult := mrOk;
    btnCancel := cButton(frm, pnl, btnSmash.Top, btnSmash.Left + btnSmash.Width + 16, 0, 0, 'Cancel');
    btnCancel.ModalResult := mrCancel;
    
    if frm.ShowModal = mrOk then begin
      for i := 0 to pnlCount - 1 do begin
        s := TComboBox(pnlArray[i].Components[1]).Text;
        slOptions.Add(s);
      end;
      global_setting := gscb.Text;
      Result := true;
    end;
  finally
    frm.Free;
  end;
end;


{===========================================================}
{ SMASHING METHODS }

var
  pb: TProgressBar;
  memo: TMemo;
  lbl: TLabel;


{
  LogMessage
  Adds @msg to the log stringlist
}
procedure LogMessage(msg: string);
begin
  memo.Lines.add(msg);
  Application.processmessages;
end;

{
  GetMasterElement
  Gets the first instance of an element (the master)
}
function GetMasterElement(src, se, dstrec: IInterface): IInterface;
var
  i, j, ndx: integer;
  p, sk: string;
  ovr, ae, ne, mst: IInterface;
  sorted: boolean;
begin
  Result := nil;
  mst := MasterOrSelf(dstrec);
  p := IndexedPath(src);
  sk := SortKey(se, false);
  sorted := not (sk = '');
  // if sorted, look for an element matching sort key
  if sorted then begin
    if debugGetMaster then LogMessage('  Called GetMasterElement at path '+p+' looking for SortKey '+SortKey(se, false));
    // loop from override 0 to the second to last override
    for i := 0 to OverrideCount(mst) - 2 do begin
      ovr := OverrideByIndex(mst, i);
      ae := ebp(mst, p);
      for j := 0 to ElementCount(ae) - 1 do begin
        ne := ebi(ae, j);
        if (SortKey(ne, false) = sk) then begin
          Result := ne;
          break;
        end;
      end;
      // break if we found a subrecord matching the sortkey
      if Result <> nil then
        break;
    end;
  end 
  // if unsorted, look for the element using gav
  else begin
    sk := gav(se);
    if debugGetMaster then LogMessage('  Called GetMasterElement at path '+p+' looking for '+sk);
    ae := ebp(mst, p);
    for i := 0 to OverrideCount(mst) - 1 do begin
      ovr := OverrideByIndex(mst, i);
      ae := ebp(ovr, p);
      for j := 0 to ElementCount(ae) - 1 do begin
        ne := ebi(ae, j);
        if (gav(ne) = sk) then begin
          Result := ne;
          break;
        end;
      end;
    end;
  end;
end;
  
{
  nbsOverrideCount
  Non-Bethesda Override Count
}
function nbsOverrideCount(r: IInterface): integer;
var
  i: integer;
  fn: string;
begin
  Result := 0;
  for i := 0 to OverrideCount(r) - 1 do begin
    fn := GetFileName(GetFile(OverrideByIndex(r, i)));
    if Pos(fn, bethesdaFiles) = 0 then
      Result := Result + 1;
  end;
end;

{
  BuildSortKeyList
  Puts the sort keys of elements in a stringlist
}
procedure BuildSortKeyList(element: IInterface; var sl: TStringList);
var
  i, n: integer;
  childElement: IInterface;
  sk, skAdj: string;
begin
  for i := 0 to ElementCount(element) - 1 do begin
    childElement := ebi(element, i);
    sk := SortKey(childElement, false);
    skAdj := sk;
    n := 0;
    while sl.IndexOf(skAdj) > -1 do begin
      Inc(n);
      skAdj := sk + '-' + IntTostr(n);
    end;
    if debugArrays and (n > 0) then LogMessage('    Adjusted SortKey: '+skAdj);
    sl.Add(skAdj);
  end;
end;

{
  MergeSortedArray
  Merges sorted array elements
}
procedure MergeSortedArray(mst, src, dst, dstrec: IInterface; depth: string; ini: TMemIniFile);
var
  i, m_ndx, s_ndx, d_ndx, n: integer;
  me, se, de, ne: IInterface;
  slMst, slDst, slSrc: TStringList;
  useValues: boolean;
  dts, ets, sk: string;
begin
  // Step 1: build lists of elements in each array for easy comparison
  slMst := TStringList.Create;
  slSrc := TStringList.Create;
  slDst := TStringList.Create;
  slDst.Sorted := true;
  BuildSortKeyList(mst, slMst);
  BuildSortKeyList(src, slSrc);
  BuildSortKeyList(dst, slDst);
  
  // Step 2: Remove elements that are in mst and dst, but missing from src
  for i := 0 to slMst.Count - 1 do begin
    s_ndx := slSrc.IndexOf(slMst[i]);
    d_ndx := slDst.IndexOf(slMst[i]);
    
    if (s_ndx = -1) and (d_ndx > -1) then begin
      if debugArrays then LogMessage('      > Removing element '+Path(ebi(dst, d_ndx))+' with key: '+slDst[d_ndx]);
      RemoveElement(dst, ebi(dst, d_ndx));
      slDst.Delete(d_ndx);
    end;
  end;
  
  // Step 3: Copy array elements in src that aren't in mst.
  for i := 0 to slSrc.Count - 1 do begin
    d_ndx := slDst.IndexOf(slSrc[i]);
    m_ndx := slMst.IndexOf(slSrc[i]);
    
    se := ebi(src, i);
    dts := DefTypeString(se);
    ets := ElementTypeString(se);
    if (d_ndx = -1) and (m_ndx = -1) then begin
      if debugArrays then LogMessage('      > Adding element '+IntToStr(i)+' at '+Path(dst)+' with key: '+slSrc[i]);
      ne := ElementAssign(dst, HighInteger, se, false);
      if debugArrays then LogMessage('      > '+gav(ne));
      slDst.Add(slSrc[i]);
    end
    // Step 3.5: If array element is in dst and has subelements, traverse it.
    else if (d_ndx > -1) and ((dts = 'dtStruct') or (ets = 'etSubRecordArray')) then begin
	    if showTraversal then LogMessage('      > Traversing element '+Path(se)+' with key: '+slSrc[i]);
      if showTraversal and debugArrays then LogMessage('      > Source Element: '+gav(se)+
        #13#10'      > Destination Element: '+gav(ebi(dst, d_ndx)));
      try
        rcore(se, GetMasterElement(src, se, dstrec), ebi(dst, d_ndx), dstrec, depth + '    ', ini);
      except on x : Exception do begin
          LogMessage('      !! rcore exception: '+x.Message);
        end;
      end;
    end
    else if (d_ndx > -1) and (ets = 'etSubRecordStruct') then begin
	    if showTraversal then LogMessage('      > Traversing element '+Path(se)+' with key: '+slSrc[i]);
      if showTraversal and debugArrays then LogMessage('      > Source Element: '+gav(se)+
        #13#10'      > Destination Element: '+gav(ebi(dst, d_ndx)));
      try
        rcore(se, GetMasterElement(src, se, dstrec), ebi(dst, d_ndx), dstrec, depth + '    ', ini);
      except on x : Exception do begin
          LogMessage('      !! rcore exception: '+x.Message);
        end;
      end;
    end;
  end;
  
  // Step 4: Free lists.
  slMst.Free;
  slSrc.Free;
  slDst.Free;
end;

{
  BuildElementList
  Puts the values of elements in a stringlist
}
procedure BuildElementList(element: IInterface; var sl: TStringList);
var
  i, n: integer;
  childElement: IInterface;
  values, valuesAdj: string;
begin
  for i := 0 to ElementCount(element) - 1 do begin
    childElement := ebi(element, i);
    values := gav(childElement);
    valuesAdj := values;
    n := 0;
    while (sl.IndexOf(valuesAdj) > -1) do begin
      Inc(n);
      valuesAdj := values + IntToStr(n);
    end;
    sl.Add(valuesAdj);
  end;
end;

{
  MergeUnsortedArray
  Merges unsorted array elements
}
procedure MergeUnsortedArray(mst, src, dst, dstrec: IInterface; depth: string; ini: TMemIniFile);
var
  i, m_ndx, s_ndx, d_ndx: integer;
  me, se, de: IInterface;
  slMst, slSrc, slDst: TStringList;
  useValues: boolean;
  dts, ets: string;
begin
  // Step 1: build lists of elements in each array for easy comparison
  slMst := TStringList.Create;
  slSrc := TStringList.Create;
  slDst := TStringList.Create;
  BuildElementList(mst, slMst);
  BuildElementList(src, slSrc);
  BuildElementList(dst, slDst);
  
  // Step 2: Remove elements that are in mst and dst, but missing from src
  for i := 0 to slMst.Count - 1 do begin
    s_ndx := slSrc.IndexOf(slMst[i]);
    d_ndx := slDst.IndexOf(slMst[i]);
    
    if (s_ndx = -1) and (d_ndx > -1) then begin
      if debugArrays then LogMessage('      > Removing element at '+Path(dst)+' with values: '+slMst[i]);
      RemoveElement(dst, d_ndx);
      slDst.Delete(d_ndx);
    end;
  end;
  
  // Step 3: Copy array elements in src that aren't in mst or dst
  for i := 0 to slSrc.Count - 1 do begin
    d_ndx := slDst.IndexOf(slSrc[i]);
    m_ndx := slMst.IndexOf(slSrc[i]);
    se := ebi(src, i);
    //if debugArrays then LogMessage('Looking to copy '+slSrc[i]);
    if (m_ndx = -1) and (d_ndx = -1) then begin
      if debugArrays then LogMessage('      > Adding element at '+Path(dst)+' with values: '+slSrc[i]);
      ElementAssign(dst, HighInteger, se, false);
      slDst.Add(slSrc[i]);
    end;
  end;
  
  // Step 4: Free lists.
  slMst.Free;
  slSrc.Free;
  slDst.Free;
end;

{
  ListHasMatch
  Check if a list has a matching subrecord
}
function ListHasMatch(var sl: TStringList; input: string): boolean;
var
  i: integer;
  ex: string;
begin
  Result := false;
  for i := 0 to sl.Count - 1 do begin
    ex := sl[i];
    if (Pos('*', ex) > 0) then begin
      SetChar(ex, Pos('*', ex), '');
      if (Pos(ex, input) > 0) then begin
        Result := true;
        exit;
      end;
    end
    else if (ex = input) then begin
      Result := true;
      exit;
    end;
  end;
end;

{
  SkipRecord
  Returns whether or not a record should be skipped
}
function SkipRecord(rec: IInterface; records, recordMode: string): boolean;
var
  s: string;
begin
  s := Signature(rec);
  Result := ((Pos(s, records) > 0) and (recordMode = '0'))
    or ((Pos(s, records) = 0) and (recordMode = '1'))
    or ((Pos(s, global_records) > 0) and (global_recordMode = '0')) 
    or ((Pos(s, global_records) = 0) and (global_recordMode = '1'))
end;

{
  SkipSubrecord
  Check if a subrecord should be skipped
}
function SkipSubrecord(subrecord: IInterface; ini: TMemIniFile): boolean;
var
  subrecords, subrecordMode, subrecordPath: string;
  match, globalMatch: boolean;
begin
  // load subrecord settings
  subrecordMode := ini.ReadString('Setting', 'subrecordMode', '0');
  
  // path string
  subrecordPath := Path(subrecord);
  
  // result boolean
  match := ListHasMatch(slSubrecords, subrecordPath);
  globalMatch := ListHasMatch(slGlobalSubrecords, subrecordPath);
  Result := ((subrecordMode = '0') and (match))
    or ((subrecordMode = '1') and not (match))
    or ((global_subrecordMode = '0') and (globalMatch)) 
    or ((global_subrecordMode = '1') and not (globalMatch));
end;

{
  AddElementsToList
  Adds children elements to a stringlist
}
procedure AddElementsToList(element: IInterface; var sl: TStringList);
var
  i: integer;
  childElement: IInterface;
begin
  for i := 0 to ElementCount(element) - 1 do begin
    childElement := ebi(element, i);
    sl.Add(Name(childElement));
  end;
end;

{
  IsValueElement
  Checks if an element is a value element
}
function IsValueElement(elementType: string): boolean;
begin
  Result := (elementType = 'dtInteger') 
    or (elementType = 'dtFloat') 
    or (elementType = 'dtUnion') 
    or (elementType = 'dtByteArray')
    or (elementType = 'dtString') 
    or (elementType = 'dtLString') 
    or (elementType = 'dtLenString');
end;

{
  rcore
  Recursively Copy Overridden Elements
}
procedure rcore(src, mst, dst, dstrec: IInterface; depth: string; ini: TMemIniFile);
var
  i, j, k, max: integer;
  se, me, de, sse, mse, kse, kme, kde, xse: IInterface;
  mv, sv, ets, dts, cts, cas, ctsrc, subrecords, subrecordMode: string;
  diff: TRecordDiff;
  slDst, slMst: TStringList;
  skip: boolean;
begin
  // initialize stringlists
  slDst := TStringList.Create; // list of destination elements
  slMst := TStringList.Create; // list of master elements
  
  // copy elements from source to destination if missing
  AddElementsToList(dst, slDst);
  AddElementsToList(mst, slMst);
  for i := 0 to ElementCount(src) - 1 do begin
    se := ebi(src, i);
    // if the element isn't in the destination record
    // and wasn't in the master record, copy it to the destination
    // if it isn't in the destination but is in the master it means
    // that it was deleted and shouldn't be copied.
    if (slDst.IndexOf(Name(se)) = -1) 
    and (slMst.IndexOf(Name(se)) = -1) then
      wbCopyElementToRecord(se, dst, false, true);
  end;
  
  // loop through subelements
  i := 0;
  j := 0;
  while i < ElementCount(src) do begin
    // assign source, destination, master elements
    // ensure index out of bounds doesn't occur by not reassigning
    // past the last element
    if i < ElementCount(src) then
      se := ebi(src, i);
    if j < ElementCount(dst) then
      de := ebi(dst, j);
    me := ebn(mst, Name(se));
    
    // DefType and ElementType strings
    ets := ElementTypeString(se);
    dts := DefTypeString(se);
    
    // skip record header, copy record flags
    if Name(se) = 'Record Header' then begin
      wbCopyElementToRecord(ebp(se, 'Record Flags'), dst, false, true);
      Inc(i);
      Inc(j);
      continue;
    end;
    // skip subrecordsToSkip
    skip := SkipSubrecord(se, ini);
    if skip then begin
      if showSkips then LogMessage('    Skipping '+Path(se));
      Inc(i);
      Inc(j);
      continue;
    end;
    
    // debug messages
    if showTraversal then LogMessage('    '+Path(se));
    if showTypeStrings then LogMessage('    ets: '+ets+'  dts: '+dts);
    
    // if destination element doesn't match source element
    if (Name(se) <> Name(de)) then begin
      // if we're not at the end of the destination elements
      // proceed to next destination element
      // else proceed to next source element
      if (j < ElementCount(dst)) then
        Inc(j)
      else
        Inc(i);
      continue;
    end;
    
    // deal with subrecord arrays
    if (ets = 'etSubRecordArray') or (dts = 'dtArray') then begin
      // if sorted, deal with sorted array
      if IsSorted(se) then begin
        if debugArrays then LogMessage('    Sorted array found: '+Path(se));
        try
          MergeSortedArray(me, se, de, dstrec, depth, ini);
        except on x : Exception do
          LogMessage('      !! MergeSortedArray exception: '+x.Message);
        end;
      end
      // else deal with unsorted etSubRecordArray
      else if (ets = 'etSubRecordArray') then begin
        if debugArrays then LogMessage('    Unsorted etSubRecordArray found: '+Path(se));
        try 
          MergeUnsortedArray(me, se, de, dstrec, depth, ini);
        except on x : Exception do
          LogMessage('      !! MergeUnsortedArray exception: '+x.Message);
        end;
      end
      // else deal with unsorted dtArray
      else begin
        if debugArrays then LogMessage('    Unsorted dtArray found: '+Path(se));
        try 
          MergeUnsortedArray(me, se, de, dstrec, depth, ini);
          //rcore(se, me, de, dstrec, depth + ' ', ini);
        except on x : Exception do
          LogMessage('      !! MergeUnsortedArray exception: '+x.Message);
        end;
      end;
    end
    
    // else recurse deeper
    else if (ElementCount(se) > 0) and (dts <> 'dtInteger') then begin
      try
        rcore(se, me, de, dstrec, depth + '    ', ini);
      except on x : Exception do
        LogMessage('      !! rcore exception in element '+Path(se)+': '+x.Message);
      end;
    end
    
    // else copy element if value differs from master
    else if IsValueElement(dts) and (GetEditValue(se) <> GetEditValue(me)) then begin
      if (Assigned(me)) and showChanges then begin
        if (not showTraversal) then LogMessage('    '+Path(se));
        LogMessage('      > Found differing values: '+GetEditValue(se)+' and '+GetEditValue(me));
      end;
      // try to copy element value to destination element from source element
      try 
        SetEditValue(de, GetEditValue(se));
      except on x : Exception do
        LogMessage('      !! Copy element value exception: '+x.Message);
      end;
    end;
    
    // proceed to next subelement
    Inc(i);
    Inc(j);
  end;
  
  slDst.Free;
  slMst.Free;
end;

{
  IsSmashedPatch
  Checks if @f is a smashed patch
}
function IsSmashedPatch(f: IInterface): boolean;
var
  author: string;
begin
  author := geev(ElementByIndex(f, 0), 'CNAM');
  Result := (Pos('Mator Smash', author) = 1); 
end;

{
  SmashRecord
  Smashes @rec into @smashFile.
}
procedure SmashRecord(rec, smashFile: IInterface);
var
  i: integer;
  fn, author: string;
  f, ovr, mr: IInterface;
  ini: TMemIniFile;
begin
  // loop through record's overrides
  for i := 0 to OverrideCount(rec) - 1 do begin
    ovr := OverrideByIndex(rec, i);
    f := GetFile(ovr);
    fn := GetFileName(f);
    
    // skip overrides in bethesda files
    if (Pos(fn, bethesdaFiles) > 0) then
      continue;
    // skip overrides in smashed patches
    if (IsSmashedPatch(f)) then 
      continue;
    // skip ctIdenticalToMaster overrides
    if (ConflictThisForMainRecord(ovr) = ctIdenticalToMaster) then
      continue;
    
    // if master record is not assigned, copy winning override to smashed patch
    if (not Assigned(mr)) then begin
      try
        mr := wbCopyElementToFile(WinningOverride(ovr), smashFile, false, true);
      except on x: Exception do
        LogMessage('      !! Exception copying record '+Name(rec)+' : '+x.Message);
      end;
    end;
    
    // look up setting for this file
    try
      ini := TMemIniFile(lstSettings[slSettings.IndexOf(slOptions[slFiles.IndexOf(fn)])]);
    except on x : Exception do
      LogMessage('Setting lookup exception : '+x.Message);
    end;
    
    // recursively copy overriden elements
    try
      if makeNewLine then LogMessage('');
      LogMessage('Smashing record '+Name(rec)+' from file: '+fn);
      slSubrecords.Text := StringReplace(ini.ReadString('Setting', 'subrecords', ''), '#13', #13, [rfReplaceAll]);
      rcore(ovr, rec, mr, mr, '    ', ini);
    except on x : Exception do
      LogMessage('    !! Exception smashing record '+Name(rec)+' : '+x.Message);
    end;
  end;
end;


{===========================================================}
{ MAIN EXECUTION }


{
  ShowDetails
  Enables the visibilty of the TMemo log
}
procedure ShowDetails;
begin
  frm.Height := 600;
  frm.Position := poScreenCenter;
  memo.Height := frm.Height - 150;
  btnDetails.Visible := false;
  memo.Visible := true;
end;

{
  InitializeSettings
  Loads settings from files
}
procedure InitializeSettings;
var
  i: integer;
  ini: TMemIniFile;
  s: string;
  info: TSearchRec;
begin
  // create lists
  slSettings := TStringList.Create;
  lstSettings := TList.Create;
  
  // load settings
  SetCurrentDir(settingsPath);
  if FindFirst(settingsPath+'*.ini', faAnyFile, info) = 0 then begin
    repeat
      lstSettings.Add(TMemIniFile.Create(settingsPath + info.Name));
    until FindNext(info) <> 0;
  end;
  
  // add setting strings
  for i := 0 to lstSettings.Count - 1 do begin
    s := TMemIniFile(lstSettings[i]).ReadString('Setting', 'Name', '');
    slSettings.Add(s);
  end;
end;

{
  FreeMemory
  Frees memory used by script
}
procedure FreeMemory;
begin
  gear.Free; slOptions.Free; slFiles.Free; slSettings.Free;
  lstSettings.Free;
end;

{
  Initialize
  Load assets, settings, display forms, show progress bar, perform 
  main smashing procedure.
}
function Initialize: integer;
var
  f, r: IInterface;
  i, j, k: integer;
  fn, rn, records, recordMode, logFileName, fdt: string;
  ini: TMemIniFile;
  tStart, tRec: TDateTime;
  diff: double;
  pic: TPicture;
  bmpChecked, bmpUnChecked, bmpPChecked: TBitmap;
begin
  // load tree stringlists
  AssetPath := ScriptsPath + 'smash\assets\';
  recordTree := TStringList.Create;
  recordTree.LoadFromFile(AssetPath + 'RecordTree.txt');
  subrecordTree := TStringList.Create;
  subrecordTree.LoadFromFile(AssetPath + 'SubrecordTree.txt');
  include := TStringList.Create;
  
  // load checkbox images
  checkboxImages := TCustomImageList.Create(nil);
  pic := TPicture.Create;
  
  bmpChecked := TBitmap.Create;
  bmpUnChecked := TBitmap.Create;
  bmpPChecked := TBitmap.Create;
  
  pic.LoadFromFile(AssetPath + 'Check.bmp');
  bmpChecked.SetSize(17, 17);
  bmpChecked.Canvas.Draw(0, 0, pic.Graphic);  
  
  pic.LoadFromFile(AssetPath + 'UnCheck.bmp');
  bmpUnChecked.SetSize(17, 17);
  bmpUnChecked.Canvas.Draw(0, 0, pic.Graphic);  
  
  pic.LoadFromFile(AssetPath + 'PartialCheck.bmp');
  bmpPChecked.SetSize(17, 17);
  bmpPChecked.Canvas.Draw(0, 0, pic.Graphic);  

  checkboxImages.Add(bmpChecked, nil);
  checkboxImages.Add(bmpChecked, nil);
  checkboxImages.Add(bmpUnChecked, nil);
  checkboxImages.Add(bmpPChecked, nil);
  
  // track time
  tStart := Now;
  
  // stringlist creation
  slOptions := TStringList.Create;
  slFiles := TStringList.Create;
  slSubrecords := TStringList.Create;
  slGlobalSubrecords := TStringList.Create;
  makeNewLine := showSkips or showTraversal or debugGetMaster or debugArrays 
    or showChanges or showTypeStrings or showRecTimes;
  
  // load gui elements
  gear := TPicture.Create;
  gear.LoadFromFile(AssetPath + 'gear.png');
  
  // load setting files
  InitializeSettings;
  
  // set up for saving log
  ForceDirectories(ScriptsPath + '\smash\logs');
  fdt := FormatDateTime('mmddyy_hhnnss', Now);
  logFileName := ScriptsPath + '\smash\logs\smash' + fdt + '.txt';
  
  // initial options form
  if OptionsForm then begin
    frm := TForm.Create(nil);
    try 
      frm.Caption := 'Mator Smash!';
      frm.Width := 700;
      frm.Position := poScreenCenter;
      frm.Height := 150;
      
      // make progress label
      lbl := cLabel(frm, frm, 20, 20, 30, 600, 'Initializing...', '');
      
      // make progress bar
      pb := TProgressBar.Create(frm);
      pb.Parent := frm;
      pb.Top := 40;
      pb.Left := 20;
      pb.Width := frm.Width - 55;
      pb.Height := 20;
      pb.Step := 1;
      pb.Min := 0;
      pb.Position := 0;
      
      // make log memo
      memo := cMemo(frm, frm, 70, 20, 0, pb.Width, false, true, ssBoth, '');
      memo.Visible := false;
      
      // make details button
      btnDetails := cButton(frm, frm, pb.Top + pb.Height + 8, pb.Left, 0, 100, 'Show Details');
      btnDetails.OnClick := ShowDetails;
      
      // display form, initial logging messages
      frm.Show;
      application.processmessages;
      LogMessage(dashes);
      LogMessage('Mator Smash '+vs+': Makes a smashed patch.');
      LogMessage(dashes);
     
      // create stringlists
      slRecords := TStringList.Create;
      
      // load global settings
      ini := TMemIniFile(lstSettings[slSettings.IndexOf(global_setting)]);
      global_records := StringReplace(ini.ReadString('Setting', 'records', ''), splitChar, #13#10, [rfReplaceall]);
      global_recordMode := ini.ReadString('Setting', 'recordMode', '0');
      global_subrecords := StringReplace(ini.ReadString('Setting', 'subrecords', ''), splitChar, #13#10, [rfReplaceall]);
      global_subrecordMode := ini.ReadString('Setting', 'subrecordMode', '0');
      slGlobalSubrecords.Text := global_subrecords;
     
      // see if a smashed patch is loaded
      for i := 0 to FileCount - 1 do begin
        f := FileByIndex(i);
        // if smashed patch found, break
        if (IsSmashedPatch(f)) then begin
          userFile := f;
          break;
        end;
      end;
     
      // make userFile if not found
      lbl.Caption := 'Assigning smashed patch.';
      application.processmessages;
      if not Assigned(userFile) then
        userFile := AddNewFile;
      if not Assigned(userFile) then begin
        LogMessage('Smashed patch not assigned, terminating script');
        FreeMemory;
        frm.Free;
        Result := -1;
        exit;
      end;
      
      // set userFile author to Mator Smash
      lbl.Caption := 'Adding masters to smashed patch.';
      application.processmessages;
      seev(ebi(userFile, 0), 'CNAM', 'Mator Smash '+vs);
      // add masters to userFile
      for i := 0 to FileCount - 3 do begin
        f := FileByLoadOrder(i);
        if (IsSmashedPatch(f)) then
          break;
        fn := GetFileName(f);
        AddMasterIfMissing(userFile, fn);
      end;
     
      // loop through all loaded files
      k := 0;
      tRec := Now;
      for i := 0 to FileCount - 1 do begin
        f := FileByIndex(i);
        fn := GetFileName(f);
        
        // skip bethesda files, we're not patching them
        if Pos(fn, bethesdaFiles) > 0 then
          continue;
        // if smashed patch found, break
        if (IsSmashedPatch(f)) then
          break;
        
        // build list of records with multiple overrides
        lbl.Caption := 'Processing '+fn;
        LogMessage('Processing '+fn);
        application.processmessages;
        // load ini settings
        ini := TMemIniFile(lstSettings[slSettings.IndexOf(slOptions[k])]);
        records := StringReplace(ini.ReadString('Setting', 'records', ''), splitChar, #13#10, [rfReplaceAll]);
        recordMode := ini.ReadString('Setting', 'recordMode', '0');
        
        // loop through records
        for j := 0 to RecordCount(f) - 1 do begin
          r := MasterOrSelf(RecordByIndex(f, j));
          if (OverrideCount(r) <= 1) then
            continue;
          // skip records according to ini settings
          if SkipRecord(r, records, recordMode) then 
            continue;
          rn := Name(r);
          if (nbsOverrideCount(r) > 1) then
            if (ConflictThisForMainRecord(WinningOverride(r)) <> ctOverride) then
              if slRecords.IndexOf(rn) = -1 then begin
                slRecords.AddObject(rn, TObject(r));
              end;
        end;
        Inc(k);
      end;
      diff := (Now - tRec) * 86400;
      LogMessage(FormatFloat('0.###', diff) + ' seconds spent processing records.');
     
      // test list of records
      if listOverrides then begin
        LogMessage('');
        for i := 0 to slRecords.Count - 1 do begin
          r := ObjectToElement(slRecords.Objects[i]);
          LogMessage(slRecords[i]+' ('+IntToStr(OverrideCount(r))+' overrides)');
          for j := 0 to OverrideCount(r) - 1 do
            LogMessage('    Override #'+IntToStr(j)+': '+GetFileName(GetFile(OverrideByIndex(r, j))));
        end;
      end;
     
      // smash records that have been overridden multiple times
      lbl.Caption := 'Smashing records (1/'+IntToStr(slRecords.Count)+')';
      application.processmessages;
      pb.Max := slRecords.Count;
      LogMessage('');
      for i := 0 to slRecords.Count - 1 do begin
        tRec := Now;
        if i = maxRecords then break;
        // smash record
        r := ObjectToElement(slRecords.Objects[i]);
        SmashRecord(r, userFile);
        // update label, print debug message to log after smashing record
        lbl.Caption := 'Smashing records ('+IntToStr(i + 2)+'/'+IntToStr(slRecords.Count)+')';
        pb.Position := pb.Position + 1;
        if showRecTimes then begin
          diff := (Now - tRec) * 86400;
          LogMessage('  '+FormatFloat('0.###', diff) + 's');
        end;
        application.processmessages;
      end;
      
      // sort and clean masters
      SortMasters(userFile);
      CleanMasters(userFile);
      
      // finishing messages
      lbl.Caption := 'All done.';
      LogMessage(#13#10+dashes);
      LogMessage('Smashing complete.  '+IntToStr(RecordCount(userfile))+' records smashed.');
      diff := (Now - tStart) * 86400;
      LogMessage('Completed in ' + FormatFloat('0.###', diff) + ' seconds.');
      memo.Lines.SaveToFile(logFileName);
      application.processmessages;
      
      if (memo.Visible) then begin
        frm.Visible := false;
        frm.ShowModal;
      end;
    except on x : Exception do begin
        // smash failed
        LogMessage(#13#10'Smash failed.  Exception: '+x.Message);
        memo.Lines.SaveToFile(logFileName);
        pb.Position := 0;
        lbl.Caption := 'Smash Failed.  Exception: '+x.Message;
        if not memo.Visible then ShowDetails;
        frm.Visible := false;
        frm.ShowModal;
        Application.processmessages;
      end;
    end;
    frm.Free;
  end;

  // free memory
  FreeMemory;

  // call RemoveFilter() to update TES5Edit GUI
  try
    RemoveFilter();
  except on Exception do
    AddMessage(#13#10'You''re not using the latest version of xEdit, so the script couldn''t update the GUI.');
    AddMessage('Right click in the plugin view and click "Remove Filter" to update the GUI manually.');
  end;
end;

end.
