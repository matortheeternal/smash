unit msProfileForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls, Graphics,
  ImgList, Menus, Dialogs, ExtCtrls, pngimage,
  // mte components
  RttiIni, mteHelpers,
  // mp components
  msConfiguration, msLoader, msProfilePanel;

type
  TProfileForm = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    GameIcons: TImageList;
    ProfilePopupMenu: TPopupMenu;
    NewProfileItem: TMenuItem;
    DeleteProfileItem: TMenuItem;
    GeneralIcons: TImageList;
    ScrollBox: TScrollBox;
    NewProfilePanel: TPanel;
    NewProfileImage: TImage;
    NewProfileLabel: TLabel;
    PaddingLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure RealignPanels;
    function CreateNewProfile(name: string): TProfilePanel;
    procedure NewProfileItemClick(Sender: TObject);
    procedure LoadProfiles;
    function ProfileExists(gameMode: Integer): Boolean;
    procedure CreateDefaultProfiles;
    procedure ProfilePopupMenuPopup(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure DeleteClicked(Sender: TObject);
    procedure DeleteProfileItemClick(Sender: TObject);
    function ProfileNameTaken(name: string): boolean;
    procedure NewProfileImageClick(Sender: TObject);
    procedure NewProfilePanelClick(Sender: TObject);
    procedure NewProfileLabelClick(Sender: TObject);
    procedure NewProfilePanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FOldWndProc: TWndMethod;
    FMouseInPanel: Boolean;
    procedure PanelWndProc(var Message: TMessage);
  public
    ProfilePanels: TList;
  end;

var
  ProfileForm: TProfileForm;
  SelectCallback, DeleteCallback: TNotifyEvent;
  MouseOverProfile: TProfilePanel;
  ProgramPath: String;

implementation

{$R *.dfm}

procedure TProfileForm.DeleteProfileItemClick(Sender: TObject);
var
  bApproved: boolean;
  aProfile: TProfile;
begin
  // get user verification
  aProfile := MouseOverProfile.GetProfile;
  bApproved := MessageDlg('Are you sure you want to delete '+
    aProfile.name + '?', mtConfirmation, mbOKCancel, 0) = mrOk;

  if not (bApproved and Assigned(MouseOverProfile)) then exit;
  ProfilePanels.Delete(ProfilePanels.IndexOf(MouseOverProfile));
  aProfile.Delete;
  MouseOverProfile.Free;
  RealignPanels;
end;

procedure TProfileForm.RealignPanels;
var
  i, vPos, pCount: Integer;
  p: TProfilePanel;
begin
  // just some aliases
  vPos := ScrollBox.VertScrollBar.ScrollPos;
  pCount := ProfilePanels.Count;

  // adjust tops
  NewProfilePanel.Top := 100 * pCount - vPos;
  for i := Pred(pCount) downto 0 do begin
    p := TProfilePanel(ProfilePanels[i]);
    p.SetTop(100 * i - vPos);
  end;
  PaddingLabel.Top := 100 * (pCount + 1) - vPos - PaddingLabel.Height;

  // adjust widths
  NewProfilePanel.Width := ScrollBox.ClientWidth;
  for i := Pred(pCount) downto 0 do begin
    p := TProfilePanel(ProfilePanels[i]);
    p.SetWidth(ScrollBox.ClientWidth);
  end;
end;

procedure TProfileForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
  p: TProfilePanel;
  aProfile: TProfile;
begin
  // save all profiles
  for i := 0 to Pred(ProfilePanels.Count) do begin
    p := TProfilePanel(ProfilePanels[i]);
    aProfile := p.GetProfile;
    try
      SaveProfile(aProfile);
    except
      on Exception do
        // nothing to do
    end;
  end;

  // set profile if user clicked OK
  if ModalResult = mrOK then begin
    for i := 0 to Pred(ProfilePanels.Count) do begin
      p := TProfilePanel(ProfilePanels[i]);
      if p.Selected then begin
        CurrentProfile := TProfile.Create('');
        CurrentProfile.Clone(p.GetProfile);
        break;
      end;
    end;
  end;

  // free memory
  FreeList(ProfilePanels);
end;

procedure TProfileForm.FormCreate(Sender: TObject);
begin
  ProgramPath := ExtractFilePath(ParamStr(0));
  ProfilePanels := TList.Create;
  SelectCallback := SelectionChanged;
  DeleteCallback := DeleteClicked;
  LoadProfiles;
  CreateDefaultProfiles;
  FOldWndProc := NewProfilePanel.WindowProc;
  NewProfilePanel.WindowProc := PanelWndProc;
end;

procedure TProfileForm.FormDestroy(Sender: TObject);
begin
  NewProfilePanel.WindowProc:= FOldWndProc;
end;

procedure TProfileForm.LoadProfiles;
var
  path, settingsPath: string;
  info: TSearchRec;
  p: TProfilePanel;
  aSettings: TSettings;
begin
  path := ProgramPath + 'profiles\';
  if not DirectoryExists(path) then
    exit;

  if FindFirst(path + '*', faAnyFile or faDirectory, info) <> 0 then
    exit;
  // add found profiles
  repeat
    if IsDotFile(info.Name) then
      continue;
    settingsPath := path + info.Name + '\settings.ini';
    if not FileExists(settingsPath) then
      continue;
    aSettings := TSettings.Create;
    TRttiIni.Load(settingsPath, aSettings);
    if aSettings.profile <> '' then begin
      p := CreateNewProfile(aSettings.profile);
      p.SetGame(aSettings.gameMode);
      p.SetPath(aSettings.gamePath);
    end;
    aSettings.Free;
  until FindNext(info) <> 0;
end;

function TProfileForm.ProfileExists(gameMode: Integer): Boolean;
var
  i: Integer;
  profile: TProfile;
begin
  Result := False;
  for i := 0 to Pred(ProfilePanels.Count) do begin
    profile := TProfilePanel(ProfilePanels[i]).GetProfile;
    if profile.gameMode = gameMode then begin
      Result := True;
      break;
    end;
  end;
end;

procedure TProfileForm.CreateDefaultProfiles;
var
  i: Integer;
  path, name: string;
  p: TProfilePanel;
begin
  for i := Low(GameArray) to High(GameArray) do begin
    if ProfileExists(i) then continue;
    path := GetGamePath(GameArray[i]);
    name := GameArray[i].appName;
    if path <> '' then begin
      p := CreateNewProfile(name);
      p.SetGame(i);
      p.SetPath(path);
    end;
  end;
end;

function TProfileForm.CreateNewProfile(name: string): TProfilePanel;
begin
  Result := TProfilePanel.ICreate(ScrollBox, GameIcons, GeneralIcons, name);
  Result.SetSelectCallback(SelectCallback);
  Result.SetDeleteCallback(DeleteCallback);
  ProfilePanels.Add(Result);
  RealignPanels;
end;

procedure TProfileForm.NewProfileItemClick(Sender: TObject);
var
  name: string;
  i: Integer;
begin
  // find profile name
  name := 'NewProfile';
  i := 1;
  while ProfileNameTaken(name + IntToStr(i)) do
    Inc(i);

  // create a new profile
  name := name + IntToStr(i);
  CreateNewProfile(name);
end;

procedure TProfileForm.NewProfileImageClick(Sender: TObject);
begin
  NewProfileItemClick(nil);
end;

procedure TProfileForm.NewProfileLabelClick(Sender: TObject);
begin
  NewProfileItemClick(nil);
end;

procedure TProfileForm.NewProfilePanelClick(Sender: TObject);
begin
  NewProfileItemClick(nil);
end;

function TProfileForm.ProfileNameTaken(name: string): boolean;
var
  i: Integer;
  pName: string;
begin
  Result := false;
  for i := 0 to Pred(ProfilePanels.Count) do begin
    pName := TProfilePanel(ProfilePanels[i]).GetProfile.name;
    if SameText(pName, name) then begin
      Result := true;
      break;
    end;
  end;
end;

procedure TProfileForm.ProfilePopupMenuPopup(Sender: TObject);
var
  pt: TPoint;
  index: Integer;
begin
  // get profile user is moused over
  pt := ScrollBox.ScreenToClient(Mouse.CursorPos);
  Index := pt.Y div 100;
  if Index < ProfilePanels.Count then
    MouseOverProfile := TProfilePanel(ProfilePanels[index])
  else
    MouseOverProfile := nil;

  // can only delete profile if mouse over a profile
  ProfilePopupMenu.Items[1].Enabled := Assigned(MouseOverProfile);
end;

procedure TProfileForm.SelectionChanged(Sender: TObject);
var
  i: Integer;
  p: TProfilePanel;
  bSelected: boolean;
begin
  // deselect all panels except the sender
  for i := 0 to Pred(ProfilePanels.Count) do begin
    p := TProfilePanel(ProfilePanels[i]);
    if p <> TProfilePanel(Sender) then
      p.Deselect
  end;

  // enable and focus ok button if profile panel is selected
  bSelected := TProfilePanel(Sender).Selected;
  btnOk.Enabled := bSelected;
  if bSelected then self.FocusControl(btnOk)
  else self.FocusControl(btnCancel);
end;

procedure TProfileForm.DeleteClicked(Sender: TObject);
begin
  MouseOverProfile := TProfilePanel(Sender);
  DeleteProfileItemClick(nil);
end;

procedure TProfileForm.NewProfilePanelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  mEvnt: TTrackMouseEvent;
begin
  if not FMouseInPanel then begin
    mEvnt.cbSize := SizeOf(mEvnt);
    mEvnt.dwFlags := TME_LEAVE;
    mEvnt.hwndTrack := NewProfilePanel.Handle;
    TrackMouseEvent(mEvnt);
    NewProfilePanel.Color:= $f0e8d8;
    FMouseInPanel:= True;
  end;
end;

procedure TProfileForm.PanelWndProc(var Message: TMessage);
begin
  if Message.Msg = WM_MOUSELEAVE then begin
    NewProfilePanel.Color:= clBtnFace;
    FMouseInPanel:= False;
  end;
  FOldWndProc(Message);
end;

end.
