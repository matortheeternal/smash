unit msProfilePanel;

interface

uses
  SysUtils, Classes, Controls, Dialogs, Graphics, Buttons, StdCtrls, ExtCtrls,
  ImgList, Types,
  // mte components
  mteHelpers,
  // mp components
  msConfiguration, msLoader;

type
  TProfilePanel = class(TPanel)
  public
    Selected: boolean;
    Valid: boolean;
    constructor ICreate(AOwner: TComponent; GameIcons, GeneralIcons: TImageList;
      name: string);
    destructor Destroy; override;
    procedure ToggleSelect(Sender: TObject);
    procedure Deselect;
    procedure Select;
    procedure SetTop(top: Integer);
    procedure SetWidth(width: Integer);
    function GetProfile: TProfile;
    procedure SetSelectCallback(callback: TNotifyEvent);
    procedure SetDeleteCallback(callback: TNotifyEvent);
    procedure SetGame(i: integer);
    procedure SetPath(path: string);
  private
    aProfile: TProfile;
    ColorInvalid: Integer;
    ColorValid: Integer;
    ColorSelected: Integer;
    GameImage: TImage;
    lblName: TLabel;
    lblGame: TLabel;
    lblPath: TLabel;
    edName: TEdit;
    cbGame: TComboBox;
    edPath: TEdit;
    btnBrowse: TSpeedButton;
    btnDelete: TSpeedButton;
    GameIcons: TImageList;
    SelectCallback: TNotifyEvent;
    DeleteCallback: TNotifyEvent;
    procedure Browse(Sender: TObject);
    procedure Delete(Sender: TObject);
    procedure NameChanged(Sender: TObject);
    procedure GameChanged(Sender: TObject);
    procedure PathChanged(Sender: TObject);
  end;

implementation


{******************************************************************************}
{ TProfilePanel
  A GUI component for interacting with a profile.
}
{******************************************************************************}

constructor TProfilePanel.ICreate(AOwner: TComponent; GameIcons: TImageList;
  GeneralIcons: TImageList; name: string);
const
  GameItems = 'The Elder Scrolls V: Skyrim'#13#10 +
              'The Elder Scrolls IV: Oblivion'#13#10 +
              'Fallout: New Vegas'#13#10 +
              'Fallout 3'#13#10 +
              'Fallout 4'#13#10 +
              'Skyrim: Special Edition';
begin
  // set up panel
  inherited Create(AOwner);
  Parent := AOwner as TWinControl;
  Width := (AOwner as TWinControl).ClientWidth;
  Height := 100;
  Left := 0;
  Top := 0;
  ParentBackground := false;
  ParentColor := false;
  ParentDoubleBuffered := false;
  Cursor := crDefault;
  Anchors := [akLeft, akTop, akRight];
  Visible := true;

  // set local variables
  aProfile := TProfile.Create(name);
  self.GameIcons := GameIcons;
  Selected := false;
  ColorInvalid := $d8d8f0;
  ColorValid := $d8f0d8;
  ColorSelected := $f0e8d8;

  // create components
  GameImage := TImage.Create(self);
  lblName := TLabel.Create(self);
  lblGame := TLabel.Create(self);
  lblPath := TLabel.Create(self);
  edName := TEdit.Create(self);
  cbGame := TComboBox.Create(self);
  edPath := TEdit.Create(self);
  btnDelete := TSpeedButton.Create(self);
  btnBrowse := TSpeedButton.Create(self);

  // set up GameImage
  GameImage.Parent := self;
  GameImage.Top := 2;
  GameImage.Left := 2;
  GameImage.Height := 96;
  GameImage.Width := 96;
  GameImage.Cursor := crDefault;
  GameImage.Transparent := true;
  GameImage.Align := alCustom;
  GameImage.Anchors := [akLeft, akTop, akBottom];

  // set up lblName
  lblName.Parent := self;
  lblName.Top := 11;
  lblName.Left := 112;
  lblName.Caption := 'Name';
  lblName.Align := alCustom;
  lblName.Anchors := [akLeft, akTop];

  // set up lblGame
  lblGame.Parent := self;
  lblGame.Top := 38;
  lblGame.Left := 112;
  lblGame.Caption := 'Game';
  lblGame.Align := alCustom;
  lblGame.Anchors := [akLeft, akTop];

  // set up lblPath
  lblPath.Parent := self;
  lblPath.Top := 65;
  lblPath.Left := 112;
  lblPath.Caption := 'Path';
  lblPath.Align := alCustom;
  lblPath.Anchors := [akLeft, akTop];

  // set up edName
  edName.Parent := self;
  edName.Top := 8;
  edName.Left := 177;
  edName.Width := 227;
  edName.Text := name;
  edName.Align := alCustom;
  edName.Anchors := [akLeft, akTop];

  // set up cbGame
  cbGame.Parent := self;
  cbGame.Top := 35;
  cbGame.Left := 177;
  cbGame.Width := 227;
  cbGame.Style := csDropDownList;
  cbGame.Items.Text := GameItems;
  cbGame.ItemIndex := 0;
  cbGame.Align := alCustom;
  cbGame.Anchors := [akLeft, akTop];

  // set up edPath
  edPath.Parent := self;
  edPath.Top := 62;
  edPath.Left := 177;
  edPath.Width := 227;
  edPath.Align := alCustom;
  edPath.Anchors := [akLeft, akTop];

  // set up btnBrowse
  btnBrowse.Parent := self;
  btnBrowse.Top := 61;
  btnBrowse.Left := 410;
  btnBrowse.Width := 22;
  btnBrowse.Height := 23;
  btnBrowse.Flat := true;
  btnBrowse.Transparent := true;
  GeneralIcons.GetBitmap(0, btnBrowse.Glyph);
  btnBrowse.Align := alCustom;
  btnBrowse.Anchors := [akLeft, akTop];

  // set up btnDelete
  btnDelete.Parent := self;
  btnDelete.Top := 4;
  btnDelete.Left := 410;
  btnDelete.Width := 22;
  btnDelete.Height := 23;
  btnDelete.Flat := true;
  btnDelete.Transparent := true;
  GeneralIcons.GetBitmap(1, btnDelete.Glyph);
  btnDelete.Align := alCustom;
  btnDelete.Anchors := [akLeft, akTop];

  // set event handlers
  self.OnClick := ToggleSelect;
  GameImage.OnClick := ToggleSelect;
  lblName.OnClick := ToggleSelect;
  lblGame.OnClick := ToggleSelect;
  lblPath.OnClick := ToggleSelect;
  btnBrowse.OnClick := Browse;
  btnDelete.OnClick := Delete;
  edName.OnChange := NameChanged;
  edPath.OnChange := PathChanged;
  cbGame.OnChange := GameChanged;

  // call initial events
  GameChanged(nil);
  PathChanged(nil);
end;

destructor TProfilePanel.Destroy;
begin
  aProfile.Free;
  GameImage.Free;
  lblName.Free;
  lblGame.Free;
  lblPath.Free;
  edName.Free;
  cbGame.Free;
  edPath.Free;
  btnDelete.Free;
  btnBrowse.Free;
  inherited;
end;

{ EVENT HANDLING }
procedure TProfilePanel.ToggleSelect(Sender: TObject);
begin
  if Cursor = crHandPoint then begin
    if Selected then Deselect else Select;
    if Assigned(SelectCallback) then SelectCallback(self);
  end;
end;

procedure TProfilePanel.Select;
begin
  Selected := true;
  Color := ColorSelected;
  Repaint;
end;

procedure TProfilePanel.Deselect;
begin
  Selected := false;
  Color := ColorValid;
  Repaint;
end;

procedure TProfilePanel.Browse(Sender: TObject);
begin
  // have user browse for folder
  BrowseForFolder(edPath, '');

  // then update in profile
  aProfile.gamePath := edPath.Text;
end;

procedure TProfilePanel.Delete(Sender: TObject);
begin
  if Assigned(DeleteCallback) then DeleteCallback(self);
end;

procedure TProfilePanel.NameChanged(Sender: TObject);
begin
  if FileNameValid(edName.Text) and
  (not DirectoryExists(PathList.Values['ProgramPath'] + 'profiles\' + edName.Text)) then
    aProfile.Rename(edName.Text);
end;

procedure TProfilePanel.GameChanged(Sender: TObject);
begin
  // set in profile
  aProfile.gamemode := cbGame.ItemIndex + 1;

  // clear the GameImage, then set it to the image of the
  // game the user selected
  GameImage.Canvas.Brush.Color := clBlack;
  GameImage.Canvas.Rectangle(0, 0, 96, 96);
  GameIcons.GetBitmap(cbGame.ItemIndex, GameImage.Picture.Bitmap);
  GameImage.Repaint;
end;

procedure TProfilePanel.PathChanged(Sender: TObject);
begin
  // set in profile
  aProfile.gamePath := AppendIfMissing(edPath.Text, '\');

  // reflect validity in the GUI
  if GamePathValid(aProfile.gamePath, aProfile.gameMode) then begin
    Valid := true;
    Cursor := crHandPoint;
    GameImage.Cursor := crHandPoint;
    Color := ColorValid;
  end
  else begin
    Valid := false;
    Cursor := crDefault;
    GameImage.Cursor := crDefault;
    Color := ColorInvalid;
  end;

  // repaint to update GUI
  Repaint;
end;

procedure TProfilePanel.SetTop(top: Integer);
begin
  self.top := top;
end;

procedure TProfilePanel.SetWidth(width: Integer);
begin
  self.width := width;
end;

function TProfilePanel.GetProfile: TProfile;
begin
  Result := aProfile;
end;

procedure TProfilePanel.SetSelectCallback(callback: TNotifyEvent);
begin
  SelectCallback := callback;
end;

procedure TProfilePanel.SetDeleteCallback(callback: TNotifyEvent);
begin
  DeleteCallback := callback;
end;

procedure TProfilePanel.SetGame(i: Integer);
begin
  cbGame.ItemIndex := i - 1;
  GameChanged(nil);
end;

procedure TProfilePanel.SetPath(path: string);
begin
  edPath.Text := path;
  PathChanged(nil);
end;

end.
