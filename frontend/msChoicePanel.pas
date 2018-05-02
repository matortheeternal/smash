unit msChoicePanel;

interface

uses
  SysUtils, Classes, Controls, Dialogs, Graphics, Buttons, StdCtrls, ExtCtrls,
  ImgList, Types,
  // mte components
  mteHelpers,
  // mp components
  msCore;

type
  TSenderCallback = procedure(Sender: TObject) of object;
  TChoicePanel = class(TPanel)
  public
    Selected: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ToggleSelect(Sender: TObject);
    procedure SettingChanged(Sender: TObject);
    procedure Deselect;
    procedure Select;
    procedure SetTop(top: Integer);
    procedure SetWidth(width: Integer);
    function GetSetting: TSmashSetting;
    function GetLabel: string;
    procedure SetSelectCallback(callback: TNotifyEvent);
    procedure SetLabel(sig: string);
    procedure AddChoice(choice: TSmashSetting);
  private
    lstChoices: TList;
    ColorUnselected: Integer;
    ColorSelected: Integer;
    lblSig: TLabel;
    cbChoices: TComboBox;
    SelectCallback: TNotifyEvent;
  end;

implementation


{******************************************************************************}
{ TChoicePanel
  A GUI component for interacting with a profile.
}
{******************************************************************************}

constructor TChoicePanel.Create(AOwner: TComponent);
begin
  // set up panel
  inherited Create(AOwner);
  Parent := AOwner as TWinControl;
  Width := (AOwner as TWinControl).ClientWidth;
  Height := 36;
  Left := 0;
  Top := 0;
  ParentBackground := false;
  ParentColor := false;
  ParentDoubleBuffered := false;
  Cursor := crHandPoint;
  Anchors := [akLeft, akTop, akRight];
  Visible := true;

  // set local variables
  Selected := false;
  ColorUnselected := Color;
  ColorSelected := $f0ece4;
  lstChoices := TList.Create;

  // create components
  lblSig := TLabel.Create(self);
  cbChoices := TComboBox.Create(self);

  // set up lblSig
  lblSig.Parent := self;
  lblSig.Top := 10;
  lblSig.Left := 16;
  lblSig.Caption := 'SIGN';
  lblSig.Cursor := crHandPoint;
  lblSig.Align := alCustom;
  lblSig.Anchors := [akLeft, akTop];

  // set up cbGame
  cbChoices.Parent := self;
  cbChoices.Top := 8;
  cbChoices.Left := 75;
  cbChoices.Width := Width - cbChoices.Left - 6;
  cbChoices.Style := csDropDownList;
  cbChoices.Items.Text := '';
  cbChoices.Items.Add('Patch');
  cbChoices.ItemIndex := 0;
  cbChoices.Align := alCustom;
  cbChoices.Anchors := [akLeft, akTop, akRight];

  // set event handlers
  self.OnClick := ToggleSelect;
  lblSig.OnClick := ToggleSelect;
  cbChoices.OnChange := SettingChanged;
end;

destructor TChoicePanel.Destroy;
begin
  lblSig.Free;
  cbChoices.Free;
  lstChoices.Free;
  inherited;
end;

{ EVENT HANDLING }
procedure TChoicePanel.SettingChanged(Sender: TObject);
begin
  if Assigned(SelectCallback) then SelectCallback(self);
end;

procedure TChoicePanel.ToggleSelect(Sender: TObject);
begin
  if Cursor = crHandPoint then begin
    if Selected then Deselect else Select;
    if Assigned(SelectCallback) then SelectCallback(self);
  end;
end;

procedure TChoicePanel.Select;
begin
  Selected := true;
  Color := ColorSelected;
  Repaint;
end;

procedure TChoicePanel.Deselect;
begin
  Selected := false;
  Color := ColorUnselected;
  Repaint;
end;

procedure TChoicePanel.SetTop(top: Integer);
begin
  self.top := top;
end;

procedure TChoicePanel.SetWidth(width: Integer);
begin
  self.width := width;
end;

function TChoicePanel.GetSetting: TSmashSetting;
begin
  Result := nil;
  if cbChoices.ItemIndex = 0 then
    exit;
  Result := TSmashSetting(lstChoices[cbChoices.ItemIndex - 1]);
end;

function TChoicePanel.GetLabel: string;
begin
  Result := lblSig.Caption;
end;

procedure TChoicePanel.SetSelectCallback(callback: TNotifyEvent);
begin
  SelectCallback := callback;
end;

procedure TChoicePanel.SetLabel(sig: string);
begin
  lblSig.Caption := sig;
end;

procedure TChoicePanel.AddChoice(choice: TSmashSetting);
begin
  lstChoices.Add(choice);
  cbChoices.Items.Add(choice.name);
  cbChoices.ItemIndex := 0;
end;

end.
