unit msSettingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, ExtCtrls,
  // ms units
  msFrontend;

type
  TSettingForm = class(TForm)
    PageControl: TPageControl;
    GeneralTabSheet: TTabSheet;
    TreePopupMenu: TPopupMenu;
    btnCancel: TButton;
    btnOK: TButton;
    lblName: TLabel;
    edName: TEdit;
    meDescription: TMemo;
    TreeView: TTreeView;
    RightPanel: TPanel;
    lblDescription: TLabel;
    procedure LoadTree;
    procedure DumpTree;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    setting: TSmashSetting;
  end;

var
  SettingForm: TSettingForm;

implementation

{$R *.dfm}

procedure TSettingForm.LoadTree;
begin
  // ?
end;

procedure TSettingForm.DumpTree;
begin
  // ?
end;

procedure TSettingForm.FormShow(Sender: TObject);
begin
  edName.Text := setting.name;
  meDescription.Lines.Text := setting.description;
  LoadTree;
end;

procedure TSettingForm.btnOKClick(Sender: TObject);
begin
  setting.name := edName.Text;
  setting.description := meDescription.Lines.Text;
  DumpTree;
end;

end.
