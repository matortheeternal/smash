unit msPriorityForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TPriorityForm = class(TForm)
    lblPriority: TLabel;
    lblPrompt: TLabel;
    btnCancel: TButton;
    btnOK: TButton;
    edPriority: TEdit;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    priority: Byte;
  end;

var
  PriorityForm: TPriorityForm;

implementation

{$R *.dfm}

procedure TPriorityForm.btnOKClick(Sender: TObject);
begin
  priority := StrToInt(edPriority.Text) mod 255;
  ModalResult := mrOK;
end;

end.
