unit icrefobjchoiceeditbutton;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, EditBtn,
  ICRefObjDataSource;

type
  TICRefObjChoiceEditButton = class(TEditButton)
  private

  protected
    FRefObj: TICRefObjDataSource;

  public
    procedure Choice();

  published
    property RefObj: TICRefObjDataSource read FRefObj write FRefObj;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I icrefobjchoiceeditbutton_icon.lrs}
  RegisterComponents('IC Tools',[TICRefObjChoiceEditButton]);
end;

procedure TICRefObjChoiceEditButton.Choice();
var
  cod: String;
  ref_name: AnsiString;
begin
  if FRefObj <> nil then
  begin
    cod := FRefObj.ChoiceCod();
    ref_name := FRefObj.GetColumnNameValue(cod);
    Text := ref_name;
  end;
end;

end.
