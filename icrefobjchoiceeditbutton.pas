unit icrefobjchoiceeditbutton;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, EditBtn;

type
  TICRefObjChoiceEditButton = class(TEditButton)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I icrefobjchoiceeditbutton_icon.lrs}
  RegisterComponents('IC Tools',[TICRefObjChoiceEditButton]);
end;

end.
