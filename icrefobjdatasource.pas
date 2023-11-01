unit ICRefObjDataSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, db;

type
  TICRefObjDataSource = class(TDataSource)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I icrefobjdatasource_icon.lrs}
  RegisterComponents('Data Access',[TICRefObjDataSource]);
end;

end.
