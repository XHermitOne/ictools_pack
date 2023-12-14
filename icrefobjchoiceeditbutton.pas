{
Контрол выбора элемента иерархического справочника
Версия: 0.0.0.1
}

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
    { Справочник }
    FRefObj: TICRefObjDataSource;
    { Текущий выбранный код справочника }
    FSelectedCod: String;

  public
    // Create
    constructor Create(AOwner:TComponent); override;
    // Destroy
    destructor Destroy; override;

    procedure Choice();

  published
    property RefObj: TICRefObjDataSource read FRefObj write FRefObj;
    property SelectedCod: String read FSelectedCod;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I icrefobjchoiceeditbutton_icon.lrs}
  RegisterComponents('IC Tools',[TICRefObjChoiceEditButton]);
end;

constructor TICRefObjChoiceEditButton.Create(AOwner:TComponent);
begin
  inherited;

  FSelectedCod := '';
  { Делаем не редактируемым поле ввода }
  Edit.ReadOnly := True;
end;

// Destroy
destructor TICRefObjChoiceEditButton.Destroy;
begin
  inherited;
end;

procedure TICRefObjChoiceEditButton.Choice();
var
  ref_name: AnsiString;
begin
  if FRefObj <> nil then
  begin
    FSelectedCod := FRefObj.ChoiceCod();
    ref_name := FRefObj.GetColumnNameValue(FSelectedCod);
    Text := ref_name;
  end;
end;

end.
