{
Форма редактирования конфигурации в XML файле.

Версия: 0.0.1
}
unit edit_xml_config_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ValEdit,
  ICXMLConfig;

type

  { TEditXmlConfigForm }

  TEditXmlConfigForm = class(TForm)
    ObjectListBox: TListBox;
    CtrlSplitter: TSplitter;
    PropertyValueListEditor: TValueListEditor;
  private
    FXmlConfig: TICXMLConfig;

  public
    { Запуск редактирования XML конфигурации }
    function EditConfig(aXmlConfig: TICXMLConfig): Boolean;
    { Заполнить блок XML файла конфигурации }
    function Build(aPath: WideString): Boolean;

  end;

var
  EditXmlConfigForm: TEditXmlConfigForm;

implementation

{$R *.lfm}

{ Запуск редактирования XML конфигурации }
function TEditXmlConfigForm.EditConfig(aXmlConfig: TICXMLConfig): Boolean;
begin
  ShowModal();
end;

{ Заполнить блок XML файла конфигурации }
function TEditXmlConfigForm.Build(aPath: WideString): Boolean;
begin

end;

end.

