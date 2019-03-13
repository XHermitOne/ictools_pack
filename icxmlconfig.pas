{
Модуль компонента управления файлом XML.

Версия: 0.0.2
}
unit ICXMLConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, XMLConf;

type
  TICXMLConfig = class(TXMLConfig)
  private
    { Имя XML файла по умолчанию. В случае если не определено основное имя }
    FDefaultFileName: AnsiString;

    procedure SetDefaultFileName(ADefaultFileName: AnsiString);

  protected

  public
    {
    Получить значение как целое
    @param APath: Путь до элемента. Например /L1/L2/L3
    @param ADefault: Значение по умолчанию в случае не существующего элемента или не корректных данных.
    }
    function GetValueAsInteger(APath: WideString; ADefault: Integer=0): Integer;
    {
    Получить значение как вещественное
    @param APath: Путь до элемента. Например /L1/L2/L3
    @param ADefault: Значение по умолчанию в случае не существующего элемента или не корректных данных.
    }
    function GetValueAsFloat(APath: WideString; ADefault: Double=0.0): Double;
    {
    Получить значение как логическое
    @param APath: Путь до элемента. Например /L1/L2/L3
    @param ADefault: Значение по умолчанию в случае не существующего элемента или не корректных данных.
    }
    function GetValueAsBoolean(APath: WideString; ADefault: Boolean=False): Boolean;

    {
    Получить значение как строку
    @param APath: Путь до элемента. Например /L1/L2/L3
    @param ADefault: Значение по умолчанию в случае не существующего элемента или не корректных данных.
    }
    function GetValueAsString(APath: WideString; ADefault: AnsiString=''): AnsiString;

    {
    Запуск редактора конфигурационного файла XML
    }
    function Edit(): Boolean;

  published
    property DefaultFileName: AnsiString read FDefaultFileName write SetDefaultFileName;
  end;

procedure Register;

implementation

uses
  edit_xml_config_form;

procedure Register;
begin
  {$I icxmlconfig_icon.lrs}
  RegisterComponents('IC Tools',[TICXMLConfig]);
end;

procedure TICXMLConfig.SetDefaultFileName(ADefaultFileName: AnsiString);
begin
  if ADefaultFileName = '' then
  begin
    ADefaultFileName := FileName;
  end;

  FDefaultFileName := ADefaultFileName;
end;

{ Получить значение как целое }
function TICXMLConfig.GetValueAsInteger(APath: WideString; ADefault: Integer): Integer;
var
  value: WideString;
begin
  if FileName = '' then
    FileName := DefaultFileName;

  value := GetValue(APath, '');
  if value = '' then
    Result := ADefault
  else
    Result := StrToInt(value);
end;

{ Получить значение как вещественное }
function TICXMLConfig.GetValueAsFloat(APath: WideString; ADefault: Double): Double;
var
  value: WideString;
begin
  if FileName = '' then
    FileName := DefaultFileName;

  value := GetValue(APath, '');
  if value = '' then
    Result := ADefault
  else
    Result := StrToFloat(value);
end;

{ Получить значение как логическое }
function TICXMLConfig.GetValueAsBoolean(APath: WideString; ADefault: Boolean): Boolean;
var
  value: WideString;
begin
  if FileName = '' then
    FileName := DefaultFileName;

  value := GetValue(APath, '');
  if value = '' then
    Result := ADefault
  else
    Result := LowerCase(value) = 'true';
end;

{ Получить значение как строку }
function TICXMLConfig.GetValueAsString(APath: WideString; ADefault: AnsiString): AnsiString;
begin
  if FileName = '' then
    FileName := DefaultFileName;

  Result := GetValue(APath, ADefault);
end;

{
Запуск редактора конфигурационного файла XML
}
function TICXMLConfig.Edit(): Boolean;
begin
  edit_xml_config_form.EditXmlConfigForm.EditConfig(self);
  Result := True;
end;

end.
