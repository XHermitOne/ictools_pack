{
Модуль компонента работы с набором записей как с иерархическим справочником.
Иерархия организуется при помощи много уровневого кода.

Версия: 0.0.0.1
}
unit ICRefObjDataSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, db,
  SQLdb,
  dictionary, logfunc, strfunc, mathfunc;

const
  { Имена колонок по умолчанию }
  DEFAULT_COD_COLUMN_NAME = 'cod';
  DEFAULT_NAME_COLUMN_NAME = 'name';
  DEFAULT_ACTIVE_COLUMN_NAME = 'activate';


type
  { Источник данных. Справочник }
  TICRefObjDataSource = class(TDataSource)
  private
    { Описание справочника }
    FDEscription: AnsiString;
    { Длины кодов уровней }
    FCodLen: TStringList;
    { Наименонивание уровней }
    FLevelLabels: TStringList;
    { Флаг разрешения редактирования справочника }
    FCanEdit: Boolean;

    { Имя колонки кода }
    FCodColumnName: String;
    { Имя колонки наименования }
    FNameColumnName: String;
    { Имя колонки активации элемента справочника }
    FActiveColumnName: String;

    { Наименование таблицы }
    FTableName: String;
  protected

  public
    { Получить объект записи по коду }
    function GetRecByCod(ACod: String): TDataSet;
    { Получить объект записи по значению колонки }
    function GetRecByColValue(AColumnName: String; AColumnValue: String): TDataSet;
    { Получить код по значению колонки }
    function GetCodByColValue(AColumnName: String; AColumnValue: String): String;

    { Поиск записей по значению колонки }
    function SearchRecsByColValue(AColumnName: String; AColumnValue: String): TDataSet;
    { Поиск кодов по значению колонки }
    function SearchCodesByColValue(AColumnName: String; AColumnValue: String): TStringList;
    { Поиск записей по содержимому колонки }
    function SearchRecsByColContent(AColumnName: String; ASearchText: String;
                                    ACaseSensitive: Boolean;
                                    ADoSort: Boolean;
                                    AOrderBy: String): TDataSet;
    { Поиск записей по значениям нескольких колонок }
		function SearchRecords(AColumnValues: TStrDictionary): TDataSet;
    { Поиск кодов по значениям нескольких колонок }
		function SearchCodes(AColumnValues: TStrDictionary): TStringList;
    { Поиск записи по содержимому колонки }
    function FindRecByColContent(AColumnName: String; ASearchText: String;
                                 ACaseSensitive: Boolean;
                                 ADoSort: Boolean): TDataSet;

    { Получить словарь значений запрашиваемых колонок по коду }
    function GetColumnValues(ACod: String; AColumnNames: TStringList): TStrDictionary;
    { Получить значение колонки по коду }
		function GetColumnValue(ACod, AColumnName: String): Variant;
    { Получить наименование по коду }
		function GetColumnNameValue(ACod: String): AnsiString;

    { Пустой справочник? }
    function IsEmpty(): Boolean;
    { В справочнике присутствует код? }
		function HasCod(ACod: String): Boolean;
    { В справочнике присутствует наименование? }
		function HasName(AName: AnsiString): Boolean;
    { Элемент справочника с кодом активен? }
    function IsActive(ACod: String): Boolean;

    { Получить длину кода уровня }
    function GetLevelCodLen(ALevel: Integer): Integer;
    { Получить код в виде списка }
    function GetCodAsList(ACod: String): TStringList;
    { Количество уровней }
    function GetLevelCount(): Integer;
    { Получить список записей уровня по коду }
    function GetLevelRecsByCod(ACod: String): TDataSet;
    { Есть подкоды? }
    function HasChildrenCodes(AParentCod: String): Boolean;
    { Получить список дочерних кодов }
    function GetChildrenCodes(AParentCod: String): TStringList;
    { Получить индекс уровня по коду }
    function GetLevelIdxByCod(ACod: String): Integer;
    { Это код родительского уровня? }
    function IsParentLevelCod(ACod: String): Boolean;
    { Это код над-родительского уровня? }
    function IsGrandParentLevelCod(ACod: String): Boolean;

    { Запустить справочник на редактирование }
		function Edit(): Boolean;
    { Запустить процедуру выбора записи из справочника }
    function ChoiceRecord(): TDataSet;
    { Запустить процедуру выбора кода из справочника }
    function ChoiceCod(): String;

    { Сохранить элемент справочника }
    function Save(ACod: String; ARecord: TStrDictionary): Boolean;
    { Удалить элемент справочника по коду }
    function DelRecByCod(ACod: String): Boolean;

  published
    property Description: AnsiString read FDescription write FDescription;
    property CodColumnName: String read FCodColumnName write FCodColumnName;
    property NameColumnName: String read FNameColumnName write FNameColumnName;
    property ActiveColumnName: String read FActiveColumnName write FActiveColumnName;

    property TableName: String read FTableName write FTableName;

    property CodLen: TStringList read FCodLen write FCodLen;
    property LevelLabels: TStringList read FLevelLabels write FLevelLabels;
    property CanEdit: Boolean read FCanEdit write FCanEdit;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I icrefobjdatasource_icon.lrs}
  RegisterComponents('IC Tools',[TICRefObjDataSource]);
end;

{ Получить объект записи по коду }
function TICRefObjDataSource.GetRecByCod(ACod: String): TDataSet;
begin
  if DataSet.Locate(CodColumnName, ACod, []) then
    Result := DataSet
  else
  begin
    logfunc.WarningMsgFmt('Код <%s> не найден в справочнике <%s>', [ACod, Name]);
    Result := Nil;
  end;
end;

{ Получить объект записи по значению колонки }
function TICRefObjDataSource.GetRecByColValue(AColumnName: String; AColumnValue: String): TDataSet;
begin
  if DataSet.Locate(AColumnName, AColumnValue, []) then
    Result := DataSet
  else
  begin
    logfunc.WarningMsgFmt('Запись, соответствующая <%s : (%s)> не найдена в справочнике <%s>', [AColumnName, AColumnValue, Name]);
    Result := Nil;
  end;
end;

{ Получить код по значению колонки }
function TICRefObjDataSource.GetCodByColValue(AColumnName: String; AColumnValue: String): String;
var
  data_set: TDataSet;
begin
  data_set := GetRecByColValue(AColumnName, AColumnValue);
  if data_set <> Nil then
    Result := data_set.FieldByName(CodColumnName).AsString
  else
    Result := '';
end;


{ Поиск записей по значению колонки }
function TICRefObjDataSource.SearchRecsByColValue(AColumnName: String; AColumnValue: String): TDataSet;
var
  search_query: TSQLQuery;
begin
  // search_query.
  search_query.Close;

  search_query.Active := False;
  search_query.SQL.Text := 'SELECT * FROM :table_name WHERE (:column_name = :column_value)';
  search_query.Active := True;

  if not search_query.Prepared then
    search_query.Prepare;
  search_query.ParamByName('table_name').Value := TableName;
  search_query.ParamByName('column_name').Value := AColumnName;
  search_query.ParamByName('column_value').Value := AColumnValue;
  search_query.Open;

  Result := search_query;
end;


{ Поиск кодов по значению колонки }
function TICRefObjDataSource.SearchCodesByColValue(AColumnName: String; AColumnValue: String): TStringList;
var
  search_dataset: TDataSet;
begin
  Result := TStringList.Create();

  search_dataset := SearchRecsByColValue(AColumnName, AColumnValue);
  search_dataset.First;
  while not search_dataset.EOF do
    begin
      Result.Add(search_dataset.FieldValues[CodColumnName].AsString);
      search_dataset.Next;
    end;
  search_dataset.Destroy();
end;

{ Поиск записей по содержимому колонки }
function TICRefObjDataSource.SearchRecsByColContent(AColumnName: String; ASearchText: String;
				                                            ACaseSensitive: Boolean;
                                										ADoSort: Boolean;
                                										AOrderBy: String): TDataSet;
var
  search_query: TSQLQuery;
begin
  // search_query.
  search_query.Close;

  search_query.Active := False;
  search_query.SQL.Text := 'SELECT * FROM :table_name WHERE (:column_name :operation_like ''%%:column_value%%'') :sort_param :order_by_param';
  search_query.Active := True;

  if not search_query.Prepared then
    search_query.Prepare;
  search_query.ParamByName('table_name').Value := TableName;
  search_query.ParamByName('column_name').Value := AColumnName;
  search_query.ParamByName('column_value').Value := ASearchText;
  if ACaseSensitive then
    search_query.ParamByName('operation_like').Value := 'ILIKE'
  else
    search_query.ParamByName('operation_like').Value := 'LIKE';
  if ADoSort then
  begin
    search_query.ParamByName('sort_param').Value := 'ORDER BY';
    search_query.ParamByName('order_by_param').Value := AOrderBy;
  end
  else
  begin
    search_query.ParamByName('sort_param').Value := '';
    search_query.ParamByName('order_by_param').Value := '';
  end;
  search_query.Open;

  Result := search_query;
end;

{ Поиск записей по значениям нескольких колонок }
function TICRefObjDataSource.SearchRecords(AColumnValues: TStrDictionary): TDataSet;
var
  search_query: TSQLQuery;
  sql_text, sql_where_params: String;
  key, value: AnsiString;
  i: Integer;
begin

  for i := 0 to AColumnValues.GetKeys().Count - 1 do
  begin
    key := AColumnValues.GetKey(i);
    // value := AColumnValues.GetStrValue(key);
    if i = 0 then
      sql_where_params := Format(' :%s_column = :%s_value AND', [key, key])
    else
      sql_where_params := sql_where_params + Format(' AND :%s_column = :%s_value', [key, key]);
  end;
  sql_text := Format('SELECT * FROM :table_name WHERE (%s)', [sql_where_params]);
  logfunc.DebugMsgFmt('SQL format: %s', [sql_text]);

  // search_query.
  search_query.Close;

  search_query.Active := False;
  search_query.SQL.Text := sql_text;
  search_query.Active := True;

  if not search_query.Prepared then
    search_query.Prepare;
  search_query.ParamByName('table_name').Value := TableName;
  for i := 0 to AColumnValues.GetKeys().Count - 1 do
  begin
    key := AColumnValues.GetKey(i);
    value := AColumnValues.GetStrValue(key);
    search_query.ParamByName(Format('%s_column', [key])).Value := key;
    search_query.ParamByName(Format('%s_value', [key])).Value := value;
  end;
  search_query.Open;

  Result := search_query;
end;


{ Поиск кодов по значениям нескольких колонок }
function TICRefObjDataSource.SearchCodes(AColumnValues: TStrDictionary): TStringList;
var
  search_dataset: TDataSet;
begin
  Result := TStringList.Create();

  search_dataset := SearchRecords(AColumnValues);
  search_dataset.First;
  while not search_dataset.EOF do
    begin
      Result.Add(search_dataset.FieldValues[CodColumnName].AsString);
      search_dataset.Next;
    end;
  search_dataset.Destroy();
end;

{ Поиск записи по содержимому колонки }
function TICRefObjDataSource.FindRecByColContent(AColumnName: String; ASearchText: String;
                                                 ACaseSensitive: Boolean;
                             										 ADoSort: Boolean): TDataSet;
begin
  if ASearchText = '' then
  begin
    logfunc.WarningMsg('Не определен текст для поиска');
    Result := Nil;
    exit;
  end;

  if AColumnName = '' then
    AColumnName := CodColumnName;

  Result := SearchRecsByColContent(AColumnName, ASearchText, ACaseSensitive, ADoSort, CodColumnName);
  if Result <> Nil then
    Result.First;
end;

{ Получить словарь значений запрашиваемых колонок по коду }
function TICRefObjDataSource.GetColumnValues(ACod: String; AColumnNames: TStringList): TStrDictionary;
var
  data_set: TDataSet;
  i: Integer;
  column_name, value: AnsiString;
begin
  Result := TStrDictionary.Create();

  data_set := GetRecByCod(ACod);
  for i := 0 to AColumnNames.Count - 1 do
  begin
    column_name := AColumnNames.Strings[i];
    value := data_set.FieldValues[column_name].AsString;
    Result.AddStrValue(column_name, value);
  end;
end;

{ Получить значение колонки по коду }
function TICRefObjDataSource.GetColumnValue(ACod, AColumnName: String): Variant;
var
  data_set: TDataSet;
begin
  data_set := GetRecByCod(ACod);
  Result := data_set.FieldValues[AColumnName];
end;

{ Получить наименование по коду }
function TICRefObjDataSource.GetColumnNameValue(ACod: String): AnsiString;
var
  data_set: TDataSet;
begin
  data_set := GetRecByCod(ACod);
  Result := data_set.FieldValues[NameColumnName].AsString;
end;


{ Пустой справочник? }
function TICRefObjDataSource.IsEmpty(): Boolean;
begin
  if DataSet <> Nil then
    Result := DataSet.IsEmpty
  else
    Result := True;
end;

{ В справочнике присутствует код? }
function TICRefObjDataSource.HasCod(ACod: String): Boolean;
begin
  Result := GetRecByCod(ACod) <> Nil;
end;

{ В справочнике присутствует наименование? }
function TICRefObjDataSource.HasName(AName: AnsiString): Boolean;
begin
  Result := GetRecByColValue(NameColumnName, AName) <> Nil;
end;

{ Элемент справочника с кодом активен? }
function TICRefObjDataSource.IsActive(ACod: String): Boolean;
begin
  Result := GetRecByCod(ACod).FieldValues[ActiveColumnName].AsBoolean;
end;

{ Получить длину кода уровня }
function TICRefObjDataSource.GetLevelCodLen(ALevel: Integer): Integer;
begin
  Result := StrToInt(CodLen.Strings[ALevel]);
end;

{ Получить код в виде списка }
function TICRefObjDataSource.GetCodAsList(ACod: String): TStringList;
var
  i: Integer;
  i_start, i_stop: Integer;
  sub_cod: AnsiString;
begin
  Result := TStringList.Create();
  if CodLen.Count > 0 then
    for i := 0 to CodLen.Count - 1 do
    begin
      i_start := mathfunc.SumRangeAsInteger(CodLen, 0, i);
      i_stop := mathfunc.SumRangeAsInteger(CodLen, 0, i + 1);
      sub_cod := Copy(ACod, i_start, i_stop);
      Result.Add(sub_cod);
    end
  else
    Result.Add(ACod);
end;

{ Количество уровней }
function TICRefObjDataSource.GetLevelCount(): Integer;
begin
  Result := CodLen.Count;
end;

{ Получить список записей уровня по коду }
function TICRefObjDataSource.GetLevelRecsByCod(ACod: String): TDataSet;
var
  search_query: TSQLQuery;
begin
  // search_query.
  search_query.Close;

  search_query.Active := False;
  search_query.SQL.Text := 'SELECT * FROM :table_name WHERE (:column_name LIKE '':cod_value%%'' AND :column_name <> '':cod_value'')';
  search_query.Active := True;

  if not search_query.Prepared then
    search_query.Prepare;
  search_query.ParamByName('table_name').Value := TableName;
  search_query.ParamByName('column_name').Value := CodColumnName;
  search_query.ParamByName('cod_value').Value := ACod;
  search_query.Open;

  Result := search_query;
end;

{ Есть подкоды? }
function TICRefObjDataSource.HasChildrenCodes(AParentCod: String): Boolean;
var
  data_set: TDataSet;
begin
  data_set := GetLevelRecsByCod(APArentCod);
  Result := not data_set.IsEmpty;
  data_set.Destroy();
end;

{ Получить список дочерних кодов }
function TICRefObjDataSource.GetChildrenCodes(AParentCod: String): TStringList;
var
  data_set: TDataSet;
  cod: String;
begin
  Result := TStringList.Create();
  data_set := GetLevelRecsByCod(APArentCod);

  data_set.First;
  while not data_set.EOF do
  begin
    cod := data_set.FieldValues[CodColumnName].AsString;
    Result.Add(cod);
  end;
  data_set.Destroy();
end;

{ Получить индекс уровня по коду }
function TICRefObjDataSource.GetLevelIdxByCod(ACod: String): Integer;
var
  i: Integer;
begin
  for i := 0 to CodLen.Count - 1 do
  begin
    // level_cod_len := StrToInt(CodLen.Strings[i]);
    if Length(ACod) = mathfunc.SumRangeAsInteger(CodLen, 0, i+1) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

{ Это код родительского уровня? }
function TICRefObjDataSource.IsParentLevelCod(ACod: String): Boolean;
var
  cod_list: TStringList;
  level_count: Integer;
begin
  cod_list := GetCodAsList(ACod);
  level_count := GetLevelCount();
  Result := cod_list.Count < level_count;
end;

{ Это код над-родительского уровня? }
function TICRefObjDataSource.IsGrandParentLevelCod(ACod: String): Boolean;
var
  cod_list: TStringList;
  level_count: Integer;
begin
  cod_list := GetCodAsList(ACod);
  level_count := GetLevelCount();
  Result := cod_list.Count < (level_count - 1);
end;

{ Запустить справочник на редактирование }
function TICRefObjDataSource.Edit(): Boolean;
begin
  Result := True;
end;

{ Запустить процедуру выбора записи из справочника }
function TICRefObjDataSource.ChoiceRecord(): TDataSet;
begin
  Result := Nil;
end;

{ Запустить процедуру выбора кода из справочника }
function TICRefObjDataSource.ChoiceCod(): String;
begin
  Result := '';
end;


{ Сохранить элемент справочника }
function TICRefObjDataSource.Save(ACod: String; ARecord: TStrDictionary): Boolean;
begin
  Result := True;
end;

{ Удалить элемент справочника по коду }
function TICRefObjDataSource.DelRecByCod(ACod: String): Boolean;
begin
  Result := True;
end;

end.
