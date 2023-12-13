{
Модуль компонента работы с набором записей как с иерархическим справочником.
Иерархия организуется при помощи много уровневого кода.

Версия: 0.0.1.1
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
  DEFAULT_ID_COLUMN_NAME = 'id';
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
    FCodLen: TStrings;
    { Наименонивание уровней }
    FLevelLabels: TStrings;
    { Флаг разрешения редактирования справочника }
    FCanEdit: Boolean;

    { Имя колонки идентификатора }
    FIdColumnName: String;
    { Имя колонки кода }
    FCodColumnName: String;
    { Имя колонки наименования }
    FNameColumnName: String;
    { Имя колонки активации элемента справочника }
    FActiveColumnName: String;

    { Наименование таблицы }
    FTableName: String;
  protected
    procedure SetCodLen(AValues: TStrings); virtual;
    procedure SetLevelLabels(AValues: TStrings); virtual;

  public
    // Create
    constructor Create(AOwner:TComponent); override;
    // Destroy
    destructor Destroy; override;

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
    function GetLevelRecsByCod(ACod: String; ARecordSet: TDataSet): TDataSet;
    { Получить все активные записи справочника }
    function GetAllActiveRecs(): TDataSet;
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
    property IdColumnName: String read FIdColumnName write FIdColumnName;
    property CodColumnName: String read FCodColumnName write FCodColumnName;
    property NameColumnName: String read FNameColumnName write FNameColumnName;
    property ActiveColumnName: String read FActiveColumnName write FActiveColumnName;

    property TableName: String read FTableName write FTableName;

    property CodLen: TStrings read FCodLen write SetCodLen;
    property LevelLabels: TStrings read FLevelLabels write SetLevelLabels;
    property CanEdit: Boolean read FCanEdit write FCanEdit;

  end;

procedure Register;

implementation

uses
  choice_ref_obj_form;

procedure Register;
begin
  {$I icrefobjdatasource_icon.lrs}
  RegisterComponents('IC Tools',[TICRefObjDataSource]);
end;

constructor TICRefObjDataSource.Create(AOwner:TComponent);
begin
  inherited;

  FIdColumnName := 'id';
  FCodColumnName := 'cod';
  FNameColumnName := 'name';
  FActiveColumnName := 'activate';

  FCodLen := TStringList.Create;
  FLevelLabels := TStringList.Create;
end;

// Destroy
destructor TICRefObjDataSource.Destroy;
begin
  FreeAndNil(FLevelLabels);
  FreeAndNil(FCodLen);

  inherited;
end;

procedure TICRefObjDataSource.SetCodLen(AValues: TStrings);
begin
  if (AValues <> FCodLen) then
  begin
    //LockSelectionChange;
    FCodLen.Assign(AValues);
    //UnlockSelectionChange;
  end;
end;

procedure TICRefObjDataSource.SetLevelLabels(AValues: TStrings);
begin
  if (AValues <> FLevelLabels) then
  begin
    //LockSelectionChange;
    FLevelLabels.Assign(AValues);
    //UnlockSelectionChange;
  end;
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
var
  sql_query: TSQlQuery;
begin
  sql_query := TSQLQuery(GetAllActiveRecs());
  if sql_query <> nil then
  begin
    Result := sql_query.IsEmpty;
    sql_query.Close;
  end
  else
    Result := True;
end;

{ В справочнике присутствует код? }
function TICRefObjDataSource.HasCod(ACod: String): Boolean;
begin
  Result := GetRecByCod(ACod) <> nil;
end;

{ В справочнике присутствует наименование? }
function TICRefObjDataSource.HasName(AName: AnsiString): Boolean;
begin
  Result := GetRecByColValue(NameColumnName, AName) <> nil;
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
function TICRefObjDataSource.GetLevelRecsByCod(ACod: String; ARecordSet: TDataSet): TDataSet;
var
  search_query: TSQlQuery;
  cod_len, next_level_idx: Integer;
begin
  if ARecordSet = nil then
    search_query := TSQLQuery.Create(Application)
  else
    search_query := TSQLQuery(ARecordSet);
  search_query.DataBase := TSQLQuery(DataSet).DataBase;

  try
    // search_query.
    search_query.Close;

    //search_query.Active := False;
    search_query.SQL.Clear;
    if strfunc.IsEmptyStr(ACod) then
    begin
      cod_len := StrToInt(FCodLen.Strings[0]);
      search_query.SQL.Text := Format('SELECT * FROM %s WHERE LENGTH(%s) = %d AND activate = 1', [TableName, CodColumnName, cod_len]);
    end
    else
    begin
      next_level_idx := GetLevelIdxByCod(ACod) + 1;
      if next_level_idx >= FCodLen.Count then
      begin
        Result := search_query;
        Exit;
      end;
      cod_len := mathfunc.SumRangeAsInteger(FCodLen, 0, next_level_idx + 1);
      search_query.SQL.Text := Format('SELECT * FROM %s WHERE LENGTH(%s) = %d AND %s LIKE ''%s%%'' AND activate = 1', [TableName, CodColumnName, cod_len, CodColumnName, ACod]);

    end;
    logfunc.DebugMsgFmt('SQL: %s', [search_query.SQL.Text], True);
    //search_query.Active := True;

    //if not search_query.Prepared then
    //  search_query.Prepare;
    search_query.Open;
  except
    logfunc.FatalMsg('Ошибка БД', True);
  end;

  Result := search_query;
end;

function TICRefObjDataSource.GetAllActiveRecs(): TDataSet;
var
  sql_query: TSQlQuery;
begin
  sql_query := TSQLQuery(DataSet);
  if sql_query <> nil then
  begin
    sql_query.Close;
    sql_query.SQL.Text := Format('SELECT * FROM %s WHERE %s = 1', [TableName, ActiveColumnName]);
    sql_query.Open;
  end;
  Result := sql_query;
end;

{ Есть подкоды? }
function TICRefObjDataSource.HasChildrenCodes(AParentCod: String): Boolean;
var
  sql_query: TSQLQuery;
begin
  sql_query := TSQLQuery.Create(Application);
  sql_query := TSQLQuery(GetLevelRecsByCod(AParentCod, sql_query));
  Result := not sql_query.IsEmpty();
  sql_query.Close();
  FreeAndNil(sql_query);
end;

{ Получить список дочерних кодов }
function TICRefObjDataSource.GetChildrenCodes(AParentCod: String): TStringList;
var
  sql_query: TSQLQuery;
  cod: String;
begin
  Result := TStringList.Create();

  sql_query := TSQlQuery.Create(Application);
  sql_query := TSQLQuery(GetLevelRecsByCod(APArentCod, sql_query));

  sql_query.First;
  while not sql_query.EOF do
  begin
    cod := sql_query.FieldValues[CodColumnName].AsString;
    Result.Add(cod);
  end;
  FreeAndNil(sql_query);
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
  Result := choice_ref_obj_form.ChoiceRefObjCodDialogForm(self, '', []);
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
