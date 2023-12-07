{
Диалоговая форма выбора записи из справочника.

Версия: 0.0.0.1
}
unit choice_ref_obj_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  TreeListView;

const
  SORT_REVERSE_SIGN = '-';

type

  { TChoiceRefObjForm }

  TChoiceRefObjForm = class(TForm)
    OkBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
    EditBitBtn: TBitBtn;
    SearchFieldComboBox: TComboBox;
    FindEdit: TEdit;
    Label1: TLabel;
    FindSpeedButton: TSpeedButton;
    RefObjTreeListView: TTreeListView;
  private
    FRefObj: TICRefObjDataSource;
    //FImageList: TImageList;
    { Имена отображаемых полей }
    FRefObjColNames: Array of string;
    { Имена полей поиска }
    FRefObjSearchColNames: Array of string;

    { Инициализация изображений, если необходимо }
    procedure InitImages();
    { Инициализация колонок по описанию полей }
    procedure InitColumns(AFields: Array of String);
    { Инициализация полей поиска }
    procedure InitSearch(ASearchFields: Array of String);
    { Определить надпись по имени поля }
    function GetFieldLabel(AFieldDef: TFieldDef): AnsiString;
    { Обновить уровень дерева справочника }
    procedure SetRefObjLevelTree(AParentItem: TTreeListItem; ACod: String; ASortColumn: Array of string);
    { Определите имя поля сортировки, указав имя/ индекс столбца сортировки }
    function GetSortField(ASortColumn: String): String;
    function GetSortField(ASortColumn: Integer): String;
    function GetSortFieldIdx(ASortColumn: String): Integer;
    function GetSortFieldIdx(ASortColumn: Integer): Integer;

  public
    { Установить справочник для формы }
    procedure SetRefObj(ARefObj: TICRefObjDataSource);
    { Инициализация диалоговой формы }
    procedure Init(AFields: Array of String);
    { Заполнить дерево справочника }
    procedure SetRefObjTree(ASortColumn: Array of string);
    { Обновить дерево справочника }
    function RefreshRefObjTree();
    { Проверка обратной сортировки }
    function IsReverseSort();
    { Отсортировать набор записей }
    function SortRefObjRecordset(ARecordSet: TDataSet; ASortColumn: Array of string): TDataSet;
    { Установить элемент дерева }
    function SetRefObjItem();
    { Наити элемент дерева }
    function FindTreeChildItem();
    { Инициализация ветки дерева }
    function InitLevelTree();
    { Поиск элемента дерева по коду }
    function FindRefObjTreeItem();
    { Найти и выделить элемент дерева по коду }
    function SelectRefObjTreeItem();
    { Получить выбранный код }
    function GetSelectedCode();
    { Открыть форму редактирования справочника }
    function EditRefObj();
    { Определите имя выбранного поля, по которому мы будем выполнять поиск }
    function GetSearchFieldName();
    { }
    function ClearSearch();
    { Обновите найденные коды, соответствующие параметрам поиска }
    function GetSearchCodes();
    { Обновите найденные коды, соответствующие параметрам поиска.Определите индекс выбранного столбца }
    function GetSelectedColIdx();
    { Обновить сортировку по колонке }
    function RefreshSortColumn();

  end;

var
  ChoiceRefObjForm: TChoiceRefObjForm;

implementation

uses
  logfunc, strfunc;

{$R *.lfm}

procedure TChoiceRefObjForm.SetRefObj(ARefObj: TICRefObjDataSource);
begin
  FRefObj := ARefObj;
end;

procedure TChoiceRefObjForm.InitImages();
begin
  //FImageList:=TImageList.Create(self);
  //FImageList.Width:=16;
  //FImageList.Height:=16;
  //if FileExists('tv1.bmp') then begin
  //  bmp:=TBitmap.Create;
  //  bmp.LoadFromFile('tv1.bmp');
  //  ImageList.Add(bmp,nil);
  //  bmp:=TBitmap.Create;
  //  bmp.LoadFromFile('tv2.bmp');
  //  ImageList.Add(bmp,nil);
  //  bmp:=TBitmap.Create;
  //  bmp.LoadFromFile('tv3.bmp');
  //  ImageList.Add(bmp,nil);
  //  TreeListView1.Images:=ImageList;
  //end;
end;

procedure TChoiceRefObjForm.Init(AFields: Array of string);
begin
  InitImages();

  InitColumns(AFields);
  InitSearch();
  SetRefObjTree();
end;

procedure TChoiceRefObjForm.InitColumns(AFields: Array of string);
var
  field_names: Array of String;
  field_name: String;
  column_label: AnsiString;
  i: Integer;
begin
  if FRefObj is not nil then
      field_names := [FRefObj.CodColumnName, FRefObj.NameColumnName]
  else
    field_names := ['cod', 'name']
  end;
  if Length(AFields) then
    FRefObjColNames := field_names + AFields
  else
    FRefObjColNames := field_names
  end;

  if FRefObj is nil then
  begin
    logfunc.WarningMsg('Не определен справочник для выбора кода');
    Exit;
  end;
  if FRefObj.DataSet is nil then
  begin
    logfunc.WarningMsgFmt('Не определен объект набора данных для <%s>', [FRefObj.Name]);
    Exit;
  end;

	RefObjTreeListView.Columns.clear;
  for i := 0 to Length(FRefObjColNames) - 1 do
  begin
    field_name := FRefObjColNames[i];
    field := FRefObj.DataSet.FieldByName[field_name];
    column_label = GetFieldLabel(field);
    RefObjTreeListView.Columns.Add.Text := column_label;
    RefObjTreeListView.Columns.Add.Width := 100;
  end;
end;

procedure InitSearch(ASearchFields: Array of String);
var
  field_names: Array of String;
  field_name: String;
  column_label: AnsiString;
  i: Integer;
begin
  if FRefObj is not nil then
      field_names := [FRefObj.CodColumnName, FRefObj.NameColumnName]
  else
    field_names := ['cod', 'name']
  end;
  if Length(ASearchFields) then
    FRefObjSearchColNames := field_names + ASearchFields
  else
    FRefObjSearchColNames := field_names
  end;

  if FRefObj is nil then
  begin
    logfunc.WarningMsg('Не определен справочник для выбора кода');
    Exit;
  end;
  if FRefObj.DataSet is nil then
  begin
    logfunc.WarningMsgFmt('Не определен объект набора данных для <%s>', [FRefObj.Name]);
    Exit;
  end;

  SearchFieldComboBox.Items.Clear;
  for i := 0 to Length(FRefObjSearchColNames) - 1 do
  begin
    field_name := FRefObjColNames[i];
    field := FRefObj.DataSet.FieldByName[field_name];
    column_label = GetFieldLabel(field);
    SearchFieldComboBox.Items.Add(column_label);
  end;
end;

function TChoiceRefObjForm.GetFieldLabel(AFieldDef: TFieldDef): AnsiString;
begin
  Result := '';
  if AFieldDef is not nil then
    Result := AFieldDef.DisplayName;
end;

procedure TChoiceRefObjForm.SetRefObjTree(ASortColumn: Array of string);
var
  title: AnsiString;
begin
  if (FRefObj is not nil) and (not strfunc.IsEmpty(FRefObj.Description)) then
    title := FRefObj.Description
  else
    title := FRefObj.Name;

  RefObjTreeListView.BeginUpdate;
  { Добавляем корневой элемент дерева }
  TreeListView1.Items.Add.Text := title;

  if (FRefObj is not nil) or (FRefObj.IsEmpty()) then
    logfunc.WarningMsgFmt('Справочник <%s> пустой', [FRefObj.Name])
  else
  begin
    //if Length(ASortColumn) = 0 then
    //  sort_column = FSortColumn;
    SetRefObjLevelTree();
	end;

  RefObjTreeListView.EndUpdate;

  { Распахнуть корневой элемент }
end;

procedure TChoiceRefObjForm.SetRefObjLevelTree(AParentItem: TTreeListItem; ACod: String; ASortColumn: Array of string);
var
  level_data: TDataSet;
  i: Integer;
begin
  if FRefObj is nil then
  begin
    logfunc.WarningMsg('Не определен справочник для выбора кода');
    Exit;
  end;

  level_data := FRefObj.GetLevelRecsByCod(ACod);
  { Сортировка, если определены колонки сортировки }
  if Length(ASortColumn) > 0:
      level_data = SortRefObjRecordset(level_data, ASortColumn);
  if level_data.IsEmpty() then
  begin
    logfunc.WarningMsg('Нет данных');
    Exit;
  end;

  Screen.BeginWaitCursor;
  //{$IFDEF LINUX}Application.ProcessMessages;{$ENDIF}
  try
    level_data.First;
    while not level_data.EOF do
    begin

      SetRefObjTreeItem(AParentItem, level_data);
      level_data.Next;
    end;
    { Установить автоматически ширины колонок }
  except
    logfunc.
  end;
  Screen.EndWaitCursor;
end;

function TChoiceRefObjForm.SortRefObjRecordset(ARecordSet: TDataSet; ASortColumn: Array of string): TDataSet;
begin
end;

function TChoiceRefObjForm.GetSortField(ASortColumn: String): String;
begin
  if strfunc.IsStartsWith(ASortColumn, SORT_REVERSE_SIGN) then
  	Result := Copy(ASortColumn, 1, Length(ASortColumn) - 1)
  else
    Result := ASortColumn;

  if not strfunc.IsStrInList(Result, FRefObjColNames) then
  begin
    logfunc.WarningMsgFmt('Сортировка. Поле <%s> не найдено в %s', [Result, FRefObjColNames);
    Result := '';
	end;
end;

function TChoiceRefObjForm.GetSortField(ASortColumn: Integer): String;
begin
  // An increase of 1 is made in order to take
  // into account the first column with index 0
  //                                      V
  Result := FRefObjColNames[ASortColumn - 1]

  if not strfunc.IsStrInList(Result, FRefObjColNames) then
  begin
    logfunc.WarningMsgFmt('Сортировка. Поле <%s> не найдено в %s', [Result, FRefObjColNames);
    Result := '';
	end;
end;

function TChoiceRefObjForm.GetSortFieldIdx(ASortColumn: String): Integer;
begin
end;

end.

