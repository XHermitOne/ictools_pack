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
  TREE_ITEM_LABEL = '...';

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
  protected
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

  public
    { Установить справочник для формы }
    procedure SetRefObj(ARefObj: TICRefObjDataSource);
    { Инициализация диалоговой формы }
    procedure Init(AFields: Array of String);
    { Заполнить дерево справочника }
    procedure SetRefObjTree(ASortColumn: Array of string);
    { Обновить дерево справочника }
    procedure RefreshRefObjTree(ASortColumn: Array of string);
    { Проверка обратной сортировки }
    function IsReverseSort(ASortColumn: String): Boolean;
    function IsReverseSort(ASortColumn: Integer): Boolean;
    { Отсортировать набор записей }
    function SortRefObjRecordset(ARecordSet: TDataSet; ASortColumn: String): TDataSet;
    { Установить элемент дерева }
    procedure SetRefObjTreeItem(AParentItem: TTreeListViewItem; ARecordSet: TDataSet);
    { Наити элемент дерева }
    function FindTreeChildItem(AItemText: AnsiString; ACurItem: TTreeListViewItem): TTreeListViewItem;
    { Инициализация ветки дерева }
    procedure InitLevelTree(AItem: TTreeListViewItem; ARecordSet: TDataSet);
    { Поиск элемента дерева по коду }
    function FindRefObjTreeItem(AParentItem: TTreeListViewItem; ACode: String; ARecordSet: TDataSet): TTreeListViewItem;
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

function TChoiceRefObjForm.SortRefObjRecordset(ARecordSet: TDataSet; ASortColumn: String): TDataSet;
var
  sort_field: String;
begin
  if ARecordSet.IsEmpty() then
  begin
    Result := ARecordSet;
    Exit;
  end;

  sort_field := GetSortField(ASortColumn);

  if not strfunc.IsEmpty(sort_field) then
    Resut := ARecordSet;
  else
    Result := ARecordSet;
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
  if strfunc.IsStartsWith(ASortColumn, SORT_REVERSE_SIGN) then
  	Result := Copy(ASortColumn, 1, Length(ASortColumn) - 1)
  else
    Result := ASortColumn;

  Result := strfunc.GetIdxStrInList(ASortColumn, FRefObjColNames);
end;

function TChoiceRefObjForm.IsReverseSort(ASortColumn: String): Boolean;
begin
  Result := strfunc.IsStartsWith(ASortColumn, SORT_REVERSE_SIGN);
end;

function TChoiceRefObjForm.IsReverseSort(ASortColumn: Integer): Boolean;
begin
  Result := ASortColumn < 0;
end;

procedure TChoiceRefObjForm.RefreshRefObjTree(ASortColumn: Array of string);
var
  selected_code, title: String;
begin
  selected_code := GetSelectedCode();
  { Сначала удаляем все элементы дерева }
  RefObjTreeListView.Items.Clear;

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

  if strfunc.IsEmpty(selected_code) then
    SelectRefObjTreeItem(selected_code)
  else
    { Распахнуть корневой элемент }
    FRefObjTreeListView.Expand();

end;

procedure TChoiceRefObjForm.SetRefObjTreeItem(AParentItem: TTreeListViewItem; ARecordSet: TDataSet);
var
  code, field_name, value: String;
  is_activate: Boolean;
  i: Integer;
  item: TTreeListViewItem;
begin
  code := ARecordSet.FieldByName[FRefObj.CodColumnName].AsString;
  is_activate := ARecordSet.FieldByName[FRefObj.ActivateColumnName].AsBoolean;

  if is_activate then
  begin
    item := AParentItem.SubItems.Add(code);
    { Здесь надо привязать запись к элементу справочника}
    item.Data := ARecordSet.RecNo;
    { Заполнение колонок }
    for i := 1 to Length(FRefObjColNames) - 1 do
    begin
      field_name := FRefObjColNames[i];
      value := ARecordSet.FieldByName[field_name].AsString;
      AParentItem.RecordItems.Add.Text := value;
    end;
  end;
end;

function TChoiceRefObjForm.FindTreeChildItem(AItemText: AnsiString; ACurItem: TTreeListViewItem): TTreeListViewItem;
var
  i: Integer;
  child_item: TTreeListViewItem;
begin
  if ACurItem is nil then
    ACurItem := FRefObjTreeListView.Items[0];

  Result := nil;
	for i := 0 to ACurItem.SubItems.Count - 1 do
  begin
  	child_item := ACurItem.SubItems[i];
    if child_item.GetText() = AItemText then
    begin
      Result := child_item;
      break;
    end;
  end;
end;

procedure TChoiceRefObjForm.InitLevelTree(AItem: TTreeListViewItem; ARecordSet: TDataSet);
var
  find_item: TTreeListViewItem;
  code: String;
begin
  find_item := FindTreeChildItem(TREE_ITEM_LABEL, AItem);

  if find_item is not nil then
    FRefObjTreeListView.RemoveSelection();

  if AItem.SubItems.Count = 0 then
  begin
    { Получить запись }
    ARecordSet.MoveBy(AItem.Data);
    { Получить код }
    code := ARecordSet.FieldByName[FRefObj.CodColumnName];
    SetRefObjLevelTree(AItem, code);
  end;
end;

function TChoiceRefObjForm.FindRefObjTreeItem(AParentItem: TTreeListViewItem; ACode: String; ARecordSet: TDataSet): TTreeListViewItem;
var
  find_result, child_item: TTreeListViewItem;
  i: Integer;
begin
  { Поиск в текущем элементе }
  { Получить запись }
  ARecordSet.MoveBy(AParentItem.Data);
  if ARecordSet.FieldByName[FRefObj.CodColumnName] = ACode then
  begin
    Result := AParentItem;
    Exit;
  end;

  { Поиск в дочерних элементах }
  find_result := nil;
  for i := 0 to AParentItem.SubItems.Count - 1 do
  begin
    child_item := AParentItem.SubItems[i];
    ARecordSet.MoveBy(child_item.Data);
    if ARecordSet.FieldByName[FRefObj.CodColumnName] = ACode then
    begin
      find_result := child_item;
      break;
    end;
  end;

  { На этом уровне не нашли, необходимо искать уровнями ниже }
  if find_result is nil then
  begin
    for i := 0 to AParentItem.SubItems.Count - 1 do
    begin
      child_item := AParentItem.SubItems[i];
      InitLevelTree(child_item, ARecordSet);
      find_result := FindRefObjTreeItem(child_item, ACode, ARecordSet);
      if find_result is not nil then
        break;
    end;
  end;

  if find_result is not il then
    find_result.Expand;

  Result := find_result;
end;

end.

