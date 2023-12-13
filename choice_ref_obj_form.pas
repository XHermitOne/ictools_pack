{
Диалоговая форма выбора записи из справочника.

Версия: 0.0.0.1
}
unit choice_ref_obj_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  TreeListView, findControl, db,
  exttypes, ICRefObjDataSource;

const
  SORT_REVERSE_SIGN = '-';
  TREE_ITEM_LABEL = '...';

type

  { TChoiceRefObjForm }

  TChoiceRefObjForm = class(TForm)
    OkBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
    EditBitBtn: TBitBtn;
    RefObjTreeListView: TTreeListView;
  protected
    FRefObj: TICRefObjDataSource;
    //FImageList: TImageList;
    { Имена отображаемых полей }
    FRefObjColNames: TArrayOfString;
    { Имена полей поиска }
    FRefObjSearchColNames: TArrayOfString;

    { Найденные коды }
    FSearchCodes: TArrayOfString;
    FSearchCodeIdx: Integer;
    FNotActualSearch: Boolean;

    { Колонка сортировки }

    FSortColumn: String;
    { Инициализация изображений, если необходимо }
    procedure InitImages();
    { Инициализация колонок по описанию полей }
    procedure InitColumns(AFields: TArrayOfString);
    { Инициализация полей поиска }
    procedure InitSearch(ASearchFields: TArrayOfString);
    { Определить надпись по имени поля }
    function GetFieldLabel(AFieldDef: TFieldDef): AnsiString;
    { Определить ширину колонки по имени поля }
    function GetColumnWidth(AFieldDef: TFieldDef): Integer;
    { Обновить уровень дерева справочника }
    procedure SetRefObjLevelTree(AParentItem: TTreeListItem; ACod: String; ASortColumn: String);
    { Определите имя поля сортировки, указав имя/ индекс столбца сортировки }
    function GetSortField(ASortColumn: String): String;
    function GetSortField(ASortColumn: Integer): String;
    //function GetSortFieldIdx(ASortColumn: String): Integer;

  public
    { Установить справочник для формы }
    procedure SetRefObj(ARefObj: TICRefObjDataSource);
    { Инициализация диалоговой формы }
    procedure Init(AFields: TArrayOfString);
    { Заполнить дерево справочника }
    procedure SetRefObjTree(ASortColumn: String);
    { Обновить дерево справочника }
    procedure RefreshRefObjTree(ASortColumn: String);
    { Проверка обратной сортировки }
    function IsReverseSort(ASortColumn: String): Boolean;
    function IsReverseSort(ASortColumn: Integer): Boolean;
    { Отсортировать набор записей }
    function SortRefObjRecordset(ARecordSet: TDataSet; ASortColumn: String): TDataSet;
    { Установить элемент дерева }
    procedure SetRefObjTreeItem(AParentItem: TTreeListItem; ARecordSet: TDataSet);
    { Наити элемент дерева }
    function FindTreeChildItem(AItemText: AnsiString; ACurItem: TTreeListItem): TTreeListItem;
    { Инициализация ветки дерева }
    procedure InitLevelTree(AItem: TTreeListItem; ARecordSet: TDataSet);
    { Поиск элемента дерева по коду }
    function FindRefObjTreeItem(AParentItem: TTreeListItem; ACode: String; ARecordSet: TDataSet): TTreeListItem;
    { Найти и выделить элемент дерева по коду }
    function SelectRefObjTreeItem(AParentItem: TTreeListItem; ACode: String; ARecordSet: TDataSet): TTreeListItem;
    { Получить выбранный код }
    function GetSelectedCode(ARecordSet: TDataSet): String;
    { Открыть форму редактирования справочника }
    function EditRefObj(): Boolean;
    { }
    procedure ClearSearch();
    { Определите имя выбранного поля, по которому мы будем выполнять поиск }
    function GetSearchFieldName(): String;
    { Обновите найденные коды, соответствующие параметрам поиска }
    function GetSearchCodes(ASearchTxt: String; ASearchFieldName: String; ARecordSet: TDataSet): TArrayOfString;
    { Обновите найденные коды, соответствующие параметрам поиска.Определите индекс выбранного столбца }
    //function GetSelectedColIdx();
    { Обновить сортировку по колонке }
    //function RefreshSortColumn();

  end;

function ChoiceRefObjCodDialogForm(ARefObj: TICRefObjDataSource; ADefaultCod: String; AFields: TArrayOfString): String;

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

procedure TChoiceRefObjForm.Init(AFields: TArrayOfString);
begin
  InitImages();

  InitColumns(AFields);
  //InitSearch(AFields);

  RefObjTreeListView.CreateSearchBar();
  with RefObjTreeListView.SearchBar do
  begin
    Parent := self;
    Align := alTop;
    Caption := 'Найти:';
    HighlightText := 'Выделить все';
    SearchBackwardText := 'Предыдущий';
    SearchForwardText := 'Следующий';
    SubComponents := [fscCaption, fscSelectLocation,
                      fscSearchForward, fscSearchBackwards, fscHighlight, fscStatus];
    SearchLocations.Strings[0] := 'Все';
  end;
  //RefObjTreeListView.UpdateScrollBarPos;

  SetRefObjTree('');
  RefObjTreeListView.ColumnsAutoSize();
end;

procedure TChoiceRefObjForm.InitColumns(AFields: TArrayOfString);
var
  field_names: TArrayOfString;
  field_name: String;
  field_def: TFieldDef;
  i: Integer;
begin
  if FRefObj <> nil then
      field_names := [FRefObj.CodColumnName, FRefObj.NameColumnName]
  else
    field_names := ['cod', 'name'];

  if Length(AFields) > 0 then
    FRefObjColNames := strfunc.JoinArrayOfString(field_names, AFields)
  else
    FRefObjColNames := field_names;

  if FRefObj = nil then
  begin
    logfunc.WarningMsg('Не определен справочник для выбора кода');
    Exit;
  end;
  if FRefObj.DataSet = nil then
  begin
    logfunc.WarningMsgFmt('Не определен объект набора данных для <%s>', [FRefObj.Name]);
    Exit;
  end;

	RefObjTreeListView.Columns.clear;
  for i := 0 to Length(FRefObjColNames) - 1 do
  begin
    field_name := FRefObjColNames[i];
    field_def := FRefObj.DataSet.FieldDefs.Find(field_name);
    with RefObjTreeListView.Columns.Add do
    begin
      Text := GetFieldLabel(field_def);
      Width := GetColumnWidth(field_def);
    end;
  end;
end;

procedure TChoiceRefObjForm.InitSearch(ASearchFields: TArrayOfString);
var
  field_names: TArrayOfString;
  field_name: String;
  field_def:  TFieldDef;
  column_label: AnsiString;
  i: Integer;
begin
  if FRefObj <> nil then
      field_names := [FRefObj.CodColumnName, FRefObj.NameColumnName]
  else
    field_names := ['cod', 'name'];

  if Length(ASearchFields) > 0 then
    FRefObjSearchColNames := strfunc.JoinArrayOfString(field_names, ASearchFields)
  else
    FRefObjSearchColNames := field_names;

  if FRefObj = nil then
  begin
    logfunc.WarningMsg('Не определен справочник для выбора кода');
    Exit;
  end;
  if FRefObj.DataSet = nil then
  begin
    logfunc.WarningMsgFmt('Не определен объект набора данных для <%s>', [FRefObj.Name]);
    Exit;
  end;

  //SearchFieldComboBox.Items.Clear;
  //for i := 0 to Length(FRefObjSearchColNames) - 1 do
  //begin
  //  field_name := FRefObjColNames[i];
  //  field_def := FRefObj.DataSet.FieldDefs.Find(field_name);
  //  column_label := GetFieldLabel(field_def);
  //  SearchFieldComboBox.Items.Add(column_label);
  //end;
end;

function TChoiceRefObjForm.GetFieldLabel(AFieldDef: TFieldDef): AnsiString;
begin
  Result := '';
  if AFieldDef <> nil then
    if AFieldDef.DisplayName = 'cod' then
      Result := 'Код'
    else if AFieldDef.DisplayName = 'name' then
      Result := 'Наименование'
    else
      Result := AFieldDef.DisplayName;
end;

function TChoiceRefObjForm.GetColumnWidth(AFieldDef: TFieldDef): Integer;
begin
  Result := 200;
  if AFieldDef <> nil then
    if AFieldDef.DisplayName = 'cod' then
      Result := 100
    else if AFieldDef.DisplayName = 'name' then
      Result := 500
end;

procedure TChoiceRefObjForm.SetRefObjTree(ASortColumn: String);
var
  title: AnsiString;
begin
  if (FRefObj <> nil) and (not strfunc.IsEmptyStr(FRefObj.Description)) then
    title := FRefObj.Description
  else
    title := FRefObj.Name;

  RefObjTreeListView.BeginUpdate;
  { Добавляем корневой элемент дерева }
  RefObjTreeListView.Items.Add.Text := title;

  if (FRefObj = nil) or FRefObj.IsEmpty() then
    logfunc.WarningMsgFmt('Справочник <%s> пустой', [FRefObj.Name], True)
  else
  begin
    if strfunc.IsEmptyStr(ASortColumn) then
      ASortColumn := FSortColumn;
    SetRefObjLevelTree(RefObjTreeListView.Items[0], '', ASortColumn);
	end;

  RefObjTreeListView.EndUpdate;

  { Распахнуть корневой элемент }
end;

procedure TChoiceRefObjForm.SetRefObjLevelTree(AParentItem: TTreeListItem; ACod: String; ASortColumn: String);
var
  level_data: TDataSet;
  i: Integer;
begin
  if FRefObj = nil then
  begin
    logfunc.WarningMsg('Не определен справочник для выбора кода', True);
    Exit;
  end;

  level_data := FRefObj.GetLevelRecsByCod(ACod, nil);
  { Сортировка, если определены колонки сортировки }
  if not strfunc.IsEmptyStr(ASortColumn) then
      level_data := SortRefObjRecordset(level_data, ASortColumn);
  if level_data.IsEmpty() then
  begin
    logfunc.WarningMsg('Нет данных', True);
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
    FreeAndNil(level_data);
    { Установить автоматически ширины колонок }
  except
    logfunc.FatalMsgFmt('Ошибка построения дерева справочника <%s>', [FRefObj.Name]);
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

  if not strfunc.IsEmptyStr(sort_field) then
    Result := ARecordSet
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
    logfunc.WarningMsgFmt('Сортировка. Поле <%s> не найдено в [%s]', [Result, strfunc.JoinStr(FRefObjColNames, ', ')]);
    Result := '';
	end;
end;

function TChoiceRefObjForm.GetSortField(ASortColumn: Integer): String;
begin
  // An increase of 1 is made in order to take
  // into account the first column with index 0
  //                                      V
  Result := FRefObjColNames[ASortColumn - 1];

  if not strfunc.IsStrInList(Result, FRefObjColNames) then
  begin
    logfunc.WarningMsgFmt('Сортировка. Поле <%s> не найдено в [%s]', [Result, strfunc.JoinStr(FRefObjColNames, ', ')]);
    Result := '';
	end;
end;

//function TChoiceRefObjForm.GetSortFieldIdx(ASortColumn: String): Integer;
//begin
//  if strfunc.IsStartsWith(ASortColumn, SORT_REVERSE_SIGN) then
//  	Result := Copy(ASortColumn, 1, Length(ASortColumn) - 1)
//  else
//    Result := ASortColumn;
//
//  Result := strfunc.GetIdxStrInList(ASortColumn, FRefObjColNames);
//end;

function TChoiceRefObjForm.IsReverseSort(ASortColumn: String): Boolean;
begin
  Result := strfunc.IsStartsWith(ASortColumn, SORT_REVERSE_SIGN);
end;

function TChoiceRefObjForm.IsReverseSort(ASortColumn: Integer): Boolean;
begin
  Result := ASortColumn < 0;
end;

procedure TChoiceRefObjForm.RefreshRefObjTree(ASortColumn: String);
var
  selected_code, title: String;
begin
  selected_code := GetSelectedCode(FRefObj.DataSet);
  { Сначала удаляем все элементы дерева }
  RefObjTreeListView.Items.Clear;

  if (FRefObj <> nil) and (not strfunc.IsEmptyStr(FRefObj.Description)) then
    title := FRefObj.Description
  else
    title := FRefObj.Name;

  RefObjTreeListView.BeginUpdate;
  { Добавляем корневой элемент дерева }
  RefObjTreeListView.Items.Add.Text := title;

  if (FRefObj <> nil) or FRefObj.IsEmpty() then
    logfunc.WarningMsgFmt('Справочник <%s> пустой', [FRefObj.Name])
  else
  begin
    if strfunc.IsEmptyStr(ASortColumn) then
      ASortColumn := FSortColumn;
    SetRefObjLevelTree(RefObjTreeListView.Items[0], '', ASortColumn);
	end;

  RefObjTreeListView.EndUpdate;

  if strfunc.IsEmptyStr(selected_code) then
    SelectRefObjTreeItem(RefObjTreeListView.Items[0], selected_code, FRefObj.DataSet)
  else
    { Распахнуть корневой элемент }
    RefObjTreeListView.Items[0].Expand();

end;

procedure TChoiceRefObjForm.SetRefObjTreeItem(AParentItem: TTreeListItem; ARecordSet: TDataSet);
var
  code, field_name, value: String;
  is_activate: Boolean;
  i: Integer;
  item: TTreeListItem;
begin
  code := ARecordSet.FieldByName(FRefObj.CodColumnName).AsString;
  is_activate := ARecordSet.FieldByName(FRefObj.ActiveColumnName).AsBoolean;

  if is_activate then
  begin
    item := AParentItem.SubItems.Add(code);
    { Здесь надо привязать запись к элементу справочника}
    item.Data.i64 := ARecordSet.FieldByName(FRefObj.IdColumnName).AsInteger;
    { Заполнение колонок }
    for i := 1 to Length(FRefObjColNames) - 1 do
    begin
      field_name := FRefObjColNames[i];
      value := ARecordSet.FieldByName(field_name).AsString;
      item.RecordItems.Add.Text := value;
    end;
    if FRefObj.HasChildrenCodes(code) then
    begin
      SetRefObjLevelTree(item, code, FSortColumn);
    end;
  end;
end;

function TChoiceRefObjForm.FindTreeChildItem(AItemText: AnsiString; ACurItem: TTreeListItem): TTreeListItem;
var
  i: Integer;
  child_item: TTreeListItem;
begin
  if ACurItem = nil then
    ACurItem := RefObjTreeListView.Items[0];

  Result := nil;
	for i := 0 to ACurItem.SubItems.Count - 1 do
  begin
  	child_item := ACurItem.SubItems[i];
    if child_item.Text = AItemText then
    begin
      Result := child_item;
      break;
    end;
  end;
end;

procedure TChoiceRefObjForm.InitLevelTree(AItem: TTreeListItem; ARecordSet: TDataSet);
var
  find_item: TTreeListItem;
  code: String;
begin
  find_item := FindTreeChildItem(TREE_ITEM_LABEL, AItem);

  //if find_item <> nil then
  //  RefObjTreeListView.RemoveSelection();

  if AItem.SubItems.Count = 0 then
  begin
    { Получить запись }
    // ARecordSet.MoveBy(AItem.Data.i64);
    ARecordSet.Locate(FRefObj.IdColumnName, AItem.Data.i64, []);
    { Получить код }
    code := ARecordSet.FieldByName(FRefObj.CodColumnName).AsString;
    SetRefObjLevelTree(AItem, code, '');
  end;
end;

function TChoiceRefObjForm.FindRefObjTreeItem(AParentItem: TTreeListItem; ACode: String; ARecordSet: TDataSet): TTreeListItem;
var
  find_result, child_item: TTreeListItem;
  i: Integer;
begin
  { Поиск в текущем элементе }
  { Получить запись }
  ARecordSet.Locate(FRefObj.IdColumnName, AParentItem.Data.i64, []);
  if ARecordSet.FieldByName(FRefObj.CodColumnName).AsString = ACode then
  begin
    Result := AParentItem;
    Exit;
  end;

  { Поиск в дочерних элементах }
  find_result := nil;
  for i := 0 to AParentItem.SubItems.Count - 1 do
  begin
    child_item := AParentItem.SubItems[i];
    //ARecordSet.MoveBy(child_item.Data.i64);
    ARecordSet.Locate(FRefObj.IdColumnName, child_item.Data.i64, []);
    if ARecordSet.FieldByName(FRefObj.CodColumnName).AsString = ACode then
    begin
      find_result := child_item;
      break;
    end;
  end;

  { На этом уровне не нашли, необходимо искать уровнями ниже }
  if find_result = nil then
  begin
    for i := 0 to AParentItem.SubItems.Count - 1 do
    begin
      child_item := AParentItem.SubItems[i];
      InitLevelTree(child_item, ARecordSet);
      find_result := FindRefObjTreeItem(child_item, ACode, ARecordSet);
      if find_result <> nil then
        break;
    end;
  end;

  if find_result <> nil then
    find_result.Expand;

  Result := find_result;
end;

function TChoiceRefObjForm.SelectRefObjTreeItem(AParentItem: TTreeListItem; ACode: String; ARecordSet: TDataSet): TTreeListItem;
begin
  if AParentItem = nil then
    AParentItem := RefObjTreeListView.Items[0];

  Result := FindRefObjTreeItem(AParentItem, ACode, ARecordSet);
  if Result <> nil then
  begin
    RefObjTreeListView.Selected := Result;
    Exit;
  end;
  Result := nil;
end;

function TChoiceRefObjForm.GetSelectedCode(ARecordSet: TDataSet): String;
var
  item: TTreeListItem;
begin
  item := RefObjTreeListView.Selected;
  if item <> nil then
  begin
    { Получить запись }
    //ARecordSet.MoveBy(item.Data.i64);
    ARecordSet.Locate(FRefObj.IdColumnName, item.Data.i64, []);
    Result := ARecordSet.FieldByName(FRefObj.CodColumnName).AsString;
    Exit;
  end;
  Result := '';
end;

function TChoiceRefObjForm.EditRefObj(): Boolean;
begin
  Result := True;
end;

procedure TChoiceRefObjForm.ClearSearch();
begin
  SetLength(FSearchCodes, 0);
  FSearchCodeIdx := 0;
  FNotActualSearch := False;

  RefObjTreeListView.Selected := RefObjTreeListView.Items[0];
  // FindEdit.Text := '';
end;

function TChoiceRefObjForm.GetSearchFieldName(): String;
//var
//  idx: Integer;
begin
  //idx := SearchFieldComboBox.ItemIndex;
  //if (idx >= 0) and (idx < SearchFieldComboBox.Items.Count) then
  //  Result := FRefObjSearchColNames[idx]
  //else
    Result := FRefObj.NameColumnName;
end;

function TChoiceRefObjForm.GetSearchCodes(ASearchTxt: String; ASearchFieldName: String; ARecordSet: TDataSet): TArrayOfString;
var
  is_reverse: Boolean;
  order_by: TArrayOfString;
  sort_field, fld: String;
  i: Integer;
  search_codes_list: TStringList;
begin
  if strfunc.IsEmptyStr(ASearchFieldName) then
    ASearchFieldName := GetSearchFieldName();

  is_reverse := False;
  if not strfunc.IsEmptyStr(FSortColumn) then
  begin
    sort_field := GetSortField(FSortColumn);
    is_reverse := IsReverseSort(FSortColumn);
    SetLength(order_by, Length(FRefObjColNames) + 1);
    order_by[0] := sort_field;
    for i := 1 to Length(FRefObjColNames) do
    begin
      fld := FRefObjColNames[i - 1];
      if (fld <> FRefObj.CodColumnName) and (fld <> sort_field) then
        order_by[i] := fld
      else
        order_by[i] := '';
    end;
  end;

  try
    search_codes_list := FRefObj.SearchCodesByColValue(ASearchTxt, ASearchFieldName);
    Result := strfunc.StringListToArrayOfString(search_codes_list);
                                            //order_by, is_reverse);
  	search_codes_list.Destroy();
  except
    //logfunc.Fatal
    Result := [];
  end;

  if Length(Result) > 0 then
  begin
    FSearchCodes := Result;
    //FSearchCideIdx := 0;
  end;
end;

function ChoiceRefObjCodDialogForm(ARefObj: TICRefObjDataSource; ADefaultCod: String; AFields: TArrayOfString): String;
var
  dlg: TChoiceRefObjForm;
begin
  dlg := TChoiceRefObjForm.Create(Application);
  try
    dlg.SetRefObj(ARefObj);
    dlg.Init(AFields);
    dlg.ShowModal();
  finally
    dlg.Free;
  end;
end;

end.

