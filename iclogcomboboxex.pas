{
Модуль компонента комбобокса журнала сообщений.
}
unit ICLogComboBoxEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComboEx;

const
  UNASSIGNED_DATETIME = -693594;

type
  {
  Журнал сообщений в виде комбобокса.
  }
  TICLogComboBoxEx = class(TComboBoxEx)
  private
    { Максимально допустимое количество сообщений в комбобоксе. Если -1, то ограничений нет}
    FMaxItemCount: Integer;

    { Признак отображения даты-времени }
    FShowDateTime: Boolean;

    { Установить максимально допустимое количество сообщений в комбобоксе. Если -1, то ограничений нет}
    procedure SetMaxItemCount(Value: Integer=-1);

  protected

    {
    Добавить строку элемента в комбобокс
    @param sMessage Сообщение
    @param dtValue Дата-время регистрации
    @param iImgIndex Индекс иконки
    @param bAutoSelect Автоматически выбрать вновь добавленный элемент?
    }
    procedure AddItemMsg(sMessage: AnsiString; dtValue: TDateTime=UNASSIGNED_DATETIME; iImgIndex: Integer=-1; bAutoSelect: Boolean=True);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Добавить информационное сообщение }
    procedure AddInfoMsg(sMessage: AnsiString; dtValue: TDateTime=UNASSIGNED_DATETIME);
    procedure AddInfoMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime=UNASSIGNED_DATETIME);
    { Добавить отладочное сообщение }
    procedure AddDebugMsg(sMessage: AnsiString; dtValue: TDateTime=UNASSIGNED_DATETIME);
    procedure AddDebugMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime=UNASSIGNED_DATETIME);
    { Добавить сообщение о предупреждении }
    procedure AddWarningMsg(sMessage: AnsiString; dtValue: TDateTime=UNASSIGNED_DATETIME);
    procedure AddWarningMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime=UNASSIGNED_DATETIME);
    { Добавить сообщение об ошибке }
    procedure AddErrorMsg(sMessage: AnsiString; dtValue: TDateTime=UNASSIGNED_DATETIME);
    procedure AddErrorMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime=UNASSIGNED_DATETIME);
    { Добавить сообщение об критической ошибке }
    procedure AddFatalMsg(sMessage: AnsiString; dtValue: TDateTime=UNASSIGNED_DATETIME);
    procedure AddFatalMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime=UNASSIGNED_DATETIME);
    { Добавить сервисное сообщение }
    procedure AddServiceMsg(sMessage: AnsiString; dtValue: TDateTime=UNASSIGNED_DATETIME);
    procedure AddServiceMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime=UNASSIGNED_DATETIME);

    {
    Удалить строку элемента из комбобокса
    @param iItemIndex Индекс удаляемого элемента
    }
    procedure DelItemMsg(iItemIndex: Integer);
    { Удалить текущий выбранный элемент из комбобокса }
    procedure DelSelectedItemMsg();
    { Удалить не актуальные элементы из комбобокса. Не актуальные считыются первые элементы если количество превышает MaxItemCount }
    procedure DelOverItems();

  published
    { Максимально допустимое количество сообщений в комбобоксе. Свойство. Если -1, то ограничений нет}
    property MaxItemCount: Integer read FMaxItemCount write SetMaxItemCount;

    { Признак отображения даты-времени }
    property ShowDateTime: Boolean read FShowDateTime write FShowDateTime;
  end;

procedure Register;

implementation

uses
  ICLogImageList;

procedure Register;
begin
  {$I iclogcomboboxex_icon.lrs}
  RegisterComponents('IC Tools',[TICLogComboBoxEx]);
end;

constructor TICLogComboBoxEx.Create(AOwner: TComponent);
begin
  inherited;

  FMaxItemCount := -1;
  FShowDateTime := True;
end;

destructor TICLogComboBoxEx.Destroy;
begin
  inherited;
end;

procedure TICLogComboBoxEx.SetMaxItemCount(Value: Integer);
begin
  FMaxItemCount := Value;
  if FMaxItemCount > 0 then
    // Удаляем лишние элементы
    DelOverItems;
end;

{ Добавить строку элемента в комбобокс }
procedure TICLogComboBoxEx.AddItemMsg(sMessage: AnsiString; dtValue: TDateTime; iImgIndex: Integer; bAutoSelect: Boolean);
var
  item: TComboExItem;
begin
  if (dtValue = UNASSIGNED_DATETIME) and ShowDateTime then
    dtValue := Now;

  if Images = nil then
    iImgIndex := -1;

  if ShowDateTime then
    item := ItemsEx.AddItem(Format('%s %s', [FormatDateTime('YYYY-MM-DD hh:mm:ss', dtValue), sMessage]), iImgIndex)
  else
    item := ItemsEx.AddItem(sMessage, iImgIndex);

  if bAutoSelect then
    ItemIndex := item.Index;
end;

{ Добавить информационное сообщение }
procedure TICLogComboBoxEx.AddInfoMsg(sMessage: AnsiString; dtValue: TDateTime);
begin
  AddItemMsg(sMessage, dtValue, ICLogImageList.INFO_IMG_INDEX);
end;

procedure TICLogComboBoxEx.AddInfoMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime);
begin
  AddInfoMsg(Format(sMsgFmt, aArgs), dtValue);
end;

{ Добавить отладочное сообщение }
procedure TICLogComboBoxEx.AddDebugMsg(sMessage: AnsiString; dtValue: TDateTime);
begin
  AddItemMsg(sMessage, dtValue, ICLogImageList.DEBUG_IMG_INDEX);
end;

procedure TICLogComboBoxEx.AddDebugMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime);
begin
  AddDebugMsg(Format(sMsgFmt, aArgs), dtValue);
end;

{ Добавить сообщение о предупреждении }
procedure TICLogComboBoxEx.AddWarningMsg(sMessage: AnsiString; dtValue: TDateTime);
begin
  AddItemMsg(sMessage, dtValue, ICLogImageList.WARNING_IMG_INDEX);
end;

procedure TICLogComboBoxEx.AddWarningMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime);
begin
  AddWarningMsg(Format(sMsgFmt, aArgs), dtValue);
end;

{ Добавить сообщение об ошибке }
procedure TICLogComboBoxEx.AddErrorMsg(sMessage: AnsiString; dtValue: TDateTime);
begin
  AddItemMsg(sMessage, dtValue, ICLogImageList.ERROR_IMG_INDEX);
end;

procedure TICLogComboBoxEx.AddErrorMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime);
begin
  AddErrorMsg(Format(sMsgFmt, aArgs), dtValue);
end;

{ Добавить сообщение об критической ошибке }
procedure TICLogComboBoxEx.AddFatalMsg(sMessage: AnsiString; dtValue: TDateTime);
begin
  AddItemMsg(sMessage, dtValue, ICLogImageList.FATAL_IMG_INDEX);
end;

procedure TICLogComboBoxEx.AddFatalMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime);
begin
  AddFatalMsg(Format(sMsgFmt, aArgs), dtValue);
end;

{ Добавить сервисное сообщение }
procedure TICLogComboBoxEx.AddServiceMsg(sMessage: AnsiString; dtValue: TDateTime);
begin
  AddItemMsg(sMessage, dtValue, ICLogImageList.SERVICE_IMG_INDEX);
end;

procedure TICLogComboBoxEx.AddServiceMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; dtValue: TDateTime);
begin
  AddServiceMsg(Format(sMsgFmt, aArgs), dtValue);
end;

{
Удалить строку элемента из комбобокса
@param iItemIndex Индекс удаляемого элемента
}
procedure TICLogComboBoxEx.DelItemMsg(iItemIndex: Integer);
begin
  // Выбрать предыдущий элемент
  if iItemIndex = ItemIndex then
    if ItemIndex > 0 then
      ItemIndex := ItemIndex - 1;

  Delete(iItemIndex);
end;

{ Удалить текущий выбранный элемент из комбобокса }
procedure TICLogComboBoxEx.DelSelectedItemMsg();
var
  i_del: Integer;
begin
  i_del := ItemIndex;
  // Выбрать предыдущий элемент
  if ItemIndex > 0 then
    ItemIndex := ItemIndex - 1;

  Delete(i_del);
end;

{ Удалить не актуальные элементы из комбобокса. Не актуальные считыются первые элементы если количество превышает MaxItemCount }
procedure TICLogComboBoxEx.DelOverItems();
var
  i, i_del, item_count: Integer;
begin
  item_count := Items.Count;
  for i := 0 to item_count - 1 do
  begin
    i_del := item_count - 1 - i - MaxItemCount;
    if i_del >= 0 then
      Delete(i_del)
    else
      Break;
  end;
end;

end.
