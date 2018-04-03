{
Модуль компонента работы с набором записей файла с разделителем

Версия: 0.0.0.2
}
unit ICSdfDataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, sdfdata;

const
  { Расширение имени файла блокировки }
  LOCK_FILENAME_EXT = '.lck';

type
  { Компонент работы с набором записей файла с разделителем }
  TICSdfDataset = class(TSdfDataSet)
  private

    //{ Имя файла данных}
    //FFileName: TFileName;
    //
    //{ Переопределенная процедура установки имени файла данных }
    //procedure SetFileName(Value: TFileName); // override;

    {
    Функция определения блокировки файла данных
    @return True - Файл данных заблокирован.
            False - Файл данных НЕ заблокирован.
            Файл данных считается заблокированным, если рядом с ним находится
            файл с такми же именем, но с расширением *.lck.
    }
    function GetIsLocked(): Boolean;

  protected

  public
    constructor Create(AOwner: TComponent); override;

    { Обновление набора записей из файла }
    procedure UpdateDataset;

    {
    Функция ожидания разблокировки файла данных.
    @param iTimeOut: Дополнительный таймаут (в секундах) для возможности продолжить
                     работу программы в случаях не снятия блокировки
    @return True - Блокировка снята.
            False - Сработал таймаут. Блокировка не снята.
    }
    function WaitUnLock(iTimeOut: Integer=1): Boolean;

    { Признак заблокированного файла данных. Свойство }
    property IsLocked: Boolean read GetIsLocked;

  published

    //{ Переопределяем свойство для автоматического заполнения полей в режиме дизайнера }
    //property FileName : TFileName read FFileName write SetFileName;

  end;

procedure Register;

implementation

uses
  logfunc;

procedure Register;
begin
  {$I icsdfdataset_icon.lrs}
  RegisterComponents('IC Tools',[TICSdfDataset]);
end;

constructor TICSdfDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Delimiter := ';';
  FirstLineAsSchema := True;
  //MultiLine := True;
end;

{ Обновление набора записей из файла }
procedure TICSdfDataset.UpdateDataset;
var
  wait_result: Boolean;
begin
  wait_result := WaitUnLock();
  // Если блокировка успешно снята, то можно произвести обновление
  if wait_result then
  begin
    Close;
    Open;
    // Актуальной считается последняя запись. Поэтому переходм на нее
    Last;
  end;
end;

{ Функция определения блокировки файла данных }
function TICSdfDataset.GetIsLocked(): Boolean;
var
  lck_filename: AnsiString;
begin
  if FileName = '' then
  begin
    logfunc.WarningMsgFmt('Не определен файл данных в объекте <%s : %s>', [Name, ClassName]);
    Result := False;
    Exit;
  end;

  if not FileExists(FileName) then
  begin
    logfunc.WarningMsgFmt('Не найден файл данных объекта <%s : %s>', [Name, ClassName]);
    Result := False;
    Exit;
  end;

  lck_filename := ChangeFileExt(FileName, LOCK_FILENAME_EXT);
  Result := FileExists(lck_filename);
end;

{ Функция ожидания разблокировки файла данных. }
function TICSdfDataset.WaitUnLock(iTimeOut: Integer): Boolean;
var
  timeout: TDateTime;
begin
  // Если файл данных не заблокирован, то просто выйти
  if not IsLocked then
  begin
    Result := True;
    Exit;
  end;

  timeout := Now + iTimeOut;

  Result := True;
  while IsLocked do
    if Now >= timeout then
    begin
      Result := False;
      Break;
    end;

end;

//{ Переопределенная процедура установки имени файла данных }
//procedure TICSdfDataset.SetFileName(Value: TFileName);
//var
//  txt_file: Text;
//begin
//  if csDesigning in ComponentState then
//  begin
//    //... код, работающий только в дизайне ...
//    if FileExists(Value) and FirstLineAsSchema then
//    begin
//      // Загрузить схему по первой линии файла
//      Assign(txt_file, Value);
//      //rewrite(f);
//      ReadLn(txt_file, b, k1, k2);
//      for j:=1 to 10 do
//      begin
//      writeln(a,j:4,j*2:4);
//
//      (f,a,j:4,j*2:4);
//      end;
//
//      close(txt_file);
//    end;
//  end;
//
//  // Блок из функции родительского класса
//  CheckInactive;
//  FFileName := Value;
//end;

end.
