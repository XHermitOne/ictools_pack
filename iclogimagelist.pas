{
Модуль списка иконок типов сообщений журнала.
}
unit ICLogImageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

const
  INFO_IMG_INDEX = 0;
  DEBUG_IMG_INDEX = 1;
  WARNING_IMG_INDEX = 2;
  ERROR_IMG_INDEX = 3;
  FATAL_IMG_INDEX = 4;
  SERVICE_IMG_INDEX = 5;

type
  { Список иконок типов сообщений журнала }
  TICLogImageList = class(TImageList)
  private
    FBmp0, FBmp1, FBmp2, FBmp3, FBmp4, FBmp5 : TBitmap;

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I iclogimagelist_icon.lrs}
  RegisterComponents('IC Tools',[TICLogImageList]);
end;

constructor TICLogImageList.Create(AOwner: TComponent);
begin
  inherited;

  FBmp0 := LoadBitmapFromLazarusResource('information');
  Add(FBmp0, nil);

  FBmp1 := LoadBitmapFromLazarusResource('bug');
  Add(FBmp1, nil);

  FBmp2 := LoadBitmapFromLazarusResource('error');
  Add(FBmp2, nil);

  FBmp3 := LoadBitmapFromLazarusResource('exclamation');
  Add(FBmp3, nil);

  FBmp4 := LoadBitmapFromLazarusResource('stop');
  Add(FBmp4, nil);

  FBmp5 := LoadBitmapFromLazarusResource('comment');
  Add(FBmp5, nil);
end;

destructor TICLogImageList.Destroy;
begin
  FBmp0.Free;
  FBmp1.Free;
  FBmp2.Free;
  FBmp3.Free;
  FBmp4.Free;
  FBmp5.Free;
  inherited;
end;

initialization
  { Подключение ресурса с картинками }
  {$I log_icon_resource.lrs}

end.
