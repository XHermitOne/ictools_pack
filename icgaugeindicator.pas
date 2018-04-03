{
Модуль компонента простого стрелочного индикатора c 3-мя зонами раскраски.
}
unit ICGaugeIndicator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  { Стиль отображения }
  TStyle = (LeftStyle, RightStyle, CenterStyle);
  { Опции отображения }
  TFaceOption = (ShowMargin, ShowCircles, ShowMainTicks, ShowSubTicks,
                 ShowIndicatorMin, ShowIndicatorMid, ShowIndicatorMax,
                 ShowValues, ShowCenter, ShowFrame, Show3D, ShowCaption);
  TFaceOptions = set of TFaceOption;
  { }
  TAntialiased = (aaNone, aaBiline, aaTriline, aaQuadral);

  //{ Данные индикатора }
  //PGaugeData = ^TGaugeData;
  //TGaugeData = object //(TObject)
  //public
  //  // Цвета элементов
  //  { Цвет зоны минимума }
  //  FMinColor: TColor;
  //  { Цвет средней зоны }
  //  FMidColor: TColor;
  //  { Цвет зоны максимума }
  //  FMaxColor: TColor;
  //  { Основной цвет контрола }
  //  FFaceColor: TColor;
  //  { Цвет шкалы }
  //  FTicksColor: TColor;
  //  { Цвет значений шкалы }
  //  FValueColor: TColor;
  //  { Цвет надписи }
  //  FCaptionColor: TColor;
  //  { Цвет стрелки }
  //  FArrowColor: TColor;
  //  FMarginColor: TColor;
  //  FCenterColor: TColor;
  //  FCircleColor: TColor;
  //
  //  // Размеры элементов
  //  FCenterRadius: Integer;
  //  FCircleRadius: Integer;
  //  FScaleAngle: Integer;
  //  FMargin: Integer;
  //  FStyle: TStyle;
  //  FArrowWidth: Integer;
  //  FNumMainTicks: Integer;
  //  FLengthMainTicks: Integer;
  //  FLengthSubTicks: Integer;
  //  FFaceOptions: TFaceOptions;
  //  // Значения
  //  FPosition: Single;
  //  FScaleValue: Integer;
  //  FMinimum: Integer;
  //  FMaximum: Integer;
  //  FCaption: string;
  //  // event handlers
  //  FOverMax: TNotifyEvent;
  //  FOverMin: TNotifyEvent;
  //  // anti-aliasing mode
  //  FAntiAliased: TAntialiased;
  //  // internal bitmaps
  //  FBackBitmap: TBitmap;
  //  FFaceBitmap: TBitmap;
  //  FAABitmap: TBitmap;
  //  // set properties
  //  {ecm}
  //  FLockRedraw: Integer;
  //  {/ecm}
  //protected
  //  procedure Init; virtual;
  //  destructor Destroy; virtual;
  //end;


  { Компонента простого стрелочного индикатора c 3-мя зонами раскраски }
  PGaugeIndicator = ^TICGaugeIndicator;
  TICGaugeIndicator = class(TGraphicControl)
  private
    // Цвета элементов
    { Цвет зоны минимума }
    FMinColor: TColor;
    { Цвет средней зоны }
    FMidColor: TColor;
    { Цвет зоны максимума }
    FMaxColor: TColor;
    { Основной цвет контрола }
    FFaceColor: TColor;
    { Цвет шкалы }
    FTicksColor: TColor;
    { Цвет значений шкалы }
    FValueColor: TColor;
    { Цвет надписи }
    FCaptionColor: TColor;
    { Цвет стрелки }
    FArrowColor: TColor;
    FMarginColor: TColor;
    FCenterColor: TColor;
    FCircleColor: TColor;

    // Размеры элементов
    FCenterRadius: Integer;
    FCircleRadius: Integer;
    FScaleAngle: Integer;
    FMargin: Integer;
    FStyle: TStyle;
    FArrowWidth: Integer;
    FNumMainTicks: Integer;
    FLengthMainTicks: Integer;
    FLengthSubTicks: Integer;
    FFaceOptions: TFaceOptions;
    // Значения
    FPosition: Single;
    FScaleValue: Integer;
    FMinimum: Integer;
    FMaximum: Integer;
    FCaption: string;
    // event handlers
    FOverMax: TNotifyEvent;
    FOverMin: TNotifyEvent;
    // anti-aliasing mode
    FAntiAliased: TAntialiased;
    // internal bitmaps
    FBackBitmap: TBitmap;
    FFaceBitmap: TBitmap;
    FAABitmap: TBitmap;
    // set properties
    {ecm}
    FLockRedraw: Integer;
    {/ecm}

    procedure SetFMinColor(C: TColor);
    procedure SetFMidColor(C: TColor);
    procedure SetFMaxColor(C: TColor);
    procedure SetFaceColor(C: TColor);
    procedure SetFTicksColor(C: TColor);
    procedure SetFValueColor(C: TColor);
    procedure SetFCaptionColor(C: TColor);
    procedure SetFArrowColor(C: TColor);
    procedure SetFMarginColor(C: TColor);
    procedure SetFCenterColor(C: TColor);
    procedure SetFCircleColor(C: TColor);
    procedure SetFCenterRadius(I: Integer);
    procedure SetFCircleRadius(I: Integer);
    procedure SetFScaleAngle(I: Integer);
    procedure SetFMargin(I: Integer);
    procedure SetFStyle(S: TStyle);
    procedure SetFArrowWidth(I: Integer);
    procedure SetFNumMainTicks(I: Integer);
    procedure SetFLengthMainTicks(I: Integer);
    procedure SetFLengthSubTicks(I: Integer);
    procedure SetFFaceOptions(O: TFaceOptions);
    procedure SetFPosition(V: Single);
    procedure SetFScaleValue(I: Integer);
    procedure SetFMaximum(I: Integer);
    procedure SetFMinimum(I: Integer);
    procedure SetFCaption(const S: string);
    procedure SetFAntiAliased(V: TAntialiased);
    function GetAAMultipler: Integer;
    function GetAntiAliased: TAntialiased;
    function GetArrowColor: TColor;
    function GetArrowWidth: Integer;
    function GetCaptionColor: TColor;
    function GetCenterColor: TColor;
    function GetCenterRadius: Integer;
    function GetCircleColor: TColor;
    function GetCircleRadius: Integer;
    function GetFaceColor: TColor;
    function GetFaceOptions: TFaceOptions;
    function GetFCaption: string;
    function GetLengthMainTicks: Integer;
    function GetLengthSubTicks: Integer;
    function GetMargin: Integer;
    function GetMarginColor: TColor;
    function GetMaxColor: TColor;
    function GetMaximum: Integer;
    function GetMidColor: TColor;
    function GetMinColor: TColor;
    function GetMinimum: Integer;
    function GetNumMainTicks: Integer;
    function GetOverMax: TNotifyEvent;
    function GetOverMin: TNotifyEvent;
    function GetPosition: Single;
    function GetScaleAngle: Integer;
    function GetScaleValue: Integer;
    function GetStyle: TStyle;
    function GetTicksColor: TColor;
    function GetValueColor: TColor;
    procedure SetOverMax(const Value: TNotifyEvent);
    procedure SetOverMin(const Value: TNotifyEvent);

  protected
    procedure DrawScale(Bitmap: TBitmap; K: Integer);
    procedure DrawArrow(Bitmap: TBitmap; K: Integer);
    procedure RedrawScale;
    procedure RedrawArrow;
    //procedure FastAntiAliasPicture;
    procedure PaintGauge;
    procedure Reinitialize;

  public
    procedure Init; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override; //virtual;

    {ecm}
    procedure LockRedraw;
    procedure UnLockRedraw;
    {/ecm}

  published
    property MinColor: TColor read GetMinColor write SetFMinColor;
    property MidColor: TColor read GetMidColor write SetFMidColor;
    property MaxColor: TColor read GetMaxColor write SetFMaxColor;
    property FaceColor: TColor read GetFaceColor write SetFaceColor;
    property TicksColor: TColor read GetTicksColor write SetFTicksColor;
    property ValueColor: TColor read GetValueColor write SetFValueColor;
    property CaptionColor: TColor read GetCaptionColor write SetFCaptionColor;
    property ArrowColor: TColor read GetArrowColor write SetFArrowColor;
    property MarginColor: TColor read GetMarginColor write SetFMarginColor;
    property CenterColor: TColor read GetCenterColor write SetFCenterColor;
    property CircleColor: TColor read GetCircleColor write SetFCircleColor;
    property CenterRadius: Integer read GetCenterRadius write SetFCenterRadius;
    property CircleRadius: Integer read GetCircleRadius write SetFCircleRadius;
    property Angle: Integer read GetScaleAngle write SetFScaleAngle;
    property GaugeMargin: Integer read GetMargin write SetFMargin;
    property GaugeStyle: TStyle read GetStyle write SetFStyle;
    property ArrowWidth: Integer read GetArrowWidth write SetFArrowWidth;
    property NumberMainTicks: Integer read GetNumMainTicks write SetFNumMainTicks;
    property LengthMainTicks: Integer read GetLengthMainTicks write SetFLengthMainTicks;
    property LengthSubTicks: Integer read GetLengthSubTicks write SetFLengthSubTicks;
    property FaceOptions: TFaceOptions read GetFaceOptions write SetFFaceOptions;
    property GaugePosition: Single read GetPosition write SetFPosition;
    property Scale: Integer read GetScaleValue write SetFScaleValue;
    property IndMaximum: Integer read GetMaximum write SetFMaximum;
    property IndMinimum: Integer read GetMinimum write SetFMinimum;
    property GaugeCaption: string read GetFCaption write SetFCaption;
    property AntiAliased: TAntialiased read GetAntiAliased write SetFAntiAliased;
    property OnOverMax: TNotifyEvent read GetOverMax write SetOverMax;
    property OnOverMin: TNotifyEvent read GetOverMin write SetOverMin;

  end;

  //function CreateGaugeIndicator(AOwner: TControl): PGaugeIndicator;

procedure Register;

implementation

uses
  Math,
  LCLType;
  //IntfGraphics,  // TLazIntfImage type
  //fpImage;

procedure Register;
begin
  {$I icgaugeindicator_icon.lrs}
  RegisterComponents('IC Tools',[TICGaugeIndicator]);
end;


//function CreateGaugeIndicator(AOwner: TControl): PGaugeIndicator;
//var
//  Data: PGaugeData;
//begin
//  Result := PGaugeIndicator(NewPaintbox(AOwner).setsize(120,80));
//  New(Data, Create);
//  Result.CustomObj := Data;
//  Result.Font.FontHeight := 8;
//  Result.Font.FontName := 'Verdana';
//  Result.Reinitialize;
//  //Result.AttachProc(GaugeHandler);
//end;

procedure TICGaugeIndicator.DrawScale(Bitmap: TBitmap; K: Integer);
var
  I, J, X, Y, N, M, W, H{, R}: Integer;
  Max, Min: Int64;
  A, C: Single;
  SI, CO, SI1, CO1: Extended;
begin
  //with PGaugeData(CustomObj)^ do
  //with self do
  //begin
    W := Bitmap.Width;
    H := Bitmap.Height;
    Max := FMaximum;
    Min := FMinimum;
    if fStyle in [LeftStyle, RightStyle] then
    begin
      W := Math.Min(W, H);
      H := Math.Min(W, H);
    end;
    N := FNumMainTicks*5;
    M := FMargin * K;
    //R := FCircleRadius * K;
    //with Bitmap^ do
    with Bitmap do
    begin
      // ***************************** Out Frame **************************
      if ShowFrame in fFaceOptions then
      begin
        if Show3D in fFaceOptions then
        begin
          Canvas.Pen.Width := 2 * K;
          Canvas.Pen.Color := clBtnShadow;
          Canvas.MoveTo(W, 0);
          Canvas.LineTo(0, 0);
          Canvas.LineTo(0, H);
          Canvas.Pen.Color := clBtnHighlight;
          Canvas.LineTo(W, H);
          Canvas.LineTo(W, 0);
        end
        else
        begin
          Canvas.Pen.Width := K;
          Canvas.Pen.Color := clBtnText;
          Canvas.Rectangle(0, 0, W, H);
        end;
      end;
      //************************* Out Margins **************************
      if ShowMargin in fFaceOptions then
      begin
        Canvas.Pen.Color := FMarginColor;
        Canvas.Pen.Width := K;
        Canvas.Rectangle(M, M, W - M, H - M);
      end;
      //****************************************************************
      case fStyle of
        RightStyle:
        begin
          A := 0;
          C := W - M;
          X := W - M;
          Y := H - M;
          if fScaleAngle > 90 then
            fScaleAngle := 90;
          J := W - 2*M;
        end;
        LeftStyle:
        begin
          A := 90;
          C := M;
          X := M;
          Y := H - M;
          if fScaleAngle > 90 then
            fScaleAngle := 90;
          J := W - 2*M;
        end;
        else
        begin
          X := W div 2;
          A := (180 - fScaleAngle)/2;
          C := W/2;
          if fScaleAngle >= 180 then
          begin
            J := (W - 2*M) div 2;
            Y := H div 2;
          end
          else
          begin
            J := Round(((W - 2*M)/2)/Cos(A*2*pi/360));
            if J > H - 2*M then
              J := H - 2*M;
            Y := (H - J) div 2 + J;
          end;
        end;
      end;{case}

{    // ************************************ base formula **********************************************
    Canvas.MoveTo(X, Y);
    Canvas.LineTo(Round(C-J*Cos((A+I*(FScaleAngle)/FNumMainTicks)*2*Pi/360)),
                  Round(Y-J*Sin((A+I*(FScaleAngle)/FNumMainTicks)*2*Pi/360))); }
    //******************************** Out Caption *******************
    if (ShowCaption in FFaceOptions) then
    begin
      Canvas.Font.Color := FCaptionColor;
      Canvas.TextOut(Round(C - J / 2 * Cos(A + FScaleAngle / 2) * 2 * Pi / 360) -
                     Canvas.TextWidth(FCaption) div 2,
                     Round(Y - J / 2 * Sin((A + FScaleAngle / 2) * 2 * Pi / 360)),
                     FCaption);
    end;
    //********************************** Out MinMaxLines *************************************
    Canvas.Pen.Cosmetic := True;
    Canvas.Pen.EndCap := pecFlat;//Square;
    Canvas.Pen.Width := 4 * k;
    if (ShowIndicatorMax in FFaceOptions) then
    begin
        Canvas.pen.color := FMaxColor;
        Sincos((A + FScaleAngle) * 2 * Pi / 360, Si, Co);
        Sincos((A + Max * FScaleAngle / FScaleValue) * 2 * Pi / 360, Si1, Co1);
        Canvas.Arc(X - J, Y - J, X + J, Y + J,
                   Round(C - J * Co),
                   Round(Y - J * Si),
                   Round(C - J * CO1),
                   Round(Y - J * SI1))
    end;
    if (ShowIndicatorMid in FFaceOptions) and (FMinimum < FMaximum) then
    begin
      Canvas.pen.color := FMidColor;
      Sincos((A + Max * FScaleAngle / FScaleValue) * 2 * Pi / 360, Si, Co);
      Sincos((A + Min * FScaleAngle / FScaleValue) * 2 * Pi / 360, Si1, Co1);

      Canvas.Arc(X - J, Y - J, X + J, Y + J,
                 Round(C - J * Co),
                 Round(Y - J * Si),
                 Round(C - J * Co1),
                 Round(Y - J * Si1))
    end;
    if (ShowIndicatorMin in FFaceOptions) then
    begin
      Canvas.pen.color:=FMinColor;
      Sincos((A + Min*FScaleAngle/FScaleValue)*2*Pi/360,si,co);
      Sincos(A*2*Pi/360,Si1,Co1);
      Canvas.Arc(X - J, Y - J, X + J, Y + J,
                 Round(C - J * Co),
                 Round(Y - J * Si),
                 Round(C - J * Co1),
                 Round(Y - J * Si1))
    end;
    Canvas.Font.Color := FValueColor;
    Canvas.Pen.Color := FTicksColor;
    Canvas.Pen.Width := K;
    //********************************** Out SubTicks *************************************
    if ShowSubTicks in fFaceOptions then
      for I := 0 to N do
      begin
        Sincos((A + I * (FScaleAngle) / N) * 2 * Pi / 360, Si, Co);
        Canvas.MoveTo(Round(C - (J - FLengthSubTicks * K) * Co),
                      Round(Y - (J - FLengthSubTicks * K) * Si));
        Canvas.LineTo(Round(C - (J) * Co),
                      Round(Y - (J) * Si))
      end;
    //********************************** Out Main Ticks ************************************
    for I := 0 to FNumMainTicks do
    begin
      if ShowMainTicks in fFaceOptions then
      begin
        Sincos((A+I*(FScaleAngle)/FNumMainTicks)*2*Pi/360,Si,Co);
        Canvas.MoveTo(Round(C-(J-FLengthMainTicks*K)*Co),
                      Round(Y-(J-FLengthMainTicks*K)*Si));
        Canvas.LineTo(Round(C-(J)*Co),
                      Round(Y-(J)*Si));
      end;
      //************************************* Out Circles ************************************
      if ShowCircles in fFaceOptions then
      begin
        Canvas.Brush.Color := FCircleColor;
        sincos((A+I*(FScaleAngle)/FNumMainTicks)*2*Pi/360,Si,Co);
{ecm}
        Canvas.Ellipse(Round(C-(J*Co-fCircleRadius*K)),
                       Round(Y-(J*Si-fCircleRadius*K)),
                       Round(C-(J*Co+fCircleRadius*K)),
                       Round(Y-(J*Si+fCircleRadius*K)));
{
        Canvas.Ellipse(Round(C-J*Co),//((A+I*(FScaleAngle)/FNumMainTicks)*2*Pi/360)) - R,
                       Round(Y-J*Si),//((A+I*(FScaleAngle)/FNumMainTicks)*2*Pi/360)) - R,
                       Round(C-J*Co),//((A+I*(FScaleAngle)/FNumMainTicks)*2*Pi/360)) + R,
                       Round(Y-J*Si))//((A+I*(FScaleAngle)/FNumMainTicks)*2*Pi/360)) + R);
}
{/ecm}
      end;
      // ************************************* Out Values *************************************
      if ShowValues in fFaceOptions then
      begin
        Canvas.Brush.Color := FFaceColor;
        Canvas.TextOut(Round(C-(J - fLengthMainTicks*K-5-I)*cos((A+i*(fScaleAngle)/fNumMainTicks)*2*pi/360)) -
                       Canvas.TextWidth(IntToStr(i*fScaleValue div fNumMainTicks))div 2,
                       Round(Y-(J-fLengthMainTicks*K-5)*sin((A+i*(fScaleAngle)/fNumMainTicks)*2*pi/360)),
                       IntToStr(i*fScaleValue div fNumMainTicks));
      end;
    end;
  end;
  //end;
end;

procedure TICGaugeIndicator.DrawArrow(Bitmap: TBitmap; K: Integer);
var
  J, X, Y, M, W, H, R: Integer;
  A, C: Single;
  Si, Co: Extended;
begin
  //with PGaugedata(CustomObj)^ do
  //with self do
  //begin
    M := FMargin * K;
    R := FCenterRadius * K;
    W := Bitmap.Width;
    H := Bitmap.Height;
    if fStyle in[ LeftStyle, RightStyle] then
    begin
      W := Math.Min(W, H);
      H := Math.Min(W, H);
    end;
    //with Bitmap^ do
    with Bitmap do
    begin
      case FStyle of
        RightStyle:
        begin
          A := 0;
          C := W - M;
          X := W - M;
          Y := H - M;
          if FScaleAngle > 90 then
            FScaleAngle := 90;
          J := W - 2*M;
        end;
        LeftStyle:
        begin
          A := 90;
          C := M;
          X := M;
          Y := H - M;
          if FScaleAngle > 90 then
            FScaleAngle := 90;
          J := W - 2*M;
        end;
        else
        begin
          X := W div 2;
          A := (180 - fScaleAngle)/2;
          C := W/2;
          if FScaleAngle >= 180 then
          begin
            J := (W - 2*M) div 2;
            Y := H div 2;
          end
          else
          begin
            J := Round(((W - 2*M)/2)/Cos(A*2*pi/360));
            if J > H - 2*M then
              J := H - 2*M;
            Y := (H - J) div 2 + J;
          end;
        end;
      end;{case}

      Canvas.Pen.Width := FArrowWidth * K;
      Canvas.Pen.Color := FArrowColor;
      Canvas.MoveTo(X, Y);
      Sincos((A + FPosition * FScaleAngle / FScaleValue) * 2 * Pi / 360, Si, Co);
      Canvas.LineTo(Round(C - J * Co), Round(Y - J * Si));
      //********************************* Out Center ***************************************
      if ShowCenter in FFaceOptions then
      begin
        Canvas.Brush.Color := FCenterColor;
        Canvas.Ellipse(X - R, Y - R, X + R, Y + R);
      end;
    end;
  //end;
end;

procedure TICGaugeIndicator.RedrawArrow;
begin
  //with PGaugedata(CustomObj)^ do
  //with self do
  //begin
    //BitBlt(FFaceBitmap.Canvas.Handle, 0, 0, FBackBitmap.Width,
    //       FBackBitmap.Height, FBackBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    FFaceBitmap.Canvas.CopyRect(TRect.Create(0, 0, FBackBitmap.Width, FBackBitmap.Height),
                                FBackBitmap.Canvas,
                                TRect.Create(0, 0, FBackBitmap.Width, FBackBitmap.Height));
    DrawArrow(FFaceBitmap, GetAAMultipler);
    //if FAntialiased <> aaNone then
    //  FastAntiAliasPicture;
    PaintGauge;
  //end;
end;

procedure TICGaugeIndicator.RedrawScale;
begin
  //with PGaugedata(CustomObj)^ do
//  with self do
  {ecm}
    if FLockRedraw = 0 then
  {/ecm}
    begin
      FBackbitmap.Canvas.Brush.Color := FFaceColor;
      FBackbitmap.Canvas.Brush.Style := bsSolid;
      FBackbitmap.canvas.Fillrect(FBackbitmap.canvas.cliprect);
      DrawScale(FBackBitmap, GetAAMultipler);
      RedrawArrow;
    end;
end;

//const
//  MaxPixelCount = MaxInt div SizeOf(TRGBTriple);
//
//type
//  PRGBArray = ^TRGBArray;
//  TRGBArray = array[0..MaxPixelCount-1] of TRGBTriple;

//procedure TICGaugeIndicator.FastAntiAliasPicture;
//var
//  x, y, cx, cy, cxi: Integer;
//  totr, totg, totb: cardinal;
//  Row1, Row2, Row3, Row4, DestRow: PRGBArray;
//  i,{ j,} k: Integer;
//begin
//  //with PGaugedata(CustomObj)^ do
//  with self do
//  begin
//    // For each row
//    if not Assigned(FFaceBitmap) then
//      Exit;
//    //  cx:=0;
//    K := GetAAMultipler;
//    Row2 := nil;
//    Row3 := nil;
//    Row4 := nil;
//    for Y := 0 to FAABitmap.Height - 1 do
//    begin
//      // We compute samples of K x K pixels
//      cy := y * K;
//      // Get pointers to actual, previous and next rows in supersampled bitmap
//      //    j:=FFacebitmap.ScanLineSize;
//      //    j:=j div 2;
//      Row1 := FFaceBitmap.ScanLine[cy];
//      if Row1 = nil then
//        Exit;
//      if K > 1 then
//        Row2 := FFaceBitmap.ScanLine[cy+1];
//      if K > 2 then
//        Row3 := FFaceBitmap.ScanLine[cy+2];
//      if K > 3 then
//        Row4 := FFaceBitmap.ScanLine[cy+3];
//      // Get a pointer to destination row in output bitmap
//      DestRow := FAABitmap.ScanLine[y];
//      // For each column...
//      for x := 0 to FAABitmap.Width - 1 do
//      begin
//        // We compute samples of 3 x 3 pixels
//        cx := x * K;
//        // Initialize result color
//        totr := 0;
//        totg := 0;
//        totb := 0;
//        if K > 3 then
//        begin
//          for i := 0 to 3 do
//          begin
//            cxi := cx + i;
//            totr := totr + Row1[cxi].rgbtRed + Row2[cxi].rgbtRed + Row3[cxi].rgbtRed + Row4[cxi].rgbtRed;
//            totg := totg + Row1[cxi].rgbtGreen + Row2[cxi].rgbtGreen + Row3[cxi].rgbtGreen + Row4[cxi].rgbtGreen;
//            totb := totb + Row1[cxi].rgbtBlue + Row2[cxi].rgbtBlue + Row3[cxi].rgbtBlue + Row4[cxi].rgbtBlue;
//          end;
//          DestRow[x].rgbtRed := totr shr 4 ;//div 16;
//          DestRow[x].rgbtGreen := totg shr 4 ;//16;
//          DestRow[x].rgbtBlue := totb shr 4 ;//16;
//        end
//        else
//          if K > 2 then
//          begin
//            for i := 0 to 2 do
//            begin
//              cxi := cx + i;
//              totr := totr + Row1[cxi].rgbtRed + Row2[cxi].rgbtRed + Row3[cxi].rgbtRed;
//              totg := totg + Row1[cxi].rgbtGreen + Row2[cxi].rgbtGreen + Row3[cxi].rgbtGreen;
//              totb := totb + Row1[cxi].rgbtBlue + Row2[cxi].rgbtBlue + Row3[cxi].rgbtBlue;
//            end;
//            DestRow[x].rgbtRed := totr div 9;
//            DestRow[x].rgbtGreen := totg div 9;
//            DestRow[x].rgbtBlue := totb div 9;
//          end
//          else
//            if K > 1 then
//            begin
//             for i := 0 to 1 do
//             begin
//               cxi := cx + i;
//               totr := totr + Row1[cxi].rgbtRed + Row2[cxi].rgbtRed;
//               totg := totg + Row1[cxi].rgbtGreen + Row2[cxi].rgbtGreen;
//               totb := totb + Row1[cxi].rgbtBlue + Row2[cxi].rgbtBlue;
//             end;
//             DestRow[x].rgbtRed := totr  shr 2;
//             DestRow[x].rgbtGreen := totg shr 2;
//             DestRow[x].rgbtBlue := totb shr 2;
//           end
//          else
//          begin
//            DestRow[x].rgbtRed   := Row1[cx].rgbtRed;
//            DestRow[x].rgbtGreen := Row1[cx].rgbtGreen;
//            DestRow[x].rgbtBlue  := Row1[cx].rgbtBlue;
//          end;
//        end;
//      end
//  end;
//end;

procedure TICGaugeIndicator.PaintGauge;
//var
//  p: TPaintStruct;
begin
  //beginPaint(handle, p);
  //with PGaugedata(CustomObj)^ do
  //begin
  if FAntiAliased = aaNone then
      //BitBlt(p.hdc, 0, 0, FFaceBitmap.Width,
      //       FFaceBitmap.Height,FFaceBitmap.Canvas.Handle, 0, 0, SRCCOPY)
    Canvas.CopyRect(TRect.Create(0, 0, FFaceBitmap.Width, FFaceBitmap.Height),
                    FFaceBitmap.Canvas,
                    TRect.Create(0, 0, FFaceBitmap.Width, FFaceBitmap.Height))
  else
      //BitBlt(p.hdc, 0, 0, FAABitmap.Width,
      //       FAABitmap.Height, FAABitmap.Canvas.Handle, 0, 0, SRCCOPY);
      Canvas.CopyRect(TRect.Create(0, 0, FAABitmap.Width, FAABitmap.Height),
                      FAABitmap.Canvas,
                      TRect.Create(0, 0, FAABitmap.Width, FAABitmap.Height));
  //end;
  //endpaint(handle,p);
end;

{ ------------------------------------------------------------------------- }
procedure TICGaugeIndicator.SetFMinColor(C: TColor);
begin
  //with PGaugedata(CustomObj)^ do
  if C <> FMinColor then
  begin
    FMinColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFMidColor(C: TColor);
begin
  //with PGaugedata(CustomObj)^ do
  if C <> FMidColor then
  begin
    FMidColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFMaxColor(C: TColor);
begin
  //with PGaugedata(CustomObj)^ do
  if C <> FMaxColor then
  begin
    FMaxColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFaceColor(C: TColor);
begin
  //with PGaugedata(CustomObj)^ do
  if C <> FFaceColor then
  begin
    FFaceColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFTicksColor(C: TColor);
begin
  //with PGaugedata(CustomObj)^ do
  if C <> FTicksColor then
  begin
    FTicksColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFValueColor(C: TColor);
begin
  //with PGaugedata(CustomObj)^ do
  if C <> FValueColor then
  begin
    FValueColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFCaptionColor(C: TColor);
begin
  //with PGaugedata(CustomObj)^ do
  if C <> FCaptionColor then
  begin
    FCaptionColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFArrowColor(C: TColor);
begin
  //with PGaugedata(CustomObj)^ do
  if C <> FArrowColor then
  begin
    FArrowColor := C;
    RedrawArrow
  end;
end;

procedure TICGaugeIndicator.SetFMarginColor(C: TColor);
begin
  //with PGaugedata(CustomObj)^ do
  if C <> FMarginColor then
  begin
    FMarginColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFCenterColor(C: TColor);
begin
  //with PGaugedata(CustomObj)^ do
  if C <> FCenterColor then
  begin
    FCenterColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFCircleColor(C: TColor);
begin
  //with PGaugedata(CustomObj)^ do
  if C <> FCircleColor then
  begin
    FCircleColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFCenterRadius(I: Integer);
begin
  //with PGaugedata(CustomObj)^ do
  if I <> FCenterRadius then
  begin
    FCenterRadius := I;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFCircleRadius(I: Integer);
begin
  //with PGaugedata(CustomObj)^ do
  if I <> FCircleRadius then
  begin
    FCircleRadius := I;
    RedrawScale
  end
end;

procedure TICGaugeIndicator.SetFScaleAngle(I: Integer);
begin
  //with PGaugedata(CustomObj)^ do
  if I <> FScaleAngle then
  begin
    if (I > 10) and (I <= 360) then
      FScaleAngle := I;
    RedrawScale;
  end;
end;

procedure TICGaugeIndicator.SetFMargin(I: Integer);
begin
  //with PGaugedata(CustomObj)^ do
  if I <> FMargin then
  begin
    FMargin := I;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFStyle(S: TStyle);
begin
  //with PGaugedata(CustomObj)^ do
  if S <> FStyle then
  begin
    FStyle := S;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFArrowWidth(I: Integer);
begin
  //with PGaugedata(CustomObj)^ do
  if I <> FArrowWidth then
  begin
    if I < 1 then
      fArrowWidth := 1
    else
      if I > 5 then
        fArrowWidth := 5
      else
        fArrowWidth := i;
    RedrawArrow;
  end
end;

procedure TICGaugeIndicator.SetFNumMainTicks(I: Integer);
begin
  //with PGaugedata(CustomObj)^ do
  if I <> FNumMainTicks then
  begin
    FNumMainTicks := I;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFLengthMainTicks(I: Integer);
begin
  //with PGaugedata(CustomObj)^ do
  if I <> FLengthMainTicks then
  begin
    FLengthMainTicks := I;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFLengthSubTicks(I: Integer);
begin
  //with PGaugedata(CustomObj)^ do
  if I <> FLengthSubTicks then
  begin
    FLengthSubTicks := I;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFFaceOptions(O: TFaceOptions);
begin
  //with PGaugedata(CustomObj)^ do
  if O <> FFaceOptions then
  begin
    FFaceOptions := O;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFPosition(V: Single);
begin
  //with PGaugedata(CustomObj)^ do
  if V <> FPosition then
  begin
    FPosition := V;
    if (FPosition > fMaximum) and Assigned(FOverMax) then
      //FOverMax(@Self);
      FOverMax(Self);
    if (FPosition < fMinimum) and Assigned(FOverMin) then
      //FOverMin(@Self);
      FOverMin(Self);
    RedrawArrow;
  end
end;

procedure TICGaugeIndicator.SetFScaleValue(I: Integer);
begin
  //with PGaugedata(CustomObj)^ do
  if I <> FScaleValue then
  begin
    if I > 1 then
    begin
      FScaleValue := I;
      if FMaximum >= FScaleValue then
        FMaximum := FScaleValue - 1;
      if FMinimum > FScaleValue - FMaximum then
        FMinimum := FScaleValue - fMaximum;
    end;
    RedrawScale;
  end;
end;

procedure TICGaugeIndicator.SetFMaximum(I: Integer);
begin
  //with PGaugedata(CustomObj)^ do
  if I <> FMaximum then
  begin
    if (I > 0) and (I < FScaleValue) then
      FMaximum := I;
    RedrawScale;
  end;
end;

procedure TICGaugeIndicator.SetFMinimum(I: Integer);
begin
  //with PGaugedata(CustomObj)^ do
  if I <> FMinimum then
  begin
    if (I > 0) and (I < FScaleValue) then
      FMinimum := I;
    RedrawScale;
  end
end;

procedure TICGaugeIndicator.SetFCaption(const S: string);
begin
  //TODO
  //with PGaugedata(CustomObj)^ do
  if S <> FCaption then
  begin
    Canvas.Font.Assign(Font);
    FCaption := S;
    RedrawScale
  end
end;

procedure TICGaugeIndicator.SetFAntiAliased(V: TAntialiased);
//var
//  K: Integer;
begin
  //with PGaugedata(CustomObj)^ do
  if V <> FAntiAliased then
  begin
    FAntiAliased := V;
    ReInitialize;
  end
end;

function TICGaugeIndicator.GetAAMultipler: Integer;
begin
  //with PGaugedata(CustomObj)^ do
  case FAntiAliased of
    aaBiline: Result := 2;
    aaTriline: Result := 3;
    aaQuadral: Result := 4;
    else Result := 1
  end
end;


function TICGaugeIndicator.GetAntiAliased: TAntialiased;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FAntiAliased;
  Result := FAntiAliased;
end;

function TICGaugeIndicator.GetArrowColor: TColor;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FArrowColor;
  Result := FArrowColor;
end;

function TICGaugeIndicator.GetArrowWidth: Integer;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FArrowWidth;
  Result := FArrowWidth;
end;

function TICGaugeIndicator.GetCaptionColor: TColor;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FCaptionColor;
  Result := FCaptionColor;
end;

function TICGaugeIndicator.GetCenterColor: TColor;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.Fcentercolor;
  Result := Fcentercolor;
end;

function TICGaugeIndicator.GetCenterRadius: Integer;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FCenterRadius;
  Result := FCenterRadius;
end;

function TICGaugeIndicator.GetCircleColor: TColor;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FCircleColor;
  Result := FCircleColor;
end;

function TICGaugeIndicator.GetCircleRadius: Integer;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FCircleRadius;
  Result := FCircleRadius;
end;

function TICGaugeIndicator.GetFaceColor: TColor;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FFaceColor;
  Result := FFaceColor;
end;

function TICGaugeIndicator.GetFaceOptions: TFaceOptions;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FFaceOptions;
  Result := FFaceOptions;
end;

function TICGaugeIndicator.GetFCaption: string;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FCaption;
  Result := FCaption;
end;

function TICGaugeIndicator.GetLengthMainTicks: Integer;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FLengthMainTicks;
  Result := FLengthMainTicks;
end;

function TICGaugeIndicator.GetLengthSubTicks: Integer;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FLengthSubTicks;
  Result := FLengthSubTicks;
end;

function TICGaugeIndicator.GetMargin: Integer;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FMargin;
  Result := FMargin;
end;

function TICGaugeIndicator.GetMarginColor: TColor;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FMarginColor;
  Result := FMarginColor;
end;

function TICGaugeIndicator.GetMaxColor: TColor;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FMaxColor;
  Result := FMaxColor;
end;

function TICGaugeIndicator.GetMaximum: Integer;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FMaximum;
  Result := FMaximum;
end;

function TICGaugeIndicator.GetMidColor: TColor;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FMidColor;
  Result := FMidColor;
end;

function TICGaugeIndicator.GetMinColor: TColor;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FMinColor;
  Result := FMinColor;
end;

function TICGaugeIndicator.GetMinimum: Integer;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FMinimum;
  Result := FMinimum;
end;

function TICGaugeIndicator.GetNumMainTicks: Integer;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FNumMainTicks;
  Result := FNumMainTicks;
end;

function TICGaugeIndicator.GetOverMax: TNotifyEvent;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FOverMax;
  Result := FOverMax;
end;

function TICGaugeIndicator.GetOverMin: TNotifyEvent;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FOverMin;
  Result := FOverMin;
end;

function TICGaugeIndicator.GetPosition: Single;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FPosition;
  Result := FPosition;
end;

function TICGaugeIndicator.GetScaleAngle: Integer;
//var
//  d:PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FScaleAngle;
  Result := FScaleAngle;
end;

function TICGaugeIndicator.GetScaleValue: Integer;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FScaleValue;
  Result := FScaleValue;
end;

function TICGaugeIndicator.GetStyle: TStyle;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FStyle;
  Result := FStyle;
end;

function TICGaugeIndicator.GetTicksColor: TColor;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FTicksColor;
  Result := FTicksColor;
end;

function TICGaugeIndicator.GetValueColor: TColor;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Result := d.FValueColor;
  Result := FValueColor;
end;

procedure TICGaugeIndicator.SetOverMax(const Value: TNotifyEvent);
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //d.FOverMax := Value;;
  FOverMax := Value;;
end;

procedure TICGaugeIndicator.SetOverMin(const Value: TNotifyEvent);
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //d.FOverMin := Value;
  FOverMin := Value;
end;

procedure TICGaugeIndicator.Reinitialize;
var
  K: integer;
begin
  //with PGaugedata(CustomObj)^ do
  //begin
    if Width < 30 then
      Width := 30;
    if Height < 30 then
      Height := 30;
    K := GetAAMultipler;
    if FAntiAliased = aaNone then
      K := 1;
    if Assigned(FFacebitmap) then
      FFaceBitmap.Free;
    if assigned(FBackBitmap) then
      FBackBitmap.Free;
    if assigned(FaaBitmap) then
      FaaBitmap.Free;

    //FBackBitmap := NewdibBitmap( Width * K, Height * K, pf24Bit);
    //FFaceBitmap := NewDibBitmap(Width * K, Height * K, pf24Bit);
    //FAABitmap := NewDibBitmap(Width,Height,pf24Bit);
    FBackBitmap := TBitmap.Create();
    FBackBitmap.SetSize(Width * K, Height * K);
    FFaceBitmap := TBitmap.Create();
    FFaceBitmap.SetSize(Width * K, Height * K);
    FAABitmap := TBitmap.Create();
    FAABitmap.SetSize(Width, Height);

    FBackbitmap.Canvas.Font.Assign(Font);
    FaaBitmap.Canvas.Font.Assign(Font);
    FFacebitmap.Canvas.Font.Assign(Font);
    FBackBitMap.Canvas.Font.Height := FaaBitmap.Canvas.Font.Height * K;
    FBackBitMap.Canvas.Font.Quality := fqAntiAliased;
    RedrawScale;
  //end;
end;

//{ TGaugeData }
constructor TICGaugeIndicator.Create(AOwner: TComponent);
begin
  inherited;
  Init;
end;

destructor TICGaugeIndicator.Destroy;
begin
  if Assigned(FFacebitmap) then
    FFaceBitmap.Free;
  if Assigned(FBackBitmap) then
    FBackBitmap.Free;
  if Assigned(FaaBitmap) then
    FaaBitmap.Free;
  inherited;
end;

procedure TICGaugeIndicator.Init;
begin
  inherited;
  FFaceColor := clwindow;
  FTicksColor := clBlack;
  FValueColor := clBlack;
  FCaptionColor := clRed;
  FArrowColor := clBlack;
  FMarginColor := clSilver;
  FCenterColor := clWindow;
  FCircleColor := clBtnFace;
  FMinColor := clGreen;
  FMidColor := clLime;
  FMaxColor := clRed;
  FArrowWidth := 1;
  FPosition := 0;
  FMargin := 4;
  FStyle := CenterStyle;
  FScaleValue := 120;
  FMaximum := 100;
  FMinimum := 20;
  FScaleAngle := 100;
  FCircleRadius := 1;
  FCenterRadius := 3;
  FNumMainTicks := 12;
  FLengthMainTicks := 15;
  FLengthSubTicks := 8;
  FCaption := 'dB';
  FFaceOptions := [ShowMargin, ShowMainTicks, ShowSubTicks, ShowIndicatorMin, SHowindicatormid,ShowIndicatorMax,
                   ShowValues, ShowCenter, ShowFrame, Show3D, ShowCaption];
  FAntiAliased := aaQuadral;
end;

{ecm}
procedure TICGaugeIndicator.LockRedraw;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //Inc(d.FLockRedraw);
  Inc(FLockRedraw);
end;

procedure TICGaugeIndicator.UnLockRedraw;
//var
//  d: PGaugeData;
begin
  //d := PGaugedata(CustomObj);
  //if d.FLockRedraw > 0 then
  //  Dec(d.FLockRedraw);
  //if d.FLockRedraw = 0 then
  //  RedrawScale;
  if FLockRedraw > 0 then
    Dec(FLockRedraw);
  if FLockRedraw = 0 then
    RedrawScale;
end;
{/ecm}

end.
