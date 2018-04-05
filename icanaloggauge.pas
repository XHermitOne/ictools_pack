{
Модуль компонента простого стрелочного индикатора c 3-мя зонами раскраски.
}
unit ICAnalogGauge;

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
  TAntialiased = (aaNone, aaBiline, aaTriline, aaQuadral);

  { Компонента простого стрелочного индикатора c 3-мя зонами раскраски }
  TICAnalogGauge = class(TGraphicControl)
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
    { Текщая позиция }
    FPosition: Double;
    { Диапазон значений }
    FScaleValue: Integer;
    { Граница минимальной зоны }
    FMinimum: Double;
    { Граница максимальной зоны }
    FMaximum: Double;
    { Надпись }
    FCaption: string;
    // event handlers
    FOverMax: TNotifyEvent;
    FOverMin: TNotifyEvent;
    // anti-aliasing mode
    FAntiAliased: TAntiAliased;
    // internal bitmaps
    FBackBitmap: TBitmap;
    FFaceBitmap: TBitmap;
    { Битмап для сглаживания движения стрелки }
    FAABitmap: TBitmap;
    // set properties
    {ecm}
    FLockRedraw: Integer;
    {/ecm}

    procedure SetMinColor(C: TColor);
    procedure SetMidColor(C: TColor);
    procedure SetMaxColor(C: TColor);
    procedure SetFaceColor(C: TColor);
    procedure SetTicksColor(C: TColor);
    procedure SetValueColor(C: TColor);
    procedure SetCaptionColor(C: TColor);
    procedure SetArrowColor(C: TColor);
    procedure SetMarginColor(C: TColor);
    procedure SetCenterColor(C: TColor);
    procedure SetCircleColor(C: TColor);
    procedure SetCenterRadius(I: Integer);
    procedure SetCircleRadius(I: Integer);
    procedure SetScaleAngle(I: Integer);
    procedure SetMargin(I: Integer);
    procedure SetStyle(S: TStyle);
    procedure SetArrowWidth(I: Integer);
    procedure SetNumMainTicks(I: Integer);
    procedure SetLengthMainTicks(I: Integer);
    procedure SetLengthSubTicks(I: Integer);
    procedure SetFaceOptions(O: TFaceOptions);
    procedure SetPosition(V: Double);
    procedure SetScaleValue(I: Integer);
    procedure SetMaximum(Value: Double);
    procedure SetMinimum(Value: Double);
    procedure SetCaption(S: string);
    procedure SetAntiAliased(V: TAntialiased);
    function GetAAMultipler: Integer;

  protected
    procedure DrawScale(Bitmap: TBitmap; K: Integer);
    procedure DrawArrow(Bitmap: TBitmap; K: Integer);
    procedure RedrawScale;
    procedure RedrawArrow;
    procedure FastAntiAliasPicture;
    procedure PaintGauge;
    procedure ReInitialize;

  public
    {ecm}
    procedure LockRedraw;
    procedure UnLockRedraw;
    {/ecm}

    procedure Init; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

  published
    property MinColor: TColor read FMinColor write SetMinColor;
    property MidColor: TColor read FMidColor write SetMidColor;
    property MaxColor: TColor read FMaxColor write SetMaxColor;
    property FaceColor: TColor read FFaceColor write SetFaceColor;
    property TicksColor: TColor read FTicksColor write SetTicksColor;
    property ValueColor: TColor read FValueColor write SetValueColor;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor;
    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    property MarginColor: TColor read FMarginColor write SetMarginColor;
    property CenterColor: TColor read FCenterColor write SetCenterColor;
    property CircleColor: TColor read FCircleColor write SetCircleColor;
    property CenterRadius: Integer read FCenterRadius write SetCenterRadius;
    property CircleRadius: Integer read FCircleRadius write SetCircleRadius;
    property Angle: Integer read FScaleAngle write SetScaleAngle;
    property GaugeMargin: Integer read FMargin write SetMargin;
    property GaugeStyle: TStyle read FStyle write SetStyle;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth;
    property NumberMainTicks: Integer read FNumMainTicks write SetNumMainTicks;
    property LengthMainTicks: Integer read FLengthMainTicks write SetLengthMainTicks;
    property LengthSubTicks: Integer read FLengthSubTicks write SetLengthSubTicks;
    property FaceOptions: TFaceOptions read FFaceOptions write SetFaceOptions;
    property GaugePosition: Double read FPosition write SetPosition;
    property Scale: Integer read FScaleValue write SetScaleValue;
    property IndMaximum: Double read FMaximum write SetMaximum;
    property IndMinimum: Double read FMinimum write SetMinimum;
    property GaugeCaption: string read FCaption write SetCaption;
    property AntiAliased: TAntialiased read FAntiAliased write SetAntiAliased;
    property OnOverMax: TNotifyEvent read FOverMax write FOverMax;
    property OnOverMin: TNotifyEvent read FOverMin write FOverMin;

  end;

procedure Register;

implementation

uses
  Math,
  LCLType;

procedure Register;
begin
  {$I icanaloggauge_icon.lrs}
  RegisterComponents('IC Tools',[TICAnalogGauge]);
end;

constructor TICAnalogGauge.Create(AOwner: TComponent);
begin
  inherited;

  FBackBitmap := TBitmap.Create;
  FFaceBitmap := TBitmap.Create;
  FAABitmap := TBitmap.Create;

  Init;
end;

destructor TICAnalogGauge.Destroy;
begin
  if Assigned(FBackBitmap) then
    FBackBitmap.Free;
  if Assigned(FFaceBitmap) then
    FFaceBitmap.Free;
  if Assigned(FAABitmap) then
    FAABitmap.Free;
  inherited;
end;

procedure TICAnalogGauge.Init;
begin
  inherited;
  FFaceColor := clForm;
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
  FCenterRadius := 5;
  FNumMainTicks := 12;
  FLengthMainTicks := 15;
  FLengthSubTicks := 8;
  FCaption := '---';
  FFaceOptions := [ShowMargin, ShowMainTicks, ShowSubTicks, ShowIndicatorMin, SHowindicatormid,ShowIndicatorMax,
                   ShowValues, ShowCenter, ShowFrame, ShowCaption];
  FAntiAliased := aaNone;
end;

procedure TICAnalogGauge.Paint;
begin
  inherited;
  ReInitialize;
  PaintGauge;
end;

procedure TICAnalogGauge.DrawScale(Bitmap: TBitmap; K: Integer);
var
  I, J, X, Y, N, M, W, H: Integer;
  Max, Min: Double;
  A, C: Single;
  SI, CO, SI1, CO1:Extended;
begin
  W := Bitmap.Width;
  H := Bitmap.Height;
  Max := FMaximum;
  Min := FMinimum;
  if FStyle in [LeftStyle, RightStyle] then
  begin
    W := Math.Min(W,H);
    H := Math.Min(W,H);
  end;
  N := FNumMainTicks * 5;
  M := FMargin * K;
  with Bitmap do
  begin
    // ***************************** Out Frame **************************
    if ShowFrame in FFaceOptions then
    begin
      if Show3D in FFaceOptions then
      begin
        Canvas.Pen.Width := 2 * K;
        Canvas.Pen.Color := clBtnShadow;
        Canvas.MoveTo(W, 0);
        Canvas.LineTo(0, 0);
        Canvas.LineTo(0, H);
        Canvas.Pen.Color := clBtnHighlight;
        Canvas.LineTo(W, H); Canvas.LineTo(W, 0);
      end
      else
      begin
        Canvas.Pen.Width := K;
        Canvas.Pen.Color := clBtnText;
        Canvas.Rectangle(0, 0, W, H);
      end;
    end;
    //************************* Out Margins **************************
    if ShowMargin in FFaceOptions then
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
        if FScaleAngle > 90 then
          FScaleAngle := 90;
        J := W - 2 * M;
      end;
      LeftStyle:
      begin
        A := 90;
        C := M;
        X := M;
        Y := H - M;
        if FScaleAngle > 90 then
          FScaleAngle := 90;
        J := W - 2 * M;
      end;
      else
      begin
        X := W div 2;
        A := (180 - FScaleAngle) / 2;
        C := W / 2;
        if FScaleAngle >= 180 then
        begin
          J := (W - 2 * M) div 2;
          Y := H div 2;
        end
        else
        begin
          J := Round(((W - 2 * M) / 2) / Cos(A * 2 * Pi / 360));
          if J > H - 2 * M then
            J := H - 2 * M;
          Y := (H - J) div 2 + J;
        end;
      end;
    end;{case}

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
    Canvas.Pen.EndCap := pecFlat;        //Square;
    Canvas.Pen.Width := 4 * K;
    if (ShowIndicatorMax in FFaceOptions) then
    begin
      Canvas.pen.color := FMaxColor;
      SinCos((A + FScaleAngle) * 2 * Pi / 360, Si, Co);
      SinCos((A + Max * FScaleAngle / FScaleValue) * 2 * Pi / 360, Si1, Co1);
      Canvas.Arc(X - J, Y - J, X + J, Y + J,
                 Round(C - J * Co),
                 Round(Y - J * Si),
                 Round(C - J * Co1),
                 Round(Y - J * Si1))
    end;
    if (ShowIndicatorMid in FFaceOptions) and (FMinimum < FMaximum) then
    begin
      Canvas.Pen.Color := FMidColor;
      SinCos((A + Max * FScaleAngle / FScaleValue) * 2 * Pi / 360, Si, Co);
      SinCos((A + Min * FScaleAngle / FScaleValue) * 2 * Pi / 360, Si1, Co1);

      Canvas.Arc(X - J, Y - J, X + J, Y + J,
                 Round(C - J * Co),
                 Round(Y - J * Si),
                 Round(C - J * Co1),
                 Round(Y - J * Si1))
    end;
    if (ShowIndicatorMin in FFaceOptions) then
    begin
      Canvas.Pen.Color := FMinColor;
      SinCos((A + Min * FScaleAngle / FScaleValue) * 2 * Pi / 360, Si, Co);
      SinCos(A * 2 * Pi / 360, Si1, Co1);
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
    if ShowSubTicks in FFaceOptions then
      for I := 0 to N do
      begin
        SinCos((A + I * (FScaleAngle) / N) * 2 * Pi / 360, Si, Co);
        Canvas.MoveTo(Round(C - (J - FLengthSubTicks * K) * Co),
                      Round(Y - (J - FLengthSubTicks * K) * Si));
        Canvas.LineTo(Round(C - (J) * Co),
                      Round(Y - (J) * Si))
      end;
    //********************************** Out Main Ticks ************************************
    for I := 0 to FNumMainTicks do
    begin
      if ShowMainTicks in FFaceOptions then
      begin
        SinCos((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360, Si, Co);
        Canvas.MoveTo(Round(C - (J - FLengthMainTicks * K) * Co),
                      Round(Y - (J - FLengthMainTicks * K) * Si));
        Canvas.LineTo(Round(C - (J) * Co),
                      Round(Y - (J) * Si));
        end;
      //************************************* Out Circles ************************************
      if ShowCircles in FFaceOptions then
      begin
        Canvas.Brush.Color := FCircleColor;
        SinCos((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360, Si, Co);
{ecm}
        Canvas.Ellipse(Round(C - (J * Co - FCircleRadius * K)),
                       Round(Y - (J * Si - FCircleRadius * K)),
                       Round(C - (J * Co + FCircleRadius * K)),
                       Round(Y - (J * Si + FCircleRadius * K)));
{/ecm}
      end;
      // ************************************* Out Values *************************************
      if ShowValues in FFaceOptions then
      begin
        Canvas.Brush.Color := FFaceColor;
        Canvas.TextOut(Round(C - (J - FLengthMainTicks * K - 5 - I) * Cos((A + I * (FScaleAngle) / fNumMainTicks) * 2 * Pi / 360)) -
                       Canvas.TextWidth(IntToStr(I * FScaleValue div FNumMainTicks))div 2,
                       Round(Y - (J - FLengthMainTicks * K - 5) * Sin((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360)),
                       IntToStr(I * FScaleValue div fNumMainTicks));
      end;
    end;
  end;
end;

procedure TICAnalogGauge.DrawArrow(Bitmap: TBitmap; K: Integer);
var
  J, X, Y, M, W, H, R: Integer;
  A, C: Single;
  Si, Co: Extended;
begin
  M := FMargin * K;
  R := FCenterRadius * K;
  W := Bitmap.Width;
  H := Bitmap.Height;
  if FStyle in [LeftStyle, RightStyle] then
  begin
    W := Math.Min(W, H);
    H := Math.Min(W, H);
  end;

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
        J := W - 2 * M;
      end;
      LeftStyle:
      begin
        A := 90;
        C := M;
        X := M;
        Y := H - M;
        if FScaleAngle > 90 then
          FScaleAngle := 90;
        J := W - 2 * M;
      end;
      else
      begin
        X := W div 2;
        A := (180 - FScaleAngle) / 2;
        C := W / 2;
        if FScaleAngle >= 180 then
        begin
          J := (W - 2 * M) div 2;
          Y := H div 2;
        end
        else
        begin
          J := Round(((W - 2 * M) / 2) / Cos(A * 2 * Pi / 360));
          if J > H - 2 * M then
            J := H - 2 * M;
          Y := (H - J) div 2 + J;
        end;
      end;
    end;{case}

    Canvas.Pen.Width := FArrowWidth * K;
    Canvas.Pen.Color := FArrowColor;
    Canvas.MoveTo(X, Y);
    SinCos((A + FPosition * FScaleAngle / FScaleValue) * 2 * Pi / 360, Si, Co);
    Canvas.LineTo(Round(C - J * Co),
                  Round(Y - J * Si));
    //********************************* Out Center ***************************************
    if ShowCenter in FFaceOptions then
    begin
      Canvas.Brush.Color := FCenterColor;
      Canvas.Ellipse(X - R, Y - R, X + R, Y + R);
    end;
  end;
end;

procedure TICAnalogGauge.RedrawArrow;
begin
  FFaceBitmap.Canvas.CopyRect(TRect.Create(0, 0, FBackBitmap.Width, FBackBitmap.Height),
                              FBackBitmap.Canvas,
                              TRect.Create(0, 0, FBackBitmap.Width, FBackBitmap.Height));

  DrawArrow(FFaceBitmap, GetAAMultipler);

  if FAntiAliased <> aaNone then
    FastAntiAliasPicture;
  PaintGauge;
end;

procedure TICAnalogGauge.RedrawScale;
begin
  {ecm}
  if FLockRedraw = 0 then
  {/ecm}
  begin
    FBackBitmap.Canvas.Brush.Color := FFaceColor;
    FBackBitmap.Canvas.Brush.Style := bsSolid;
    FBackBitmap.Canvas.FillRect(FBackBitmap.Canvas.ClipRect);
    DrawScale(FBackBitmap, GetAAMultipler);
    RedrawArrow;
  end;
end;

const
  MaxPixelCount = MaxInt div SizeOf(TRGBTriple);

type
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount - 1] of TRGBTriple;

procedure TICAnalogGauge.FastAntiAliasPicture;
var
  x, y, cx, cy, cxi: Integer;
  totr, totg, totb: cardinal;
  Row1, Row2, Row3, Row4, DestRow: PRGBArray;
  i, K: Integer;
begin
  // For each row
  if not Assigned(FFaceBitmap) then
    Exit;

  K := GetAAMultipler;
  Row2 := nil;
  Row3 := nil;
  Row4 := nil;

  for Y := 0 to FAABitmap.Height - 1 do
  begin
    // We compute samples of K x K pixels
    cy := y * K;
    // Get pointers to actual, previous and next rows in supersampled bitmap
    Row1 := FFaceBitmap.ScanLine[cy];
    if Row1 = nil then
      Exit;
    if K > 1 then
      Row2 := FFaceBitmap.ScanLine[cy + 1];
    if K > 2 then
      Row3 := FFaceBitmap.ScanLine[cy + 2];
    if K > 3 then
      Row4 := FFaceBitmap.ScanLine[cy + 3];
    // Get a pointer to destination row in output bitmap
    DestRow := FAABitmap.ScanLine[y];
    // For each column...
    for x := 0 to FAABitmap.Width - 1 do
    begin
      // We compute samples of 3 x 3 pixels
      cx := x * K;
      // Initialize result color
      totr := 0;
      totg := 0;
      totb := 0;
      if K > 3 then
      begin
        for i := 0 to 3 do
        begin
          cxi := cx + i;
          totr := totr + Row1^[cxi].rgbtRed + Row2^[cxi].rgbtRed + Row3^[cxi].rgbtRed + Row4^[cxi].rgbtRed;
          totg := totg + Row1^[cxi].rgbtGreen + Row2^[cxi].rgbtGreen + Row3^[cxi].rgbtGreen + Row4^[cxi].rgbtGreen;
          totb := totb + Row1^[cxi].rgbtBlue + Row2^[cxi].rgbtBlue + Row3^[cxi].rgbtBlue + Row4^[cxi].rgbtBlue;
        end;
        DestRow^[x].rgbtRed := totr shr 4 ;    //div 16;
        DestRow^[x].rgbtGreen := totg shr 4 ;  //16;
        DestRow^[x].rgbtBlue := totb shr 4 ;   //16;
      end
      else
        if K > 2 then
        begin
          for i := 0 to 2 do
          begin
            cxi := cx + i;
            totr := totr + Row1^[cxi].rgbtRed + Row2^[cxi].rgbtRed + Row3^[cxi].rgbtRed;
            totg := totg + Row1^[cxi].rgbtGreen + Row2^[cxi].rgbtGreen + Row3^[cxi].rgbtGreen;
            totb := totb + Row1^[cxi].rgbtBlue + Row2^[cxi].rgbtBlue + Row3^[cxi].rgbtBlue;
          end;
          DestRow^[x].rgbtRed := totr div 9;
          DestRow^[x].rgbtGreen := totg div 9;
          DestRow^[x].rgbtBlue := totb div 9;
        end
        else
          if K > 1 then
          begin
            for i := 0 to 1 do
            begin
              cxi := cx + i;
              totr := totr + Row1^[cxi].rgbtRed + Row2^[cxi].rgbtRed;
              totg := totg + Row1^[cxi].rgbtGreen + Row2^[cxi].rgbtGreen;
              totb := totb + Row1^[cxi].rgbtBlue + Row2^[cxi].rgbtBlue;
            end;
            DestRow^[x].rgbtRed := totr  shr 2;
            DestRow^[x].rgbtGreen := totg shr 2;
            DestRow^[x].rgbtBlue := totb shr 2;
          end
          else
          begin
            DestRow^[x].rgbtRed   := Row1^[cx].rgbtRed;
            DestRow^[x].rgbtGreen := Row1^[cx].rgbtGreen;
            DestRow^[x].rgbtBlue  := Row1^[cx].rgbtBlue;
          end;
        end;
    end;
end;

procedure TICAnalogGauge.PaintGauge;
begin
  if FAntiAliased = aaNone then
    Canvas.CopyRect(TRect.Create(0, 0, FFaceBitmap.Width, FFaceBitmap.Height),
                    FFaceBitmap.Canvas,
                    TRect.Create(0, 0, FFaceBitmap.Width, FFaceBitmap.Height))
  else
    Canvas.CopyRect(TRect.Create(0, 0, FAABitmap.Width, FAABitmap.Height),
                    FAABitmap.Canvas,
                    TRect.Create(0, 0, FAABitmap.Width, FAABitmap.Height));

end;

{ ------------------------------------------------------------------------- }
procedure TICAnalogGauge.SetMinColor(C: TColor);
begin
  if C <> FMinColor then
  begin
    FMinColor := C;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetMidColor(C: TColor);
begin
  if C <> FMidColor then
  begin
    FMidColor := C;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetMaxColor(C: TColor);
begin
  if C <> FMaxColor then
  begin
    FMaxColor := C;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetFaceColor(C: TColor);
begin
  if C <> FFaceColor then
  begin
    FFaceColor := C;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetTicksColor(C: TColor);
begin
  if C <> FTicksColor then
  begin
    FTicksColor := C;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetValueColor(C: TColor);
begin
  if C <> FValueColor then
  begin
    FValueColor := C;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetCaptionColor(C: TColor);
begin
  if C <> FCaptionColor then
  begin
    FCaptionColor := C;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetArrowColor(C: TColor);
begin
  if C <> FArrowColor then
  begin
    FArrowColor := C;
    RedrawArrow;
  end;
end;

procedure TICAnalogGauge.SetMarginColor(C: TColor);
begin
  if C <> FMarginColor then
  begin
    FMarginColor := C;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetCenterColor(C: TColor);
begin
  if C <> FCenterColor then
  begin
    FCenterColor := C;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetCircleColor(C: TColor);
begin
  if C <> FCircleColor then
  begin
    FCircleColor := C;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetCenterRadius(I: Integer);
begin
  if I <> FCenterRadius then
  begin
    FCenterRadius := I;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetCircleRadius(I: Integer);
begin
  if I <> FCircleRadius then
  begin
    FCircleRadius := I;
    RedrawScale;
  end
end;

procedure TICAnalogGauge.SetScaleAngle(I: Integer);
begin
  if I <> FScaleAngle then
  begin
    if (I > 10) and (I <= 360) then
      FScaleAngle := I;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetMargin(I: Integer);
begin
  if I <> FMargin then
  begin
    FMargin := I;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetStyle(S: TStyle);
begin
  if S <> FStyle then
  begin
    FStyle := S;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetArrowWidth(I: Integer);
begin
  if I <> FArrowWidth then
  begin
    if I < 1 then
      FArrowWidth := 1
    else
      if I > 5 then
        FArrowWidth := 5
      else
        FArrowWidth := I;
    RedrawArrow;
  end
end;

procedure TICAnalogGauge.SetNumMainTicks(I: Integer);
begin
  if I <> FNumMainTicks then
  begin
    FNumMainTicks := I;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetLengthMainTicks(I: Integer);
begin
  if I <> FLengthMainTicks then
  begin
    FLengthMainTicks := I;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetLengthSubTicks(I: Integer);
begin
  if I <> FLengthSubTicks then
  begin
    FLengthSubTicks := I;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetFaceOptions(O: TFaceOptions);
begin
  if O <> FFaceOptions then
  begin
    FFaceOptions := O;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetPosition(V: Double);
begin
  if V <> FPosition then
  begin
    FPosition := V;
    if (FPosition > FMaximum) and Assigned(FOverMax) then
      FOverMax(Self);
    if (FPosition < FMinimum) and Assigned(FOverMin) then
      FOverMin(Self);
    RedrawArrow;
  end
end;

procedure TICAnalogGauge.SetScaleValue(I: Integer);
begin
  if I <> FScaleValue then
  begin
    if I > 1 then
    begin
      FScaleValue := I;
      if FMaximum >= FScaleValue then
        FMaximum := FScaleValue - 1;
      if FMinimum > FScaleValue - FMaximum then
        FMinimum := FScaleValue - FMaximum;
    end;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetMaximum(Value: Double);
begin
  if Value <> FMaximum then
  begin
    if (Value > 0) and (Value < FScaleValue) then
      FMaximum := Value;
    RedrawScale;
  end;
end;

procedure TICAnalogGauge.SetMinimum(Value: Double);
begin
   if Value <> FMinimum then
   begin
     if (Value > 0) and (Value < FScaleValue) then
       FMinimum := Value;
     RedrawScale;
  end
end;

procedure TICAnalogGauge.SetCaption(S: string);
begin
  //TODO
  if S <> FCaption then
  begin
    Canvas.Font.Assign(Font);
    FCaption := S;
    RedrawScale;
  end
end;

procedure TICAnalogGauge.SetAntiAliased(V: TAntialiased);
begin
  if V <> FAntiAliased then
  begin
    FAntiAliased := V;
    ReInitialize;
  end
end;

function TICAnalogGauge.GetAAMultipler: Integer;
begin
  case FAntiAliased of
    aaBiline: Result := 2;
    aaTriline: Result := 3;
    aaQuadral: Result := 4;
    else Result := 1
  end
end;

procedure TICAnalogGauge.ReInitialize;
var
  K:integer;
begin
  if Width < 30 then
    Width := 30;
  if Height < 30 then
    Height := 30;

  K := GetAAMultipler;
  if FAntiAliased = aaNone then
    K := 1;

  if Assigned(FFaceBitmap) then
    FFaceBitmap.Free;
  if Assigned(FBackBitmap) then
    FBackBitmap.Free;
  if Assigned(FAABitmap) then
    FAABitmap.Free;

  FBackBitmap := TBitmap.Create;
  FBackBitmap.SetSize(Width * K, Height * K);
  FFaceBitmap := TBitmap.Create;
  FFaceBitmap.SetSize(Width * K, Height * K);
  FAABitmap := TBitmap.Create;
  FAABitmap.SetSize(Width,Height);

  FBackBitmap.Canvas.Font.Assign(Font);
  FAABitmap.Canvas.Font.Assign(Font);
  FFaceBitmap.Canvas.Font.Assign(Font);
  FBackBitmap.Canvas.Font.Height := FAABitmap.Canvas.Font.Height * K;
  FBackBitmap.Canvas.Font.Quality := fqAntiAliased;
  RedrawScale;
end;

{ecm}
procedure TICAnalogGauge.LockRedraw;
begin
  Inc(FLockRedraw);
end;

procedure TICAnalogGauge.UnLockRedraw;
begin
  if FLockRedraw > 0 then
    Dec(FLockRedraw);
  if FLockRedraw = 0 then
    RedrawScale;
end;
{/ecm}

end.
