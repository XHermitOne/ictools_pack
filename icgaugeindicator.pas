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
    FMinimum: Double;
    FMaximum: Double;
    FCaption: string;
    // event handlers
    FOverMax: TNotifyEvent;
    FOverMin: TNotifyEvent;
    // internal bitmaps
    FBackBitmap: TBitmap;
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
    procedure SetPosition(V: Single);
    procedure SetScaleValue(I: Integer);
    procedure SetMaximum(I: Double);
    procedure SetMinimum(I: Double);
    procedure SetCaption(const S: string);

  protected
    procedure DrawScale(Bitmap: TBitmap);
    procedure DrawArrow(Bitmap: TBitmap);
    procedure RedrawScale;
    procedure RedrawArrow;
    procedure PaintGauge;
    procedure Reinitialize;

  public
    procedure Init; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    {ecm}
    procedure LockRedraw;
    procedure UnLockRedraw;
    {/ecm}

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
    property GaugePosition: Single read FPosition write SetPosition;
    property Scale: Integer read FScaleValue write SetScaleValue;
    property IndMaximum: Double read FMaximum write SetMaximum;
    property IndMinimum: Double read FMinimum write SetMinimum;
    property GaugeCaption: string read FCaption write SetCaption;
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
  {$I icgaugeindicator_icon.lrs}
  RegisterComponents('IC Tools',[TICGaugeIndicator]);
end;


procedure TICGaugeIndicator.DrawScale(Bitmap: TBitmap);
var
  I, J, X, Y, N, M, W, H: Integer;
  Max, Min: Double;
  A, C: Single;
  SI, CO, SI1, CO1: Extended;
begin
  W := Bitmap.Width;
  H := Bitmap.Height;
  Max := FMaximum;
  Min := FMinimum;
  if fStyle in [LeftStyle, RightStyle] then
  begin
    W := Math.Min(W, H);
    H := Math.Min(W, H);
  end;
  N := FNumMainTicks * 5;
  M := FMargin;

  with Bitmap do
  begin
    // ***************************** Out Frame **************************
    if ShowFrame in fFaceOptions then
    begin
      if Show3D in fFaceOptions then
      begin
        Canvas.Pen.Width := 2;
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
        Canvas.Pen.Width := 1;
        Canvas.Pen.Color := clBtnText;
        Canvas.Rectangle(0, 0, W, H);
      end;
    end;
    //************************* Out Margins **************************
    if ShowMargin in fFaceOptions then
    begin
      Canvas.Pen.Color := FMarginColor;
      Canvas.Pen.Width := 1;
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
    Canvas.Pen.Width := 4;
    if (ShowIndicatorMax in FFaceOptions) then
    begin
      Canvas.pen.color := FMaxColor;
      SinCos((A + FScaleAngle) * 2 * Pi / 360, Si, Co);
      SinCos((A + Max * FScaleAngle / FScaleValue) * 2 * Pi / 360, Si1, Co1);
      Canvas.Arc(X - J, Y - J, X + J, Y + J,
                 Round(C - J * Co),
                 Round(Y - J * Si),
                 Round(C - J * CO1),
                 Round(Y - J * SI1))
    end;
    if (ShowIndicatorMid in FFaceOptions) and (FMinimum < FMaximum) then
    begin
      Canvas.pen.color := FMidColor;
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
      Canvas.pen.color:=FMinColor;
      SinCos((A + Min*FScaleAngle/FScaleValue)*2*Pi/360,si,co);
      SinCos(A*2*Pi/360,Si1,Co1);
      Canvas.Arc(X - J, Y - J, X + J, Y + J,
                 Round(C - J * Co),
                 Round(Y - J * Si),
                 Round(C - J * Co1),
                 Round(Y - J * Si1))
    end;
    Canvas.Font.Color := FValueColor;
    Canvas.Pen.Color := FTicksColor;
    Canvas.Pen.Width := 1;
    //********************************** Out SubTicks *************************************
    if ShowSubTicks in fFaceOptions then
      for I := 0 to N do
      begin
        SinCos((A + I * (FScaleAngle) / N) * 2 * Pi / 360, Si, Co);
        Canvas.MoveTo(Round(C - (J - FLengthSubTicks) * Co),
                      Round(Y - (J - FLengthSubTicks) * Si));
        Canvas.LineTo(Round(C - (J) * Co),
                      Round(Y - (J) * Si))
      end;
    //********************************** Out Main Ticks ************************************
    for I := 0 to FNumMainTicks do
    begin
      if ShowMainTicks in fFaceOptions then
      begin
        SinCos((A+I*(FScaleAngle)/FNumMainTicks)*2*Pi/360,Si,Co);
        Canvas.MoveTo(Round(C-(J-FLengthMainTicks)*Co),
                      Round(Y-(J-FLengthMainTicks)*Si));
        Canvas.LineTo(Round(C-(J)*Co),
                      Round(Y-(J)*Si));
      end;
      //************************************* Out Circles ************************************
      if ShowCircles in fFaceOptions then
      begin
        Canvas.Brush.Color := FCircleColor;
        SinCos((A + I * (FScaleAngle) / FNumMainTicks) * 2 * Pi / 360, Si, Co);
{ecm}
        Canvas.Ellipse(Round(C - (J * Co - fCircleRadius)),
                       Round(Y - (J * Si - fCircleRadius)),
                       Round(C - (J * Co + fCircleRadius)),
                       Round(Y - (J * Si + fCircleRadius)));
{/ecm}
      end;
      // ************************************* Out Values *************************************
      if ShowValues in fFaceOptions then
      begin
        Canvas.Brush.Color := FFaceColor;
        Canvas.TextOut(Round(C - (J - fLengthMainTicks - 5 - I) * Cos((A + i * (fScaleAngle) / fNumMainTicks) * 2 * Pi / 360)) -
                       Canvas.TextWidth(IntToStr(i * fScaleValue div fNumMainTicks))div 2,
                       Round(Y - (J - fLengthMainTicks - 5) * Sin((A + i * (fScaleAngle) / fNumMainTicks) * 2 * Pi / 360)),
                       IntToStr(i * fScaleValue div fNumMainTicks));
      end;
    end;
  end;
end;

procedure TICGaugeIndicator.DrawArrow(Bitmap: TBitmap);
var
  J, X, Y, M, W, H, R: Integer;
  A, C: Single;
  Si, Co: Extended;
begin
  M := FMargin;
  R := FCenterRadius;
  W := Bitmap.Width;
  H := Bitmap.Height;
  if fStyle in[ LeftStyle, RightStyle] then
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
    end;  {case}

    Canvas.Pen.Width := FArrowWidth;
    Canvas.Pen.Color := FArrowColor;
    Canvas.MoveTo(X, Y);
    SinCos((A + FPosition * FScaleAngle / FScaleValue) * 2 * Pi / 360, Si, Co);
    Canvas.LineTo(Round(C - J * Co), Round(Y - J * Si));
    //********************************* Out Center ***************************************
    if ShowCenter in FFaceOptions then
    begin
      Canvas.Brush.Color := FCenterColor;
      Canvas.Ellipse(X - R, Y - R, X + R, Y + R);
    end;
  end;
end;

procedure TICGaugeIndicator.RedrawArrow;
begin
  DrawArrow(FBackBitmap);

  Canvas.CopyRect(TRect.Create(0, 0, FBackBitmap.Width, FBackBitmap.Height),
                  FBackBitmap.Canvas,
                  TRect.Create(0, 0, FBackBitmap.Width, FBackBitmap.Height));
  PaintGauge;
end;

procedure TICGaugeIndicator.RedrawScale;
begin
  {ecm}
  if FLockRedraw = 0 then
  {/ecm}
  begin
    FBackBitmap.Canvas.Brush.Color := FFaceColor;
    FBackBitmap.Canvas.Brush.Style := bsSolid;
    FBackBitmap.Canvas.FillRect(FBackBitmap.Canvas.ClipRect);
    DrawScale(FBackBitmap);
    RedrawArrow;
  end;
end;

procedure TICGaugeIndicator.PaintGauge;
begin
  Canvas.CopyRect(TRect.Create(0, 0, FBackBitmap.Width, FBackBitmap.Height),
                  FBackBitmap.Canvas,
                  TRect.Create(0, 0, FBackBitmap.Width, FBackBitmap.Height))
end;

{ ------------------------------------------------------------------------- }
procedure TICGaugeIndicator.SetMinColor(C: TColor);
begin
  if C <> FMinColor then
  begin
    FMinColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetMidColor(C: TColor);
begin
  if C <> FMidColor then
  begin
    FMidColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetMaxColor(C: TColor);
begin
  if C <> FMaxColor then
  begin
    FMaxColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFaceColor(C: TColor);
begin
  if C <> FFaceColor then
  begin
    FFaceColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetTicksColor(C: TColor);
begin
  if C <> FTicksColor then
  begin
    FTicksColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetValueColor(C: TColor);
begin
  if C <> FValueColor then
  begin
    FValueColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetCaptionColor(C: TColor);
begin
  if C <> FCaptionColor then
  begin
    FCaptionColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetArrowColor(C: TColor);
begin
  if C <> FArrowColor then
  begin
    FArrowColor := C;
    RedrawArrow
  end;
end;

procedure TICGaugeIndicator.SetMarginColor(C: TColor);
begin
  if C <> FMarginColor then
  begin
    FMarginColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetCenterColor(C: TColor);
begin
  if C <> FCenterColor then
  begin
    FCenterColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetCircleColor(C: TColor);
begin
  if C <> FCircleColor then
  begin
    FCircleColor := C;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetCenterRadius(I: Integer);
begin
  if I <> FCenterRadius then
  begin
    FCenterRadius := I;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetCircleRadius(I: Integer);
begin
  if I <> FCircleRadius then
  begin
    FCircleRadius := I;
    RedrawScale
  end
end;

procedure TICGaugeIndicator.SetScaleAngle(I: Integer);
begin
  if I <> FScaleAngle then
  begin
    if (I > 10) and (I <= 360) then
      FScaleAngle := I;
    RedrawScale;
  end;
end;

procedure TICGaugeIndicator.SetMargin(I: Integer);
begin
  if I <> FMargin then
  begin
    FMargin := I;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetStyle(S: TStyle);
begin
  if S <> FStyle then
  begin
    FStyle := S;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetArrowWidth(I: Integer);
begin
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

procedure TICGaugeIndicator.SetNumMainTicks(I: Integer);
begin
  if I <> FNumMainTicks then
  begin
    FNumMainTicks := I;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetLengthMainTicks(I: Integer);
begin
  if I <> FLengthMainTicks then
  begin
    FLengthMainTicks := I;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetLengthSubTicks(I: Integer);
begin
  if I <> FLengthSubTicks then
  begin
    FLengthSubTicks := I;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetFaceOptions(O: TFaceOptions);
begin
  if O <> FFaceOptions then
  begin
    FFaceOptions := O;
    RedrawScale
  end;
end;

procedure TICGaugeIndicator.SetPosition(V: Single);
begin
  if V <> FPosition then
  begin
    FPosition := V;
    if (FPosition > fMaximum) and Assigned(FOverMax) then
      FOverMax(Self);
    if (FPosition < fMinimum) and Assigned(FOverMin) then
      FOverMin(Self);
    RedrawArrow;
  end
end;

procedure TICGaugeIndicator.SetScaleValue(I: Integer);
begin
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

procedure TICGaugeIndicator.SetMaximum(I: Double);
begin
  if I <> FMaximum then
  begin
    if (I > 0) and (I < FScaleValue) then
      FMaximum := I;
    RedrawScale;
  end;
end;

procedure TICGaugeIndicator.SetMinimum(I: Double);
begin
  if I <> FMinimum then
  begin
    if (I > 0) and (I < FScaleValue) then
      FMinimum := I;
    RedrawScale;
  end
end;

procedure TICGaugeIndicator.SetCaption(const S: string);
begin
  if S <> FCaption then
  begin
    Canvas.Font.Assign(Font);
    FCaption := S;
    RedrawScale
  end
end;

procedure TICGaugeIndicator.Reinitialize;
var
  K: integer;
begin
  if Width < 30 then
    Width := 30;
  if Height < 30 then
    Height := 30;

  K := 1;

  FBackBitmap := TBitmap.Create();
  FBackBitmap.SetSize(Width * K, Height * K);

  FBackBitmap.Canvas.Font.Assign(Font);
  FBackBitmap.Canvas.Font.Height := 1;
  FBackBitmap.Canvas.Font.Quality := fqAntiAliased;
  RedrawScale;
end;

constructor TICGaugeIndicator.Create(AOwner: TComponent);
begin
  inherited;

  FBackBitmap := TBitmap.Create;
  Init;
end;

destructor TICGaugeIndicator.Destroy;
begin
  if Assigned(FBackBitmap) then
    FBackBitmap.Free;
  inherited;
end;

procedure TICGaugeIndicator.Init;
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
end;

procedure TICGaugeIndicator.Paint;
begin
  inherited;
  // Установить  размеры кадров
  FBackBitmap.SetSize(Width, Height);

  // Переисовка заднего кадра
  RedrawScale;
  // Перенос заднего кадра на лицевой кадр
  PaintGauge;
end;

{ecm}
procedure TICGaugeIndicator.LockRedraw;
begin
  Inc(FLockRedraw);
end;

procedure TICGaugeIndicator.UnLockRedraw;
begin
  if FLockRedraw > 0 then
    Dec(FLockRedraw);
  if FLockRedraw = 0 then
    RedrawScale;
end;
{/ecm}

end.
