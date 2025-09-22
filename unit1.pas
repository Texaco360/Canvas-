unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, Math,
  StdCtrls, BGRABitmap, BGRABitmapTypes,BGRACanvas2D;


type

  { TDrawingForm }

  TDrawingForm = class(TForm)
    PaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);

    private
      procedure DrawRuler( Ctx: TBGRACanvas2D; const AWidth, AOrigin, AScale: Integer );
    // procedure DrawHorizontalRuler(Ctx: TBGRACanvas2D; const R: TRect);
  end;

  type TPoint = record
    x: Double;
    y: Double;
  end;

var
  DrawingForm: TDrawingForm;

implementation

{$R *.lfm}


const
  cRulerHeight = 30;
var
  scale : Integer; //1mm = 10 du

procedure TDrawingForm.DrawRuler(Ctx: TBGRACanvas2D; const AWidth, AOrigin, AScale: Integer);
const
  MinorTick   = 10;   // every 10 px a short tick
  MajorTick   = 50;   // every 50 px a longer tick + number
var
  x, y0, yMinor, yMajor: Integer;
  txt: string;
  txtW, txtH: Single;
begin
  // ---- 1️⃣ Fill the ruler background (light gray) -----------------
  Ctx.save;
  Ctx.fillStyle('rgb(240,128,5');
  Ctx.fillRect(0, 0, AWidth, cRulerHeight);
  Ctx.restore;

  // ---- 2️⃣ Y positions for the ticks -----------------------------
  y0     := 0 + cRulerHeight - 1;   // bottom line of the ruler
  yMinor := 5;                    // short tick length
  yMajor := 12;                   // long tick length

  // ---- 3️⃣ Common style -----------------------------------------
  Ctx.lineWidth   := 1;
  Ctx.font        := '20px Tahoma';
  Ctx.textBaseline:= 'top';           // numbers will be placed below the tick
  Ctx.textAlign   := 'center';

  // ---- 4️⃣ Loop across the width, draw ticks & numbers ----------
  for x := 0 to AWidth do
  begin
    // ----- minor tick -------------------------------------------
    if ((x - AOrigin) mod MinorTick = 0) then
    begin
      Ctx.beginPath;
      Ctx.moveTo(x, 0);
      Ctx.lineTo(x, yMinor);
      Ctx.stroke;
    end;

    // ----- major tick + label -----------------------------------
    if ((x- AOrigin) mod MajorTick = 0) then
    begin
      // longer tick
      Ctx.beginPath;
      Ctx.moveTo(x, 0);
      Ctx.lineTo(x, yMajor);
      Ctx.stroke;

      // label (pixel coordinate)
      txt := IntToStr(floor((x - AOrigin)/AScale));
      // optional measurement – helps centre the text vertically
      txtW := Ctx.measureText(txt).width;
      txtH := Ctx.measureText(txt).height;

      // draw the number just below the major tick
      Ctx.fillStyle(BGRABlack);
      Ctx.fillText(txt,
        x,                // X – centre of the tick
        yMajor + 2);               // Y – a couple of pixels under the tick
    end;
  end;
end;


procedure TDrawingForm.FormCreate(Sender: TObject);
begin
     PaintBox.Align := alClient;
    PaintBox.Repaint;
end;

procedure TDrawingForm.PaintBoxPaint(Sender: TObject);
var
  bmp : TBGRABitmap;
  ctx : TBGRACanvas2D;
  R   : TRect;
  FWidth : Integer;
  FOrigin : TPoint;
begin
  // --------------------------------------------------------------
  // 1️⃣ Create a bitmap the size of the client area
  // --------------------------------------------------------------
  bmp := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAWhite);
  scale := 5; //1mm = 10du
  try
    ctx := bmp.Canvas2D;

    // ctx.scale(3,3);
    // --------------------------------------------------------------
    // 2️⃣ Draw the horizontal ruler at the very top
    // --------------------------------------------------------------
    R := Rect(0, 0, bmp.Width-30, bmp.Height-30);
    FOrigin.x := bmp.Width/2;
    FOrigin.y := bmp.Height/2;
    FWidth := R.Width;
    ctx.save();
    ctx.translate(45,0);
    DrawRuler(ctx, FWidth, Floor(FOrigin.x), scale );
     ctx.restore;

    FWidth := R.Height;
    ctx.save();
    ctx.translate(0,bmp.Height);
    ctx.rotate(-pi/2);
    DrawRuler(ctx, FWidth, Floor(FOrigin.y),scale);
    ctx.restore;

    ctx.translate(45,0);



    // --------------------------------------------------------------
    // 3️⃣ Your original demo graphics (orange rectangle)
    // --------------------------------------------------------------
    ctx.fillStyle('rgb(240,128,5)');
    ctx.fillRect(30, 30 + cRulerHeight, 80, 60);   // shift down by ruler height
    ctx.strokeRect(50, 50 + cRulerHeight, 80, 60);

    // --------------------------------------------------------------
    // 4️⃣ Blit the bitmap onto the PaintBox canvas
    // --------------------------------------------------------------
    bmp.Draw(PaintBox.Canvas, 0, 0);
  finally
    bmp.Free;
  end;
end;

end.

