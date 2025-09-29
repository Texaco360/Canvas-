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
    procedure FormResize(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    private
      RulerBitmap: TBGRABitmap;
      procedure DrawLines(Ctx: TBGRACanvas2D);
      procedure DrawRuler( Ctx: TBGRACanvas2D; const AWidth, AOrigin, AScale: Integer );
      procedure UpdateRulerBitmap;
    // procedure DrawHorizontalRuler(Ctx: TBGRACanvas2D; const R: TRect);
    
  end;

  type TPoint = record
    x: Double;
    y: Double;
  end;

  type TLine = record
    startPoint: TPoint;
    endPoint: TPoint;
  end;

var
  DrawingForm: TDrawingForm;

implementation

{$R *.lfm}


const
  cRulerHeight = 25;
var
  scale : Integer; //1mm = 5 du
  isDrawing : Boolean = False;
  startPoint, endPoint : TPoint;
  lineList : array of TLine;
  currentLine : TLine;
  lineCount : Integer = 0;

procedure TDrawingForm.UpdateRulerBitmap;
var
  ctx: TBGRACanvas2D;
  R: TRect;
  FWidth: Integer;
  FOrigin: TPoint;
  begin
    if Assigned(RulerBitmap) then RulerBitmap.Free;
    RulerBitmap := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAWhite);
    ctx := RulerBitmap.Canvas2D;

    // Draw horizontal ruler
    R := Rect(0, 0, RulerBitmap.Width-30, RulerBitmap.Height-30);
    FOrigin.x := RulerBitmap.Width/2;
    FOrigin.y := RulerBitmap.Height/2;
    FWidth := R.Width;
    ctx.save();
    ctx.translate(45,0);
    DrawRuler(ctx, FWidth, Floor(FOrigin.x), scale );
    ctx.restore;

    // Draw vertical ruler
    FWidth := R.Height;
    ctx.save();
    ctx.translate(0,RulerBitmap.Height);
    ctx.rotate(-pi/2);
    DrawRuler(ctx, FWidth, Floor(FOrigin.y),scale);
    ctx.restore;
end;


procedure TDrawingForm.DrawRuler(Ctx: TBGRACanvas2D; const AWidth, AOrigin, AScale: Integer);
const
  MinorTick   = 8;   // every 10 px a short tick
  MajorTick   = 40;   // every 50 px a longer tick + number
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
  Ctx.font        := '14px Tahoma';
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
     //scale := 2; //1mm = 10du
     UpdateRulerBitmap;
    PaintBox.Repaint;
end;

procedure TDrawingForm.FormResize(Sender: TObject);
begin
  UpdateRulerBitmap;
  PaintBox.Repaint;
end;

procedure TDrawingForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin
//start drawing a line
  if not isDrawing then
  begin
    isDrawing := True;
    startPoint.x := X;
    startPoint.y := Y;
    currentLine.startPoint := startPoint;
    currentLine.endPoint := startPoint;
  end
  else
  begin
    isDrawing := False;
    endPoint.x := X;
    endPoint.y := Y;
    currentLine.endPoint := endPoint;
    SetLength(lineList, lineCount + 1);
    lineList[lineCount] := currentLine;
    Inc(lineCount);
    PaintBox.Repaint;
  end;
end;

procedure TDrawingForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
begin
//update the current line end point
  if isDrawing then
  begin
    endPoint.x := X;
    endPoint.y := Y;
    currentLine.endPoint := endPoint;
    PaintBox.Repaint;
  end;
end;

procedure TDrawingForm.DrawLines(Ctx: TBGRACanvas2D);
var i: integer;
begin
  Ctx.strokeStyle('blue');
  Ctx.lineWidth := 2;
  Ctx.beginPath;
  for i := 0 to High(lineList) do
  begin
    Ctx.moveTo(lineList[i].startPoint.x, lineList[i].startPoint.y);
    Ctx.lineTo(lineList[i].endPoint.x, lineList[i].endPoint.y);
  end;
  Ctx.stroke;

  //draw the current line in red
  if isDrawing then
  begin
    Ctx.strokeStyle('red');
    Ctx.lineWidth := 2;
    Ctx.beginPath;
    Ctx.moveTo(currentLine.startPoint.x, currentLine.startPoint.y);
    Ctx.lineTo(currentLine.endPoint.x, currentLine.endPoint.y);
    Ctx.stroke;
  end;
end;

procedure TDrawingForm.PaintBoxPaint(Sender: TObject);
var
  bmp : TBGRABitmap;
  ctx : TBGRACanvas2D;
begin
  // --------------------------------------------------------------
  // 1️⃣ Create a bitmap the size of the client area
  // --------------------------------------------------------------
  bmp := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAWhite);
  
  try  
    ctx := bmp.Canvas2D;
    if Assigned(RulerBitmap) then
      bmp.PutImage(0, 0, RulerBitmap, dmDrawWithTransparency);

    DrawLines(ctx);


    bmp.Draw(PaintBox.Canvas, 0, 0);
  finally
    bmp.Free;
  end;
end;

end.

