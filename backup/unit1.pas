unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRABitmap, BGRABitmapTypes, BGRACanvas2D, uDrawingEngine;

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
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    DrawingEngine: TDrawingEngine;
  end;

var
  DrawingForm: TDrawingForm;

implementation

{$R *.lfm}

procedure TDrawingForm.FormCreate(Sender: TObject);
begin
  DrawingEngine := TDrawingEngine.Create;
  DrawingEngine.UpdateRulerBitmap(ClientWidth, ClientHeight);
  PaintBox.Repaint;
end;

procedure TDrawingForm.FormResize(Sender: TObject);
begin
  DrawingEngine.UpdateRulerBitmap(ClientWidth, ClientHeight);
  PaintBox.Repaint;
end;

procedure TDrawingForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if DrawingEngine.IsDrawing THEN
     DrawingEngine.FinishLine(X,Y);
  Else
  DrawingEngine.StartLine(X, Y) ;
  PaintBox.Repaint;
end;

procedure TDrawingForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DrawingEngine.UpdateLine(X, Y);
  if DrawingEngine.IsDrawing then
    PaintBox.Repaint;
end;

procedure TDrawingForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DrawingEngine.FinishLine(X, Y);
  PaintBox.Repaint;
end;

procedure TDrawingForm.PaintBoxPaint(Sender: TObject);
var
  bmp: TBGRABitmap;
  ctx: TBGRACanvas2D;
begin
  bmp := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAWhite);
  try
    if Assigned(DrawingEngine.RulerBitmap) then
      bmp.PutImage(0, 0, DrawingEngine.RulerBitmap, dmDrawWithTransparency);
    ctx := bmp.Canvas2D;
    DrawingEngine.DrawAll(ctx);
    bmp.Draw(PaintBox.Canvas, 0, 0);
  finally
    bmp.Free;
  end;
end;

end.
