unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, BGRABitmap, BGRABitmapTypes;


type

  { TDrawingForm }

  TDrawingForm = class(TForm)
    Button1: TButton;
    lblCoords: TLabel;
    PaintBox: TPaintBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  end;
                                                   
  type TLine = record
    startPoint, EndPoint: TPoint;
  end;

var
  DrawingForm: TDrawingForm;
  Lines: array of TLine;
  IsDrawing: Boolean = False;
  TempStart, TempEnd: TPoint;
  Buffer: TBitmap;

implementation

{$R *.lfm}

{ TDrawingForm }

procedure TDrawingForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  Buffer := TBitmap.Create;
  Buffer.SetSize(PaintBox.Width, PaintBox.Height);
  Buffer.Canvas.FillRect(0, 0, Buffer.Width, Buffer.Height)

end;

procedure TDrawingForm.PaintBoxClick(Sender: TObject);
begin

end;

procedure TDrawingForm.Button1Click(Sender: TObject);
begin
     SetLength(Lines, 0); // Clear all stored lines
  PaintBox.Invalidate; // Trigger repaint

end;

procedure TDrawingForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    TempStart := Point(X, Y);
    TempEnd := TempStart;
    IsDrawing := True;
  end;
end;

procedure TDrawingForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
   lblCoords.Caption := Format('X: %d, Y: %d', [X, Y]);
  if IsDrawing then
  begin
    TempEnd := Point(X, Y);
    PaintBox.Invalidate; // triggers repaint to show preview
  end;
end;


procedure TDrawingForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if IsDrawing then
  begin
    SetLength(Lines, Length(Lines) + 1);
    Lines[High(Lines)].StartPoint := TempStart;
    Lines[High(Lines)].EndPoint := Point(X, Y);

    IsDrawing := False;
    PaintBox.Invalidate;
  end;
end;

procedure TDrawingForm.PaintBoxPaint(Sender: TObject);
const
  TickSpacing = 10;
  LongTickHeight = 20;
  ShortTickHeight = 5;
  LabelOffsetX = 5;
  LabelOffsetY = 35;
var
  i: Integer;
  canvasBottom: Integer;
  C: TCanvas;
begin
  C := PaintBox.Canvas;
  canvasBottom := PaintBox.Height - 1;

  // Set up canvas
  C.AntialiasingMode := amOn;
  C.Pen.Color := clBlack;
  C.Pen.Width := 2;
  C.Brush.Color := clWhite;
  C.FillRect(PaintBox.ClientRect);

  // Draw ellipse
  C.Ellipse(10, 10, 100, 100);

  // Draw stored lines
  for i := 0 to High(Lines) do
  begin
    C.MoveTo(Lines[i].StartPoint.X, Lines[i].StartPoint.Y);
    C.LineTo(Lines[i].EndPoint.X, Lines[i].EndPoint.Y);
  end;

  // Draw preview line if dragging
  if IsDrawing then
  begin
    C.Pen.Style := psDash;
    C.MoveTo(TempStart.X, TempStart.Y);
    C.LineTo(TempEnd.X, TempEnd.Y);
    C.Pen.Style := psSolid;
  end;

  // Draw horizontal ruler line
  C.Pen.Width := 1;
  C.MoveTo(0, canvasBottom);
  C.LineTo(PaintBox.Width, canvasBottom);

  // Draw tick marks and labels
  for i := 0 to PaintBox.Width div TickSpacing do
  begin
    C.MoveTo(i * TickSpacing, canvasBottom);

    if i mod 10 = 0 then
    begin
      C.LineTo(i * TickSpacing, canvasBottom - LongTickHeight);
      C.TextOut(i * TickSpacing + LabelOffsetX, canvasBottom - LabelOffsetY, IntToStr(i * TickSpacing));
    end
    else
      C.LineTo(i * TickSpacing, canvasBottom - ShortTickHeight);
  end;
end;


procedure TDrawingForm.FormDestroy(Sender: TObject);
begin
  Buffer.Free;
end;


end.

