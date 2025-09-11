unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, BGRABitmap, BGRABitmapTypes, BGRAPen;


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


const
  TickSpacing   = 10;
  LongTickHeight = 20;
  ShortTickHeight = 5;
  LabelOffsetX  = 5;
  LabelOffsetY  = 35;


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

procedure TDrawingForm.DrawStoredLines(bmp: TBGRABitmap);
var
  i: Integer;
begin
  for i := 0 to High(Lines) do
    bmp.DrawLineAntialias(
      Lines[i].StartPoint.X, Lines[i].StartPoint.Y,
      Lines[i].EndPoint.X, Lines[i].EndPoint.Y,
      BGRA(0, 0, 0), 2, True
    );
end;

procedure TDrawingForm.DrawPreviewLine(bmp: TBGRABitmap);
var
  pen: TBGRAPen;
begin
  if not IsDrawing then Exit;

  pen := TBGRAPen.Create;
  try
    pen.Width := 2;
    pen.Style := psDash;
    pen.Color := BGRA(0, 0, 0);
    bmp.DrawLine(TempStart.X, TempStart.Y, TempEnd.X, TempEnd.Y, pen);
  finally
    pen.Free;
  end;
end;

procedure TDrawingForm.DrawRuler(bmp: TBGRABitmap);
var
  i, canvasBottom: Integer;
begin
  canvasBottom := PaintBox.Height - 1;

  // Base line
  bmp.DrawLineAntialias(0, canvasBottom, PaintBox.Width, canvasBottom, BGRA(0, 0, 0), 1, True);

  // Tick marks and labels
  for i := 0 to PaintBox.Width div TickSpacing do
  begin
    if i mod 10 = 0 then
    begin
      bmp.DrawLineAntialias(i * TickSpacing, canvasBottom, i * TickSpacing, canvasBottom - LongTickHeight, BGRA(0, 0, 0), 1, True);
      bmp.TextOut(i * TickSpacing + LabelOffsetX, canvasBottom - LabelOffsetY, IntToStr(i * TickSpacing), 10, BGRA(0, 0, 0));
    end
    else
      bmp.DrawLineAntialias(i * TickSpacing, canvasBottom, i * TickSpacing, canvasBottom - ShortTickHeight, BGRA(0, 0, 0), 1, True);
  end;
end;

procedure TDrawingForm.PaintBoxPaint(Sender: TObject);
var
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(PaintBox.Width, PaintBox.Height, BGRAWhite);
  try
    // Draw base elements
    bmp.FillEllipseAntialias(55, 55, 45, 45, BGRA(255, 0, 0)); // Red circle

    // Modular drawing
    DrawStoredLines(bmp);
    DrawPreviewLine(bmp);
    DrawRuler(bmp);

    // Render to canvas
    bmp.Draw(PaintBox.Canvas, 0, 0, True);
  finally
    bmp.Free;
  end;
end;


procedure TDrawingForm.FormDestroy(Sender: TObject);
begin
  Buffer.Free;
end;


end.

