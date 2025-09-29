unit uDrawingEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRACanvas2D, Math;

type
  TPointD = record
    x, y: Double;
  end;

  TLine = record
    startPoint, endPoint: TPointD;
  end;

  { TDrawingEngine }

  TDrawingEngine = class
  private
    FRulerBitmap: TBGRABitmap;
    FScale: Integer;
    FIsDrawing: Boolean;
    FLineList: array of TLine;
    FCurrentLine: TLine;
    procedure DrawRuler(Ctx: TBGRACanvas2D; const AWidth, AOrigin, AScale: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateRulerBitmap(Width, Height: Integer);
    procedure StartLine(X, Y: Integer);
    procedure UpdateLine(X, Y: Integer);
    procedure FinishLine(X, Y: Integer);
    procedure DrawAll(Ctx: TBGRACanvas2D);
    property RulerBitmap: TBGRABitmap read FRulerBitmap;
    property IsDrawing: Boolean read FIsDrawing;
  end;

implementation

const
  cRulerHeight = 25;

{ TDrawingEngine }

constructor TDrawingEngine.Create;
begin
  FScale := 2;
  FRulerBitmap := nil;
  SetLength(FLineList, 0);
  FIsDrawing := False;
end;

destructor TDrawingEngine.Destroy;
begin
  if Assigned(FRulerBitmap) then FRulerBitmap.Free;
  inherited Destroy;
end;

procedure TDrawingEngine.UpdateRulerBitmap(Width, Height: Integer);
var
  ctx: TBGRACanvas2D;
begin
  if Assigned(FRulerBitmap) then FRulerBitmap.Free;
  FRulerBitmap := TBGRABitmap.Create(Width, Height, BGRAWhite);
  ctx := FRulerBitmap.Canvas2D;

  // Draw horizontal ruler at the top
  ctx.save();
  DrawRuler(ctx, Width, 0, FScale);
  ctx.restore;

  // Draw vertical ruler at the left
  ctx.save();
  ctx.translate(0, Height);
  ctx.rotate(-pi/2);
  DrawRuler(ctx, Height, 0, FScale);
  ctx.restore;
end;

procedure TDrawingEngine.DrawRuler(Ctx: TBGRACanvas2D; const AWidth, AOrigin, AScale: Integer);
const
  MinorTick = 8;
  MajorTick = 40;
var
  x, yMinor, yMajor: Integer;
  txt: string;
  txtW, txtH: Single;
begin
  // Fill background
  Ctx.save;
  Ctx.fillStyle(BGRAWhite);
  Ctx.fillRect(0, 0, AWidth, cRulerHeight);
  Ctx.restore;

  yMinor := 5;
  yMajor := 12;

  Ctx.lineWidth := 1;
  Ctx.font := '14px Tahoma';
  Ctx.textBaseline := 'top';
  Ctx.textAlign := 'center';

  for x := 0 to AWidth do
  begin
    if ((x - AOrigin) mod MinorTick = 0) then
    begin
      Ctx.beginPath;
      Ctx.moveTo(x, 0);
      Ctx.lineTo(x, yMinor);
      Ctx.stroke;
    end;
    if ((x - AOrigin) mod MajorTick = 0) then
    begin
      Ctx.beginPath;
      Ctx.moveTo(x, 0);
      Ctx.lineTo(x, yMajor);
      Ctx.stroke;

      txt := IntToStr(floor((x - AOrigin) / AScale));
      txtW := Ctx.measureText(txt).width;
      txtH := Ctx.measureText(txt).height;
      Ctx.fillStyle(BGRABlack);
      Ctx.fillText(txt, x, yMajor + 2);
    end;
  end;
end;

procedure TDrawingEngine.StartLine(X, Y: Integer);
begin
  FIsDrawing := True;
  FCurrentLine.startPoint.x := X;
  FCurrentLine.startPoint.y := Y;
  FCurrentLine.endPoint := FCurrentLine.startPoint;
end;

procedure TDrawingEngine.UpdateLine(X, Y: Integer);
begin
  if FIsDrawing then
  begin
    FCurrentLine.endPoint.x := X;
    FCurrentLine.endPoint.y := Y;
  end;
end;

procedure TDrawingEngine.FinishLine(X, Y: Integer);
var
  n: Integer;
begin
  if FIsDrawing then
  begin
    FCurrentLine.endPoint.x := X;
    FCurrentLine.endPoint.y := Y;
    n := Length(FLineList);
    SetLength(FLineList, n + 1);
    FLineList[n] := FCurrentLine;
    FIsDrawing := False;
  end;
end;

procedure TDrawingEngine.DrawAll(Ctx: TBGRACanvas2D);
var
  i: Integer;
begin
  // Draw all lines in blue
  Ctx.strokeStyle('blue');
  Ctx.lineWidth := 2;
  for i := 0 to High(FLineList) do
  begin
    Ctx.beginPath;
    Ctx.moveTo(FLineList[i].startPoint.x, FLineList[i].startPoint.y);
    Ctx.lineTo(FLineList[i].endPoint.x, FLineList[i].endPoint.y);
    Ctx.stroke;
  end;

  // Draw current line in red
  if FIsDrawing then
  begin
    Ctx.strokeStyle('red');
    Ctx.lineWidth := 2;
    Ctx.beginPath;
    Ctx.moveTo(FCurrentLine.startPoint.x, FCurrentLine.startPoint.y);
    Ctx.lineTo(FCurrentLine.endPoint.x, FCurrentLine.endPoint.y);
    Ctx.stroke;
  end;
end;

end.
