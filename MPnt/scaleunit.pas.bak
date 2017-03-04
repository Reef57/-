unit ScaleUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, math;

type
  TDoublePoint = record
    X: Double;
    Y: Double;
  end;

type
  TDoubleRect = record
    X1: Double;
    Y1: Double;
    X2: Double;
    Y2: Double;
end;

function DoublePoint(X, Y: Double): TDoublePoint;
function ScreenToWorld(P: TPoint): TDoublePoint;
function WorldToScreen(ARect: TDoubleRect): TRect;
function WorldToScreen(P: TDoublePoint): TPoint;
function DoubleRect(A, B: TDoublePoint): TDoubleRect;
procedure RectangleZoom(Height, Width: Integer; AMin, AMax: TDoublePoint);
procedure PointZoom(APoint: TDoublePoint;RButton: Boolean;Height, Width: Integer);
procedure CenterZoom(AWidth, AHeight: integer; AoldZoom: Double);
procedure FindMinMax(APoint: TPoint);

var
  Zoom: integer = 100;
  Offset: TPoint;
  MinPoint : TPoint;
  MaxPoint: TPoint;
  InvalidateHandler: procedure of Object;

implementation

function DoublePoint(X, Y: Double): TDoublePoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure CenterZoom(AWidth, AHeight: integer; AoldZoom: Double);
begin
  if Zoom > AoldZoom then
    begin
      Offset.x := Offset.x + round(AWidth * (Zoom - AoldZoom) / 200);
      Offset.y := Offset.y + round(AHeight * (Zoom - AoldZoom) / 200);
    end
  else
  begin
      Offset.x := Offset.x - round(AWidth * (AoldZoom - Zoom) / 200);
      Offset.y := Offset.y - round(AHeight * (AoldZoom-Zoom) / 200);
  end;
end;

function WorldToScreen(ARect: TDoubleRect): TRect;
begin
  Result.TopLeft := WorldToScreen(DoublePoint(ARect.X1, ARect.Y1));
  Result.BottomRight := WorldToScreen(DoublePoint(ARect.X2, ARect.Y2));
end;

function WorldToScreen(P: TDoublePoint): TPoint;
begin
  Result.X := round(P.X * Zoom / 100) - Offset.X;
  Result.Y := round(P.Y * Zoom / 100) - Offset.Y;
end;

function ScreenToWorld(P: TPoint): TDoublePoint;
begin
  Result.X := (P.X + Offset.X) / Zoom * 100;
  Result.Y := (P.Y + Offset.Y) / Zoom * 100;
end;

function DoubleRect(A, B: TDoublePoint): TDoubleRect;
begin
  Result.X1 := A.X;
  Result.X2 := B.X;
  Result.Y1 := A.Y;
  Result.Y2 := B.Y;
end;

procedure RectangleZoom(Height, Width: Integer; AMin, AMax: TDoublePoint);
var
  oldzoom: Double;
  DPoint: TDoublePoint;
begin
  if AMin.X > AMax.X then
    begin
      DPoint := AMax;
      AMax := AMin;
      AMin := DPoint;
    end;
  oldzoom := Zoom;
  if (AMin.X = AMax.X) and (AMin.Y = AMax.Y) then
    exit;
  Zoom := round(Min(Height / (AMax.Y - AMin.Y) * 100,
    Width / (AMax.X - AMin.X) * 100)) - 1;
  if Zoom > 1000 then
    Zoom := 1000;
  if (Zoom <> oldzoom) and (Zoom <> 100)  then
    begin
      Offset.x := round(AMin.X * Zoom / 100) - 5;
      Offset.y := round(AMin.Y * Zoom / 100) - 5;
    end;
  InvalidateHandler;
end;

procedure PointZoom(APoint: TDoublePoint;RButton: Boolean;Height, Width: Integer);
begin
  if RButton then
    Zoom := round(Zoom / 2)
  else
    Zoom := Zoom * 2;
  if Zoom > 1000 then
    Zoom := 1000;
  if Zoom < 1 then
    Zoom := 1;
  Offset.x := Offset.x + (WorldToScreen(APoint).X - round(APoint.X));
  Offset.y := Offset.y + (WorldToScreen(APoint).y - round(APoint.Y));
  InvalidateHandler;
end;

procedure FindMinMax(APoint: TPoint);
begin
  if APoint.x > MaxPoint.x then
    MaxPoint.x := APoint.x;
  if APoint.y > MaxPoint.y then
    MaxPoint.y := APoint.y;
  if APoint.x < MinPoint.x then
    MinPoint.x := APoint.x;
  if APoint.y < MinPoint.y then
    MinPoint.y := APoint.y;
  InvalidateHandler;
end;

end.

