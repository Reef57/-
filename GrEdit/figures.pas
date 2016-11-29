unit figures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpcanvas;

type
  TFigure = class
  protected
    FPoints: array of TPoint;
   // tl, br: TPoint;   //tl is a top left point, br is a bottom right point
  public
    FColor, FBrush: TColor;
    FBrushStyle: TFPBrushStyle;
    FWidth: integer;
    procedure Draw(MyCanvas: TCanvas); virtual;
    procedure AddPoint(CurPoint: TPoint);
    constructor Create(CurPoint: TPoint);
  end;

  TRectangle = class(TFigure)
  public
    procedure Draw(MyCanvas: TCanvas); override;
  end;

  TEllipse = class(TFigure)
  public
    procedure Draw(MyCanvas: TCanvas); override;
  end;

  TLine = class(TFigure)
  public
    procedure Draw(MyCanvas: TCanvas); override;
  end;

  TPolygon = class(TFigure)
  public
    PSidesNum: integer;
    procedure Draw(MyCanvas: TCanvas); override;
    constructor PolygonCreate(CurPoint: TPoint; SidesNum: integer);
  end;

  TPolyline = class(TFigure)
    public
      PSidesNum: integer;
      procedure Draw(MyCanvas: TCanvas); override;
      constructor PolylineCreate(CurPoint: TPoint; SidesNum: integer);
  end;

var
  FiguresArray: array of TFigure;

implementation

//Add top left point(first) (for all figures)
constructor TFigure.Create(CurPoint: TPoint);
begin
  setlength(FPoints, 2);
  FPoints[0] := CurPoint;
end;

//Add top left point to array(TPolygon)
constructor TPolygon.PolygonCreate(CurPoint: TPoint; SidesNum: integer);
begin
  setlength(FPoints, 2);
  PSidesNum := SidesNum;
  FPoints[0] := CurPoint;
end;

//Add top left point to array(TPolyline)
constructor TPolyline.PolylineCreate(CurPoint: TPoint; SidesNum: integer);
begin
  setlength(FPoints, 2);
  PSidesNum := SidesNum;
  FPoints[0] := CurPoint;
end;

//Add bottom right(second) point (for all figures)
procedure TFigure.AddPoint(CurPoint: TPoint);
begin
  FPoints[1] := CurPoint;
end;

//Draw function for Rectangles
procedure TRectangle.Draw(MyCanvas: TCanvas);
begin
  MyCanvas.Pen.Width := FWidth;
  MyCanvas.Pen.Color := FColor;
  MyCanvas.Brush.Color := FBrush;
  MyCanvas.Brush.Style := FBrushStyle;
  MyCanvas.Rectangle(FPoints[0].x, FPoints[0].y, FPoints[1].x, FPoints[1].y);
end;

//Draw function for Ellipses
procedure TEllipse.Draw(MyCanvas: TCanvas);
begin
  MyCanvas.Pen.Width := FWidth;
  MyCanvas.Pen.Color := FColor;
  MyCanvas.Brush.Color := FBrush;
  MyCanvas.Brush.Style := FBrushStyle;
  MyCanvas.Ellipse(FPoints[0].x, FPoints[0].y, FPoints[1].x, FPoints[1].y);
end;

//Draw function for Lines
procedure TLine.Draw(MyCanvas: TCanvas);
begin
  MyCanvas.Pen.Width := FWidth;
  MyCanvas.Pen.Color := FColor;
  MyCanvas.Brush.Color := FBrush;
  MyCanvas.Brush.Style := FBrushStyle;
  MyCanvas.Line(FPoints[0].x, FPoints[0].y, FPoints[1].x, FPoints[1].y);
end;

//Draw function for Polygon
procedure TPolygon.Draw(MyCanvas: TCanvas);
var
  i, r, k, z: integer;
  PolygonPoints: array of TPoint;
begin
  k := 0;
  z := 0;
  k := 360 div PSidesNum;
  r := round(sqrt(sqr(abs(FPoints[0].X - FPoints[1].X)) + sqr(abs(FPoints[0].Y - FPoints[1].Y))));
  setlength(PolygonPoints, PSidesNum);
  for i := 0 to PSidesNum - 1 do
  begin
    PolygonPoints[i].X := Round(FPoints[0].X + cos(z / 180 * pi) * r);
    PolygonPoints[i].Y := Round(FPoints[0].Y - sin(z / 180 * pi) * r);
    z := z + k;
  end;
  MyCanvas.Pen.Width := FWidth;
  MyCanvas.Pen.Color := FColor;
  MyCanvas.Brush.Color := FBrush;
  MyCanvas.Brush.Style := FBrushStyle;
  MyCanvas.Polygon(PolygonPoints);
end;

//Draw function for Polyline
procedure TPolyline.Draw(MyCanvas: TCanvas);
begin
  MyCanvas.Pen.Width := FWidth;
  MyCanvas.Pen.Color := FColor;
  MyCanvas.Brush.Color := FBrush;
  MyCanvas.Brush.Style := FBrushStyle;
  MyCanvas.Polyline(FPoints);
end;

//Empty draw function for main class
procedure TFigure.Draw(MyCanvas: TCanvas);
begin
end;

end.
