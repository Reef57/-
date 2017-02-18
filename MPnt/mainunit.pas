unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ColorBox, ValEdit, PairSplitter, Spin, Menus, ToolUnit,
  FiguresUnit, Types, ScaleUnit;

type

  { TPaint }

  TPaint = class(TForm)
    clWhite: TPanel;
    clBlue: TPanel;
    clAqua: TPanel;
    clGreen: TPanel;
    clRed: TPanel;
    FillColor: TPanel;
    clBlack: TPanel;
    ZoomBtn: TBitBtn;
    PenColor: TPanel;
    Line: TBitBtn;
    Polyline: TBitBtn;
    PaintBox: TPaintBox;
    Rectangle: TBitBtn;
    Ellipse: TBitBtn;
    PolyGon: TBitBtn;
    ZoomEdit: TSpinEdit;
    WidthEdit: TSpinEdit;
    ToolPanel: TPanel;
    ColorPanel: TPanel;
    procedure clBlackMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ChangeTool(Sender: TObject);
    procedure WidthEditChange(Sender: TObject);
    procedure ZoomEditChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Paint: TPaint;

implementation

var
  CurTool: TTool;
  Colors : TCellColor;
  FigManager: TFigManager;

{$R *.lfm}

{ TPaint }

procedure TPaint.FormCreate(Sender: TObject);
var
  Tool: TTool;
begin
  for Tool in Tools do
    Tool.FigManager := FigManager;
end;

procedure TPaint.clBlackMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Colors: TCellColor;
begin
  if Button = mbLeft then
     PenColor.Color := (Sender as TPanel).Color;
  if Button = mbRight then
     FillColor.Color := (Sender as TPanel).Color;
  Colors.Pen := PenColor.Color;
  Colors.Fill := FillColor.Color;
  CurTool.SetColor(Colors);
end;

procedure TPaint.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  CurPoint: TPoint;
begin
  CurPoint.X := X;
  CurPoint.Y := Y;
  CurTool.MouseDown(Button, Shift, CurPoint);
  PaintBox.Invalidate;
end;

procedure TPaint.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  CurPoint: TPoint;
begin
  CurPoint.X := X;
  CurPoint.Y := Y;
  CurTool.MouseMove(Shift, CurPoint);
  PaintBox.Invalidate;
end;

procedure TPaint.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  RButton: boolean;
  CurPoint: TPoint;
begin
  if Button = mbLeft then
     RButton := false;
  if Button = mbRight then
     RButton := true;
  PBoxHeight := PaintBox.Height;
  PBoxWidth := PaintBox.Width;
  CurPoint.X := X;
  CurPoint.Y := Y;
  CurTool.MouseUp(Shift, CurPoint, RButton);
  ZoomEdit.Text := IntToStr(round(Zoom));
end;

procedure TPaint.PaintBoxPaint(Sender: TObject);
begin
  FigManager.Draw(PaintBox.Canvas);
end;

procedure TPaint.ChangeTool(Sender: TObject);
var
   Colors :TCellColor;
 begin
 Colors.Pen := PenColor.Color;
 Colors.Fill := FillColor.Color;
 CurTool := Tools[(Sender as TBitBtn).Tag];
 CurTool.SetColor(Colors);
 CurTool.SetWidth(WidthEdit.Value);
end;

procedure TPaint.WidthEditChange(Sender: TObject);
begin
  CurTool.SetWidth(WidthEdit.Value);
end;

procedure TPaint.ZoomEditChange(Sender: TObject);
var
  oldZoom: double;
begin
  oldZoom := Zoom;
  Zoom := ZoomEdit.Value;
  CenterZoom(PaintBox.Width, PaintBox.Height, oldZoom);
end;

initialization
  FigManager := TFigManager.Create;
  CurTool := Tools[0];
end.



