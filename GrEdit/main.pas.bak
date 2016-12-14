unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ComCtrls, ColorBox, Spin, figures, LCLType, LCLIntf,
  Clipbrd, fpcanvas;

type

  { TGEditor }

  TGEditor = class(TForm)
    EllipseButton: TBitBtn;
    PolygonButton: TBitBtn;
    LineButton: TBitBtn;
    PolylineButton: TBitBtn;
    MPolygonButton: TBitBtn;
    RectangleButton: TBitBtn;
    FillCheckBox: TCheckBox;
    FillColorButton: TColorButton;
    LineColorButton: TColorButton;
    PolygonSidesEdit: TEdit;
    ScrollBox: TScrollBox;
    WidthLabel: TLabel;
    UndoButton: TButton;
    ClearButton: TButton;
    PaintBox: TPaintBox;
    LineWidthBar: TTrackBar;
    procedure ChangeBrushStyle(Sender: TObject);
    procedure ChangeFillColor(Sender: TObject);
    procedure ChangeLineColor(Sender: TObject);
    procedure ChangeLineWidth(Sender: TObject);
    procedure Clear(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PolygonSidesChange(Sender: TObject);
    procedure ToolsClick(Sender: TObject);
    procedure Undo(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  GEditor: TGEditor;
  MyColor, MyBrushColor: TColor;
  MyBrushStyle: TFPBrushStyle;
  IsClick: boolean;
  CurTool, TWidth, SidesNum: integer;
implementation

{$R *.lfm}

{ TGEditor }

procedure TGEditor.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CurPoint: TPoint;
begin
  IsClick := True;

  setlength(FiguresArray, length(FiguresArray) + 1);
  CurPoint := Point(X, Y);
  case CurTool of
  0: FiguresArray[high(FiguresArray)] := TRectangle.Create(CurPoint);
  1: FiguresArray[high(FiguresArray)] := TEllipse.Create(CurPoint);
  2: FiguresArray[high(FiguresArray)] := TLine.Create(CurPoint);
  3: FiguresArray[high(FiguresArray)] := TPolygon.PolygonCreate(CurPoint, SidesNum);
  4: FiguresArray[high(FiguresArray)] := TPolyline.PolylineCreate(CurPoint, SidesNum);
  5: FiguresArray[high(FiguresArray)] := TModifiedPolygon.Create(CurPoint);
  end;
  FiguresArray[high(FiguresArray)].FWidth := TWidth;
  FiguresArray[high(FiguresArray)].FBrush := MyBrushColor;
  FiguresArray[high(FiguresArray)].FColor := MyColor;
  FiguresArray[high(FiguresArray)].FBrushStyle := MyBrushStyle;
  PaintBox.Invalidate;
  FiguresArray[high(FiguresArray)].AddPoint(CurPoint);
end;

procedure TGEditor.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  CurPoint: TPoint;
begin

  if IsClick = True then
  begin
    CurPoint := Point(X, Y);
    PaintBox.Invalidate;
    FiguresArray[high(FiguresArray)].AddPoint(CurPoint);
  end;
end;

procedure TGEditor.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsClick := False;
end;

procedure TGEditor.PaintBoxPaint(Sender: TObject);
var
  figr: TFigure;
begin
  for figr in FiguresArray do begin
    figr.Draw(PaintBox.Canvas);
  end;
end;

procedure TGEditor.PolygonSidesChange(Sender: TObject);
var
  EditNum, IsError: integer;
  EditStr: string;
begin
  EditStr := PolygonSidesEdit.Text;
  val(EditStr, EditNum, IsError);
  if IsError = 0 then
  begin
    if EditNum >= 3 then
    begin
      SidesNum := EditNum;
    end
    else ShowMessage('Polygon sides number must be >=3');
  end
  else
  begin
    ShowMessage('Polygon sides number must be >=3');
  end;
  if EditNum >= 250 then
  begin
    ShowMessage('Max- 250 sides');
  end;
end;

procedure TGEditor.ToolsClick(Sender: TObject);
begin
  CurTool := (Sender as TBitBtn).Tag;
end;

procedure TGEditor.Undo(Sender: TObject);
begin
  if length(FiguresArray) <> 0 then
  begin
    setlength(FiguresArray, length(FiguresArray) - 1);
    PaintBox.Invalidate;
  end
  else exit;
end;

procedure TGEditor.Clear(Sender: TObject);
begin
  setlength(FiguresArray, 0);
  PaintBox.Invalidate;
end;

procedure TGEditor.ChangeLineWidth(Sender: TObject);
begin
  TWidth := LineWidthBar.Position;
  PaintBox.Invalidate;
end;

procedure TGEditor.ChangeLineColor(Sender: TObject);
begin
  MyColor := LineColorButton.ButtonColor;
end;

procedure TGEditor.ChangeFillColor(Sender: TObject);
begin
  MyBrushColor := FillColorButton.ButtonColor;
end;

procedure TGEditor.ChangeBrushStyle(Sender: TObject);
begin
  if MyBrushStyle = bsClear then MyBrushStyle := bsSolid
  else MyBrushStyle := bsClear;
end;

initialization
SidesNum := 4;
MyBrushColor := clWhite;
MyBrushStyle := bsClear;
end.

