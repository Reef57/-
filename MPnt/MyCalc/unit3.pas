unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, main;

type

  { TMyCalc }

  TMyCalc = class(TForm)
    Zero: TButton;
    Nine: TButton;
    C: TButton;
    CE: TButton;
    Backspace: TButton;
    MC: TButton;
    MR: TButton;
    MS: TButton;
    M_plus: TButton;
    Plus_minus: TButton;
    Divide: TButton;
    One: TButton;
    Multiply: TButton;
    Minus: TButton;
    Dot: TButton;
    Plus: TButton;
    M_minus: TButton;
    Square_root: TButton;
    Mod_div: TButton;
    Onediv: TButton;
    Equals: TButton;
    Two: TButton;
    Five: TButton;
    Four: TButton;
    Seven: TButton;
    Eight: TButton;
    Three: TButton;
    Six: TButton;
    MInput: TEdit;
    MOutput: TEdit;
    procedure Button_Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MyCalc: TMyCalc;

implementation

{$R *.lfm}

{ TMyCalc }

procedure TMyCalc.Button_Click(Sender: TObject);
var
  myinput, myoutput, CurOp: string;
begin
  my_input := MInput.Text;
  my_output := MOutput.Text;
  CurOp := (Sender as TButton).Caption;
  case CurOp of
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9': Number(my_input, CurOp);
  end;
  MOutput.Text := my_output;
  MInput.Text := my_input;
end;

end.

