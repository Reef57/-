unit main;

{$mode objfpc}{$H+}

interface

var
SaveNum, SaveOp: string;
is_error, is_op: boolean;

procedure Number(var my_input, Num: string);
uses
  Classes, SysUtils;

implementation

procedure Number(var my_input, CurOp: string);
begin
  if (is_error = False) then
  begin
    if (my_input = '0') or (is_op = True) then
    begin
      my_input := '';
    end;
    my_input := my_input + Num;
    is_op := False;
  end;
end;
end.

