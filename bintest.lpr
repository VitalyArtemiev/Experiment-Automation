program bintest;

uses
  math;

var
  b:array [0..3] of byte;                        //90 254 110 0
  a: smallint;                                  //23294
  p: ^smallint;                                 //-422
begin
  {b[0]:= %01011010;
  b[1]:= %11111110;
  b[2]:= %01101110;
  b[3]:= %00000000;}
  b[0]:= %00100100;
  b[1]:= %00000001;
  b[2]:= %01101110;
  b[3]:= %00000000;
  p:= @b[0];
  a:= p^;
  writeln(single(a* power(2, b[2] - 124)));
  {writeln(, ' ',,' ', ,' ', );
  writeln(smallint(%0101101011111110));
  writeln(smallint(%1111111001011010)); }
  readln
end.

