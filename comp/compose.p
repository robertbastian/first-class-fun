(* lab3/compose.p *)

var p: num->num;

proc compose(f: num->num, g: num->num): num->num;
  proc fg(x: num): num;
  begin
    return f(g(x))
  end;
begin
  return fg
end;

proc add2(x: num): num; begin return x+2 end;
proc square(x: num): num; begin return x * x end;

begin
  p := compose(square, add2);
  print p(2); newline
end.

(*<<
 16
>>*)
