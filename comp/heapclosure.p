var h: (num,num)->num->num,
    k: num->num,
    j: num;

proc f(x: num, y: num): num->num;
  proc g(z: num): num;
  begin
    return (z+x)
  end;
begin
  print g(y); newline;
  return g
end;

begin
  h := f;
  k := h(1,2);
  j := k(5);
  print j; newline;
end.

(*<<
 3
 6
>>*)
