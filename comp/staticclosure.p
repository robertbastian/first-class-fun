var k: num->num;

proc f(y: num): num->num;
  proc g(z: num): num;
  begin
    return (z+1)
  end;
begin
  print g(y); newline;
  return g
end;

begin
  k := f(2);
  print k(5); newline;
end.

(*<<
 3
 6
>>*)
