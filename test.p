proc f(x: num, y: num): (num)->num;
  proc g(z: num): num;
  begin
    return (z+x)
  end;
begin
  print g(y); newline;
  return g
end;

begin
  print f(1,2)(5)
end.

(*<<
3
6
>>*)
