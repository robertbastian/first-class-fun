proc f(x: num, y: num): (num)->num;
  var h: (num)->num;
  proc g(z: num): num;
  begin
    return (z+x)
  end;
begin
  print g(y); newline;
  h := g;
  return g
end;

begin
  print f(1,2)(5); newline;
end.

(*<<
 3
 6
 18
>>*)
