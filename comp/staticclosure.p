proc f(y: num): (num)->num;
  proc g(z: num): num;
  begin
    return (z+1)
  end;
begin
  print g(y); newline;
  return g
end;

begin
  print f(2)(5); newline;
end.

(*<<
 3
 6
>>*)
