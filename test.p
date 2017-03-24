proc f(x,y);
  proc g(z);
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
