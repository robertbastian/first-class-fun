(* lab3/digits.p *)

var q: num;

proc search(k: num, n: num, avail: num->bool): num;

  var d: num, nn: num;

  proc avail1(x: num): bool;
  begin
    if x <> d then
      return avail(x)
    else
      return false
    end
  end;

begin
  if k = 10 then
    print n; newline
  else
    d := 1;
    while d < 10 do
      nn := 10 * n + d;
      if avail(d) and (nn mod k = 0) then
        q := search(k+1, nn, avail1)
      end;
      d := d+1
    end
  end;
  return 0
end;

proc all(x: num): bool;
begin
  return true
end;

begin
  q := search(1, 0, all)
end.

(*<<
 381654729
>>*)
