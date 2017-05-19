proc fib(x: num): ()->num;
  var fib_1: ()-> num, fib_2: ()-> num;
  proc add(): num; begin; return fib_1()+fib_2() end;
  proc const(): num; begin; return x end;
  begin
    if x < 2 then return const end;
    fib_1 := fib(x-1);
    fib_2 := fib(x-2);
    return add;
  end;

begin
  print(fib(33)()); newline();
end.

(*<<
 55
>>*)
