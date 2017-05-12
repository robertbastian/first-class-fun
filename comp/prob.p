var expectation: num,
    variance: num;

proc random_average(f: num->num, genr: ()->num, samples: num): num;
var sum: num, i: num;
begin
  while (i < samples) do
    sum := sum + f(genr());
    i := i + 1;
  end;
  return (sum div samples);
end;

proc id(x:num): num; 
  begin return 10*x end;

proc squareddiff(mean: num): num->num;
  proc a(x:num): num; 
    var diff: num;
    begin diff := 10*x-mean; return diff*diff div 10 end;
  begin return a; end;

proc die(): num; begin return rand(6)+1 end;

begin
  expectation := random_average(id, die, 10000);
  variance := random_average(squareddiff(expectation), die, 10000);
  print_f(expectation, 1); newline();
  print_f(variance, 1); newline();
end.

(*<<
 3.5
 2.8
>>*)
