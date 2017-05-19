main = putStr $ show $ fib 33 ()

fib x = if x < 2 then (\_ -> x) else (\_ -> f1() + f2())
  where f1 = fib (x-1)
        f2 = fib (x-2)
