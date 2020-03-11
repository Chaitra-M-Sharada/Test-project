main :: IO ()
main =
  print
    [ floor (0.01 + (1 / p ** n + p ** n) / sqrt 5)
    | let p = (1 + sqrt 5) / 2 
    , n <- [0 .. 42] ]
fib x =
  if x < 1
    then 0
    else if x < 2
           then 1
           else fib (x - 1) + fib (x - 2)