import Test.Tasty.Bench

fibo :: Int -> Integer
fibo n
    | n < 2 = toInteger n
    | otherwise = fibo (n - 1) + fibo (n - 2)

main :: IO ()
main = defaultMain
  [ bgroup "Fibonacci numbers"
    [ bench "fifth"     $ nf fibo  5
    , bench "tenth"     $ nf fibo 10
    , bench "twentieth" $ nf fibo 20
    ]
  ]