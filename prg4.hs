
hanoi :: Integer -> a -> a -> a -> [(a, a)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
hanoiIO n = mapM_ f $ hanoi n 1 2 3 where
  f (x,y) = putStrLn $ "Move " ++ show x ++ " to " ++ show y

hanoiM :: Integer -> IO ()
hanoiM n = hanoiM' n 1 2 3 where
  hanoiM' 0 _ _ _ = return ()
  hanoiM' n a b c = do
    hanoiM' (n-1) a c b
    putStrLn $ "Move " ++ show a ++ " to " ++ show b
    hanoiM' (n-1) c b a

