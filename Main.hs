factor :: Integer -> [Integer]

factor 1 = []
factor n = let divisores = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisores then n else head divisores
              in (prime :) $ factor $ div n prime

main = do
  print (factor 12333)
