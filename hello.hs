main :: IO ()
main = do
  putStrLn "Hello, world!"
  putStrLn ("some odd numbers: " ++ show (filter odd [10 .. 20]))