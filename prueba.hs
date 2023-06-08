findAllDuplicates :: Eq a => [a] -> [a]
findAllDuplicates xs = go xs [] []
  where
    go [] _ result = reverse result
    go (x:xs) seen result
      | x `elem` seen = go xs seen (insert x result)
      | otherwise = go xs (x:seen) result

    insert x xs = x:xs

main :: IO ()
main = do
  let allNames = ["x", "x", "y", "y", "y"]
      duplicatedNames = findAllDuplicates allNames
  putStrLn "Lista original: "
  print allNames
  putStrLn "Nombres duplicados: "
  print duplicatedNames