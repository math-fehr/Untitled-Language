module Utils where

-- Optionally get a the missing value of a range
-- Do not check if there is values outside the range
missingInRange :: [Int] -> Int -> Maybe Int
missingInRange _ 0 = Nothing
missingInRange l maxi =
  if elem (maxi - 1) l
    then missingInRange l (maxi - 1)
    else Just $ maxi - 1

-- Optionally get a duplicate value in a list
duplicate :: Eq a => [a] -> Maybe a
duplicate [] = Nothing
duplicate (x:l) =
  if elem x l
    then Just x
    else duplicate l

percolateMaybe :: Monad m => Maybe (m a) -> m (Maybe a)
percolateMaybe Nothing = return Nothing
percolateMaybe (Just x) = x >>= return . Just

splitLast :: [a] -> ([a], a)
splitLast [x] = ([], x)
splitLast (a:b) =
  let (hd, tl) = splitLast b
   in (a : hd, tl)
