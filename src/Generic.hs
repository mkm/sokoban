module Generic where

findGroups p [] = []
findGroups p (x:xs) = insert $ findGroups p xs
    where
      insert [] = [[x]]
      insert (g@(y:_):gs)
          | p x y = (x : g) : gs
          | otherwise = g : insert gs

applyWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
applyWhere _ _ [] = []
applyWhere p f (x:xs) = y : ys
    where
      y = if p x then f x else x
      ys = applyWhere p f xs

