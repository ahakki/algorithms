module List where

-- this compares each element of the list with the element
-- after it, and if the condition (cond) is met, applies an
-- action to it.
condFoldl cond action (x:y:ys)
    | cond x y =
        action x : y : condFoldl cond action ys
    | otherwise =
        x : condFoldl cond action (y : ys)

condFoldl _ _ [x] =
    [x]

condFoldl _ _ _ =
    []
