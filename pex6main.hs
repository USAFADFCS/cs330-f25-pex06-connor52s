-- pex6.hs 
-- unKnot Haskell

-- name: Connor Song

{- DOCUMENTATION:
Copied my listLength helper function from HW 6. This is used in detectTypeIKnot, and detectTypeIIKnot
I used Gavin Smith's tests that he sent in the Teams to help test my code
-}

-- most important function
-- detect type of knot, then execute that knot untangle
-- executing knot untangle should remove the applicable tuples 
-- doing so ^ should be recursive until tripCode is null
-- or until none of the detects go off, in which case you can't untangle the string
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | detectTypeIKnot tripCode = unKnot (typeIKnot tripCode)
   | detectTypeIIKnot tripCode 
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

-- only called if detected
-- removes first two tuples
-- needs an otherwise, so just return tripCode as is
typeIKnot :: [(Char, Char)] -> [(Char, Char)]
typeIKnot tripCode =
   tail (tail (tripCode))


typeIIKnot :: [(Char, Char)] -> [(Char, Char)]
typeIIKnot tripCode
   | 
   | otherwise = tripcode

-- detects if there's a typeIKnot
-- if so, then unKnot should execute typeIKnot and remove that knot
-- if list length is less than 2 (covers null list) -> False
detectTypeIKnot :: [(Char,Char)] -> Bool
detectTypeIKnot tripCode
   | listLength tripCode < 2 = False
   | EQ == compare (fst (head tripCode)) (fst (head (tail tripCode))) = True
   | otherwise = False

-- detects if there's a typeIIKnot
-- if so, then unKnot should execute typeIIKnot and remove that knot
-- if list length is less than 4 -> False
detectTypeIIKnot :: [(Char,Char)] -> Bool
detectTypeIIKnot tripCode
   | listLength tripCode < 4 = False
   | EQ == compare (fst (head tripCode)) (fst (head (tail tripCode))) = True

-- helper function cuz len not authorized
listLength [] = 0
listLength (x:xs) = 1 + listLength xs


main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)
   -- t05 from Gavin Smith
   let t05 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t05 - tripcode: " )
   print(t05)
   print("   result:" ++ unKnot t05)  
