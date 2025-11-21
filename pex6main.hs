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
   | detectTypeIIKnot tripCode = unKnot (typeIIKnot tripCode)
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)


-- only called if detected
-- removes first two tuples
typeIKnot :: [(Char, Char)] -> [(Char, Char)]
typeIKnot tripCode =
   tail (tail (tripCode))


-- only called if detected
-- removes first two tuples
-- also removes second set of tuples using helpers
typeIIKnot :: [(Char, Char)] -> [(Char, Char)]
typeIIKnot tripCode =
   typeIIKnotHelper tripCode


-- helper for typeIIKnot
-- removes 2nd set of tuples found
-- takes first part of list before 2nd set found location
-- takes second part of list after 2nd set found location
-- combines those two lists and returns that
-- no use of lets so super long line of code
typeIIKnotHelper :: [(Char, Char)] -> [(Char, Char)]
typeIIKnotHelper tripCode =
   (take (typeIIKnotCounter (fst (head tripCode)) (fst (head (tail tripCode))) (tail (tail tripCode))) tripCode) ++ (drop ((typeIIKnotCounter (fst (head tripCode)) (fst (head (tail tripCode))) (tail (tail tripCode))) + 2) tripCode)


-- helper for typeIIKnotHelper (and by extension typeIIKnot)
-- returns location of the second pair of tuples
-- then have typeIIKnot remove the first pair of tuples from tripCode
-- gonna have to remove the first x and last y tuples before and after
typeIIKnotCounter :: Char -> Char -> [(Char,Char)] -> Int
typeIIKnotCounter a b tripCode
   | (a == (fst (head tripCode))) && (b == fst (head (tail tripCode))) ||
     (b == (fst (head tripCode))) && (a == fst (head (tail tripCode))) = 0
   | otherwise = 1 + typeIIKnotCounter a b (tail tripCode)


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
-- checks that second part of first two tuples are the same ('u' or 'o')
   -- and needs to find that same first part of first two tuples adjacent somewhere
      -- ^ uses helper function
detectTypeIIKnot :: [(Char,Char)] -> Bool
detectTypeIIKnot tripCode
   | listLength tripCode < 4 = False
   | EQ == compare (snd (head tripCode)) (snd (head (tail tripCode))) &&
     detectTypeIIKnotHelper (fst (head tripCode)) (fst (head (tail tripCode))) (tail (tail tripCode)) = True
   | otherwise = False
     

-- helper function for detectTypeIIKnot
-- recursive
-- if list length is less than 2 -> False
-- finds where first part of first two tuples are adjacent
-- Not worrying about checking if it flips cuz if a string goes over it should go 
   -- under at some point I think??
-- copy into typeIIKnot and instead of returning Bool, return the tripCode without
   -- the second pair of tuples
   -- then have typeIIKnot remove the first pair of tuples from tripCode
   -- gonna have to remove the first x and last y tuples before and after
   -- have another "helper" count the location of the adjacent tuples for that
detectTypeIIKnotHelper :: Char -> Char -> [(Char,Char)] -> Bool
detectTypeIIKnotHelper a b tripCode
   | listLength tripCode < 2 = False 
   | (a == (fst (head tripCode))) && (b == fst (head (tail tripCode))) ||
     (b == (fst (head tripCode))) && (a == fst (head (tail tripCode))) = True
   | otherwise = detectTypeIIKnotHelper a b (tail tripCode)


-- helper function cuz len not authorized
listLength [] = 0
listLength (x:xs) = 1 + listLength xs


main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)
