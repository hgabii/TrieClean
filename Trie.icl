module Trie

import StdEnv, StdLib, GenEq

derive gEq Trie, Maybe

// ### Helper functions ###

stringToList :: String -> [Char]
stringToList str = [ char \\ char <-: str ]

getKeyFromChild :: (Char, Trie v) -> Char
getKeyFromChild (key, _) = key

getValueFromChild :: (Char, Trie v) -> Trie v
getValueFromChild (_, value) = value

keyExistInList :: Char [(Char, Trie v)] -> Maybe (Trie v)
keyExistInList _ [] = Nothing
keyExistInList key [x:xs]
	| key == (getKeyFromChild x) = Just (getValueFromChild x)
	| otherwise = keyExistInList key xs
	
insertPairIntoList :: (Char, Trie v) [(Char, Trie v)] -> [(Char, Trie v)]
insertPairIntoList element [] = [element]
insertPairIntoList element [x:xs]
	| getKeyFromChild element < getKeyFromChild x = [element] ++ [x:xs]
	| getKeyFromChild element > getKeyFromChild x = [x] ++ (insertPairIntoList element xs)
	| getKeyFromChild element == getKeyFromChild x = [element] ++ xs
	
changeValueOfNode :: v (Trie v) -> Trie v
changeValueOfNode insertValue (T _ childNodes) = T (Just insertValue) childNodes 

// ### End of helper functions ###

// ### Implementation of Trie ###

:: Trie v = T (Maybe v) [(Char, Trie v)]

emptyTrie :: Trie v
emptyTrie = T Nothing []

insertTrie :: String v (Trie v) -> (Trie v)
insertTrie keyStr insertValue trie = insertTrieRecursive (stringToList keyStr) insertValue trie

insertTrieRecursive :: [Char] v (Trie v) -> (Trie v)
insertTrieRecursive [] insertValue (T Nothing []) = 
	T (Just insertValue) []
insertTrieRecursive [x:xs] insertValue (T Nothing []) =
	T Nothing [(x, insertTrieRecursive xs insertValue emptyTrie)]
insertTrieRecursive [x] insertValue (T currentValue childNodes)
	| isNothing (keyExistInList x childNodes) = T currentValue (insertPairIntoList (x, T (Just insertValue) []) childNodes) 
	| otherwise = T currentValue (
						insertPairIntoList ( 
							x, 
							changeValueOfNode insertValue (fromJust (keyExistInList x childNodes))  
							) 
						childNodes)
insertTrieRecursive [x:xs] insertValue (T currentValue childNodes)
	| isNothing (keyExistInList x childNodes) = 
		T currentValue (
			insertPairIntoList 
				(x, insertTrieRecursive xs insertValue emptyTrie)
				childNodes
			)
	| otherwise = 
		T currentValue (
			insertPairIntoList
				(x, insertTrieRecursive xs insertValue (fromJust (keyExistInList x childNodes)) )
				childNodes
			)
// ----------


buildTrie :: [(String, v)] -> Trie v
buildTrie _ = abort "undefined"

lookupTrie :: String (Trie v) -> Maybe v
lookupTrie _ _ = abort "undefined"

class Functor f where
  fmap :: (a -> b) (f a) -> (f b)

//instance Functor Trie where
//  fmap = abort "undefined"

test :: [Int] -> [Int]
test [] = [0]
test [x] = [x]
test [x:xs] = xs

testStr :: String -> Int
testStr "" = 0
testStr s = 1

Start = test_insertTrie

test_insertTrie :: [Bool]
test_insertTrie =
    [ insertTrie ""    0 emptyTrie
	=== T (Just 0) []
    , insertTrie "a"   1 emptyTrie
	=== T Nothing [('a', T (Just 1) [])]

    , insertTrie "b"   2 (insertTrie "a"   1 emptyTrie)
	=== T Nothing [('a', T (Just 1) []), ('b', T (Just 2) [])]
    , insertTrie "a"   1 (insertTrie "b"   2 emptyTrie)
	=== T Nothing [('a', T (Just 1) []), ('b', T (Just 2) [])]

    , insertTrie "ab"  3 (insertTrie "a"   1 emptyTrie)
	=== T Nothing [('a', T (Just 1) [('b', T (Just 3) [])])]
    , insertTrie "abc" 4 emptyTrie
	=== T Nothing [('a', T Nothing [('b', T Nothing [('c', T (Just 4) [])])])]
    ]