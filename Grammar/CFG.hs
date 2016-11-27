module CFG (
emptySymbol, eofSymbol,
fromString, nonterminal, terminal,
source, result,
nonterminals, terminals, rules, starter,
rulesFrom, rulesTo
) where

import Data.List
import qualified Data.Set as Set
import Data.Char

data Symbol = Nonterminal String | Terminal String deriving (Show, Eq, Ord)
emptySymbol = Terminal "empty"
eofSymbol = Terminal "eof"

fromString :: String -> Symbol
fromString s = if isUpper (head (dropWhile (not.isLetter) s)) then Nonterminal s else Terminal s

nonterminal :: Symbol -> Bool
nonterminal (Nonterminal _) = True
nonterminal _ = False

terminal :: Symbol -> Bool
terminal (Terminal _) = True
terminal _ = False

type Rule = (Symbol, [Symbol])
source :: Rule -> Symbol
source = fst
result :: Rule -> [Symbol]
result = snd

ruleValid :: [String] -> Bool
ruleValid rs
  | length rs < 3 = False
  | rs !! 1 /= "::=" = False
  | otherwise = True

data CFG = CFG [Symbol] [Symbol] [Rule] Symbol deriving Show
nonterminals (CFG nts _ _ _) = nts
terminals (CFG _ ts _ _) = ts
rules (CFG _ _ rs _) = rs
starter (CFG _ _ _ st) = st

readCFG :: String -> CFG
readCFG rules = CFG (filter nonterminal symbols)
  (filter terminal symbols)
  (concat $ map readRule $ lines rules)
  (head symbols) where
    symbols = map fromString (
      (nubOrd $
      filter (\x -> x /= "::=" && x /= "|")
      (concat $ filter ruleValid (map words (lines rules))))) ++ [eofSymbol]

readRule :: String -> [Rule]
readRule r = readRule' (words r) where
  readRule' rs
    | terminal (fromString $ head rs) = error "Terminal appears on the left hand side."
    | ruleValid rs = map (\x -> (fromString (head rs), map fromString x)) (splitOn (=="|") (tail $ tail rs))
    | otherwise = []

rulesFrom :: CFG -> Symbol -> [Rule]
rulesFrom cfg s = filter (\r -> source r == s) (rules cfg)

rulesTo :: CFG -> Symbol -> [Rule]
rulesTo cfg s = filter (\r -> s `elem` (result r)) (rules cfg)

------------ Utility functions ------------

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p xs = case dropWhile p xs of
                 [] -> []
                 xs' -> w : splitOn p xs''
                   where (w, xs'') = break p xs'

nubOrd :: (Eq a, Ord a) => [a] -> [a]
nubOrd [] = []
nubOrd xs = x' : nubOrd' xs' x' where
  (x':xs') = sort xs
  nubOrd' [] _ = []
  nubOrd' (x:xs) h
    | h == x    = nubOrd' xs h
    | otherwise = x : nubOrd' xs x

