module Grammar.LL1Parser (parseProperly) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Grammar.CFG

firstSetOf :: CFG -> Map.Map Symbol (Set.Set Symbol)
firstSetOf cfg = Map.fromList fs where
  fs = map (\x -> (x, computeFirstSet cfg x)) symbol where
    symbol = (nonterminals cfg) ++ (terminals cfg)

computeFirstSet :: CFG -> Symbol -> Set.Set Symbol
computeFirstSet cfg s
  | terminal s = Set.singleton s
  | otherwise = Set.unions (map ((firstSetSeq cfg).result) (rulesFrom cfg s))

firstSet :: CFG -> Symbol -> Set.Set Symbol
firstSet cfg s = Map.findWithDefault Set.empty s (firstSetOf cfg)

firstSetSeq :: CFG -> [Symbol] -> Set.Set Symbol
firstSetSeq _ [] = Set.singleton emptySymbol
firstSetSeq cfg (x:xs) = (Set.delete emptySymbol fx) `Set.union`
  if emptySymbol `Set.member` fx then firstSetSeq cfg xs
  else Set.empty where fx = firstSet cfg x

followSetOf :: CFG -> Map.Map Symbol (Set.Set Symbol)
followSetOf cfg = Map.fromList fs where
  fs = symbol >>= (\x -> return (x, computeFollowSet cfg x)) where
    symbol = (nonterminals cfg) ++ (terminals cfg)

computeFollowSet :: CFG -> Symbol -> Set.Set Symbol
computeFollowSet cfg s = if s == starter cfg then Set.singleton eofSymbol else Set.unions (map followRule rs) where
  rs = rulesTo cfg s
  followRule :: Rule -> Set.Set Symbol
  followRule (from, (x:xs))
    | x == s = (Set.delete emptySymbol fxs) `Set.union` (if (null xs) || (emptySymbol `Set.member` fxs) then followSet cfg from else Set.empty)
    | otherwise = followRule(from, xs) where fxs = firstSetSeq cfg xs

followSet :: CFG -> Symbol -> Set.Set Symbol
followSet cfg s = Map.findWithDefault Set.empty s (followSetOf cfg)

parsingTable :: CFG -> Map.Map (Symbol, Symbol) Rule
parsingTable cfg = Map.fromListWith (\_ -> error "Not an LL1 grammar.") (
  do nt <- nonterminals cfg
     t <- terminals cfg
     r <- matchRules cfg nt t
     return ((nt, t), r))

matchRules :: CFG -> Symbol -> Symbol -> [Rule]
matchRules cfg nt t = filter (\r -> inFirst r || inFollow r) (rulesFrom cfg nt) where
  inFirst r = t `Set.member` (fresult r)
  inFollow r = emptySymbol `Set.member` (fresult r) && t `Set.member` (followSet cfg nt)
  fresult r = firstSetSeq cfg (result r)

parsingRule :: CFG -> Symbol -> Symbol -> Maybe Rule
parsingRule cfg nt t = Map.lookup (nt, t) (parsingTable cfg)

parseProperly :: CFG -> String -> Bool
parseProperly cfg t = match cfg [starter cfg] (words t)

match :: CFG -> [Symbol] -> [String] -> Bool
match _ [] [] = True
match _ [] _ = False
match cfg s [] = emptySymbol `Set.member` (firstSetSeq cfg s)
match cfg s@(x:xs) t@(y:ys) =
   if terminal x then
     if x == emptySymbol then match cfg xs t
     else matchTerm (toString x) y && match cfg xs ys
   else
     case parsingRule cfg x (fromString y) of
       Nothing -> False
       Just r -> match cfg (result r ++ xs) t

matchTerm :: String -> String -> Bool
matchTerm s t = case s of
  "filename" -> matchFileName t
  "id" -> matchId t
  "num" -> matchNum t
  _ -> s == t

matchFileName :: String -> Bool
matchFileName s = s == "filename" -- stub

matchId :: String -> Bool
matchId s = s == "id" --stub

matchNum :: String -> Bool
matchNum s = s == "num" --stub

