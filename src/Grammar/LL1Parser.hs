module Grammar.LL1Parser (parseProperly) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Grammar.CFG
import Grammar.LL1Parser.FirstSet
import Grammar.LL1Parser.FollowSet

parsingTable :: CFG -> Map.Map (Symbol, Symbol) Rule
parsingTable cfg = Map.fromListWith (\_ -> error "Not an LL1 grammar: More than one usable rule for a nonterminal-terminal pair.") (
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

