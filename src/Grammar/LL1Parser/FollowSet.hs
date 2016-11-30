module Grammar.LL1Parser.FollowSet(
  followSet
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Grammar.CFG
import Grammar.LL1Parser.FirstSet

followSetOf :: CFG -> Map.Map Symbol (Set.Set Symbol)
followSetOf cfg = Map.fromList fs where
  fs = (nonterminals cfg) >>= (\x -> return (x, computeFollowSet cfg x))

computeFollowSet :: CFG -> Symbol -> Set.Set Symbol
computeFollowSet cfg s = (if s == starter cfg then Set.singleton eofSymbol else Set.empty) `Set.union` (Set.unions (map followRule rs)) where
  rs = rulesTo cfg s
  followRule :: Rule -> Set.Set Symbol
  followRule (_, []) = Set.empty
  followRule (from, (x:xs)) = Set.unions [
                                           if x == s then Set.delete emptySymbol fxs else Set.empty,
                                           if x == s && x /= from && emptySymbol `Set.member` fxs then followSet cfg from else Set.empty,
                                           followRule (from, xs)
                                         ] where
    fxs = firstSetSeq cfg xs

followSet :: CFG -> Symbol -> Set.Set Symbol
followSet cfg s = Map.findWithDefault Set.empty s (followSetOf cfg)

