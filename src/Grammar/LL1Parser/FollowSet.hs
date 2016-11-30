module Grammar.LL1Parser.FollowSet(
  followSet
) where

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

