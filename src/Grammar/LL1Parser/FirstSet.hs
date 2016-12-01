module Grammar.LL1Parser.FirstSet(
  firstSet, firstSetSeq
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Grammar.CFG

newtype FirstSetSeq = FirstSet { getSet :: Set.Set Symbol }

-- Monoid for combining first sets of a sequence
-- If the left set contains empty symbol, the next set also get included
-- If it doesn't, stop
instance Monoid (FirstSetSeq) where
  mempty = FirstSet (Set.singleton emptySymbol)
  mappend x@(FirstSet xs) y@(FirstSet ys) = if emptySymbol `Set.member` xs then FirstSet ((Set.delete emptySymbol xs) `Set.union` ys) else x

firstSetMap :: CFG -> Map.Map Symbol (Set.Set Symbol)
firstSetMap cfg = Map.fromList fs where
  fs = map (\x -> (x, computeFirstSet cfg x)) (nonterminals cfg) where

computeFirstSet :: CFG -> Symbol -> Set.Set Symbol
computeFirstSet cfg s
  | terminal s = Set.singleton s
  | otherwise = Set.unions (map ((firstSetSeq cfg).result) (rulesFrom cfg s))

firstSet :: CFG -> Symbol -> Set.Set Symbol
firstSet cfg s = Map.findWithDefault (Set.singleton s) s (firstSetMap cfg)

firstSetSeq :: CFG -> [Symbol] -> Set.Set Symbol
firstSetSeq _ [] = Set.singleton emptySymbol
firstSetSeq cfg xs = getSet $ mconcat (map (FirstSet.(firstSet cfg)) xs)

