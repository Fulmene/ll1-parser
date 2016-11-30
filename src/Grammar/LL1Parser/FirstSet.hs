module Grammar.LL1Parser.FirstSet(
  firstSet, firstSetSeq
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Grammar.CFG

newtype FirstSetSeq = FirstSet { getSet :: Set.Set Symbol }

instance Monoid (FirstSetSeq) where
  mempty = FirstSet (Set.singleton emptySymbol)
  mappend x@(FirstSet xs) y@(FirstSet ys) = if emptySymbol `Set.member` xs then FirstSet ((Set.delete emptySymbol xs) `Set.union` ys) else x

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
firstSetSeq cfg xs = getSet $ mconcat (map (FirstSet.(firstSet cfg)) xs)

