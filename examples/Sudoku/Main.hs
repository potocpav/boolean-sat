{-# LANGUAGE LambdaCase #-}

import Prelude hiding (xor, not, (&&), (||))

import           Algebra.SAT (Expr(Var), cnf, dimacsCnf, solveCnf)
import           Control.Monad (guard)
import           Data.Algebra.Boolean (Boolean(..))
import           Data.Maybe (maybeToList)
import qualified Data.Set as S


var :: Int -> Int -> Int -> Expr Int
var num i j = Var (num + i * 10 + j * 100)

var' n i j = var n j i

var'' n i j = var n i' j' where
    i' = ((i-1) `mod` 3) + 3 * ((j-1) `mod` 3) + 1
    j' = ((i-1) `div` 3) + 3 * ((j-1) `mod` 3) + 1


sudoku :: [[Maybe Int]] -> Expr Int
sudoku b = foldl1 (&&) $ [allNumsInSegment var, allNumsInSegment var', allNumsInSegment var'', justOneNumber, board b] where

    board :: [[Maybe Int]] -> Expr Int
    board ss = foldl1 (&&) $ do
        (j, r) <- zip [1..] ss
        (i, mn) <- zip [1..] r
        n <- maybeToList mn
        pure (var n i j)

    allNumsInSegment :: (Int -> Int -> Int -> Expr Int) -> Expr Int
    allNumsInSegment var_ = foldl1 (&&) $ do
        j <- [1..9]
        num <- [1..9]
        pure . foldl1 (||) $ do
            i <- [1..9]
            pure (var_ num i j)

    justOneNumber :: Expr Int
    justOneNumber = foldl1 (&&) $ do
        i <- [1..9]
        j <- [1..9]
        pure . foldl1 (||) $ do
            num <- [1..9]
            pure . foldl1 (&&) $ var num i j : do
                other <- [1..9]
                guard $ other /= num
                pure $ not (var other i j)


board :: [[Maybe Int]]
board = map (map (\case
    "-" -> Nothing
    x -> Just (read x)
    ) . words) $
    [ "5 6 - - - - - - -"
    , "- - - - - - - - -"
    , "- - - - - - - - -"
    , "- - - - - - - - -"
    , "- - - - - - - - -"
    , "- - - - - - - - -"
    , "- - - - - - - - -"
    , "- - - - - - - - -"
    , "- - - - - - - - -"
    ]

main :: IO ()
main = do
    let e = sudoku board
    let cnf' = cnf e
    writeFile "sudoku.dimacs" $ dimacsCnf cnf'
    -- print <$> solveCnf cnf'
    pure ()
