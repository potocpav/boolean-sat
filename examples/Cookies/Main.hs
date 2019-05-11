{-# LANGUAGE LambdaCase #-}

import Prelude hiding (xor, not, (&&), (||))

import           Algebra.SAT (Expr(Var), solve, dimacs, cnf)
import           Control.Monad (join)
import           Data.Algebra.Boolean (Boolean(..))
import           Data.List (intercalate)
import           Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S


tile :: [[a]] -> Int -> Int -> Maybe a
tile x i j = if i >= 0 && i <= 4 && j >= 0 && j <= 4 then Just $ (x !! j) !! i else Nothing


vars :: [[Expr Int]]
vars = [[Var (i+j*5+1) | i <- [0..4]] | j <- [0..4]]


exprs :: [[Expr Int]] -> [[Expr Int]]
exprs goal = [[mkExpr i j | i <- [0..4]] | j <- [0..4]] where
    mkExpr :: Int -> Int -> Expr Int
    mkExpr i j = foldr1 xor $ catMaybes [tile goal i j, tile vars i j, tile vars (i+1) j, tile vars (i-1) j, tile vars i (j+1), tile vars i (j-1)]


expr :: [[Expr Int]] -> Expr Int
expr goal = not . foldr1 (||) $ join (exprs goal)


dispSolution :: [(Int, Bool)] -> IO ()
dispSolution ii = do
    let square = take 5 . map (take 5) . iterate (drop 5) $ map snd ii
    let printBool True = "o"
        printBool False = "-"
    let printRow l = putStrLn $ "[ " ++ intercalate " " (map printBool l) ++ " ]"
    sequence_ $ map printRow square


goal :: [[Expr Int]]
goal = map (\case {'o' -> true; '-' -> false}) <$>
    [ "----o"
    , "---o-"
    , "-----"
    , "-o---"
    , "o----"
    ]


main :: IO ()
main = do
    solution <- solve (expr goal)
    case solution of
        Just s  -> dispSolution (M.toList s)
        Nothing -> pure ()
    pure ()
