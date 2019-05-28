{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

{- | This example is a simple solver for the Android puzzle game "Magic Cookies!":
https://play.google.com/store/apps/details?id=uk.co.keera.games.magiccookies&hl=en

The game is played on a 5x5 grid ("tray") of squares. On each square, there may or may not be a cookie. A player is
given a starting tray with some cookies, and is tasked to empty the tray by clicking on arbitrary squares in
sequence. Each click flips the state of the clicked square and all directly adjacent squares (cookie is transformed
to empty, and empty is transformed to cookie). There is also a (generous) move limit in each level, but we ignore it
in designing our solver.

Example board state: "o"..cookie, "-"..empty, "x"..clicked empty square

[ - - - o - ]
[ - - o - - ]
[ - o x - - ]
[ o - - - - ]
[ - - - - - ]

Example board state after clicking on "x":

[ - - - o - ]
[ - - - - - ]
[ - - o o - ]
[ o - o - - ]
[ - - - - - ]

-}
module Main where

import Prelude hiding (xor, not, or, (&&))

import           Algebra.SAT (Expr(Var), solve, dimacs, cnf)
import           Control.Monad (join, guard)
import           Control.Arrow
import           Data.Algebra.Boolean
import           Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Bool (bool)


-- | Position within the board
data Pos = Pos Int Int deriving (Eq, Ord)

-- | A list newtype representing board state
newtype Board a = Board { unBoard :: [a] } deriving (Functor, Foldable)


-- | Enumeration of all possible positions
positions :: Board Pos
positions = Board [Pos i j | i <- [0..4], j <- [0..4]]


-- | Expression describing the final board state
exprs :: Board Bool -> Board (Expr Pos)
exprs startState = zipWithB xor (fromBool <$> startState) $
    foldr1 xor . map Var . neighborhood <$> positions where

    -- | zipWith equivalent over boards
    zipWithB :: (a -> b -> c) -> Board a -> Board b -> Board c
    zipWithB f (Board a) (Board b) = Board (zipWith f a b)

    -- | Get the five squares that get flipped by a single click. Filtered to disregard out-of-bounds squares.
    neighborhood :: Pos -> [Pos]
    neighborhood p@(Pos i j) = filter (`elem` positions)
        [p, Pos (i-1) j, Pos (i+1) j, Pos i (j-1), Pos i (j+1)]


-- | Expression that is True iff all squares are False (empty)
expr :: Board Bool -> Expr Pos
expr goal = not . or $ exprs goal


-- | Display the solution
dispSolution :: M.Map Pos Bool -> IO ()
dispSolution m = do
    let Board l = (m M.!) <$> positions
    let square = take 5 . map (take 5) . iterate (drop 5) $ l
    let printBool = bool "-" "o"
    let printRow l = putStrLn $ "[ " ++ unwords (map printBool l) ++ " ]"
    mapM_ printRow square


-- | Game starting state to solve
goal :: Board Bool
goal = Board . join $ map (\case {'o' -> True; '-' -> False}) <$>
    [ "----o"
    , "---o-"
    , "-----"
    , "-o---"
    , "o----"
    ]


main :: IO ()
main = do
    let cnf' = cnf (expr goal)
    writeFile "cookies.dimacs" $ dimacs cnf'
    case solve cnf' of
        Just s  -> dispSolution s
        Nothing -> putStrLn "No solution found."
