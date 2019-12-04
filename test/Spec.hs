
import Prelude hiding (not, or, and, (||), (&&))

-- import Test.QuickCheck
import Algebra.SAT (Expr(..), solveExpr)
import Data.Algebra.Boolean
import Data.Maybe (isNothing, isJust)
import Test.HUnit.Base (assertBool)


-- | Empty type. There is a constructor, because "deriving" doesn't work without it.
-- https://wiki.haskell.org/Empty_type
newtype Void = Void Void deriving (Eq, Ord)


-- | Test expressions for equivalence using a SAT solver. On top of that,
-- test equivalence for satisfiability; without that, a faux solver returning
-- always UNSAT would pass all the tests.
equiv :: Ord a => Expr a -> Expr a -> Bool
equiv e f = (isNothing . solveExpr $ e `xor` f)
         && (isJust . solveExpr $ e <--> f)


-- | Test expressions without logical variables for equivalence
equiv' :: Expr Void -> Expr Void -> Bool
equiv' = equiv


main :: IO ()
main = do
    let a:b:_ = map Var [1..]
    assertBool "Variable must be equal to itself" $ equiv a a
    assertBool "Empty disjunction must be False" $ equiv' (or []) false
    assertBool "Empty conjunction must be True" $ equiv' (and []) true
    assertBool "One-element conjunction must be identity" $ equiv (and [a]) a
    assertBool "One-element disjunction must be identity" $ equiv (or [a]) a
    assertBool "Two-element disjunction must be `or`" $ equiv (or [a, b]) (a || b)
    assertBool "Two-element conjunction must be `and`" $ equiv (and [a, b]) (a && b)
    assertBool "Double negation must be identity" $ equiv (not (not a)) a
    assertBool "Double negation must be identity" $ equiv (not (not a)) a
    assertBool "`or` must be commutative" $ equiv (a || b) (b || a)
    assertBool "`and` must be commutative" $ equiv (a && b) (b && a)
