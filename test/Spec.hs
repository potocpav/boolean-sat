
import Prelude hiding (not, or, and, (||), (&&))

-- import Test.QuickCheck
import Algebra.SAT (Expr(..), solveExpr)
import Data.Algebra.Boolean
import Data.Maybe (isNothing)
import Test.HUnit.Base (assertBool)


equiv :: Ord a => Expr a -> Expr a -> Bool
equiv e f = isNothing . solveExpr $ not (e <--> f)

equiv' :: Expr () -> Expr () -> Bool
equiv' = equiv


main :: IO ()
main = do
    let a:b:_ = map Var [1..]
    assertBool "Variable is equal to itself" $ equiv a a
    assertBool "Empty disjunction is False" $ equiv' (or []) false
    assertBool "Empty conjunction is True" $ equiv' (and []) true
    assertBool "One-element conjunction is identity" $ equiv (and [a]) a
    assertBool "One-element disjunction is identity" $ equiv (or [a]) a
    assertBool "Two-element disjunction is `or`" $ equiv (or [a, b]) (a || b)
    assertBool "Two-element conjunction is `and`" $ equiv (and [a, b]) (a && b)
    assertBool "Double negation is identity" $ equiv (not (not a)) a
    assertBool "Double negation is identity" $ equiv (not (not a)) a
    assertBool "`or` is commutative" $ equiv (a || b) (b || a)
    assertBool "`and` is commutative" $ equiv (a && b) (b && a)
