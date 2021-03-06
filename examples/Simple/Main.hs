
import Prelude hiding (xor, not, (&&), (||))

-- import qualified Algebra.SAT.FFI as FFI
import Algebra.SAT          (Expr(Var), solveExpr)
import Data.Algebra.Boolean (Boolean(..))
import Data.Maybe           (isNothing)


-- Socrates says:
--
-- “If I’m guilty, I must be punished;
-- I’m guilty. Thus I must be punished.”
--
-- This example proves that Socrates is right:


data Vars = Guilty
          | Punished
          deriving (Eq, Ord, Show)


main = do
    let premise = (Var Guilty --> Var Punished) && Var Guilty
        consequence = Var Punished
        expr = premise && not consequence

    -- if expr is UNSAT, then premise implies consequence
    let solution = solveExpr expr
    putStrLn $ "Socrates is " ++
        if isNothing solution then "right." else "wrong!"
