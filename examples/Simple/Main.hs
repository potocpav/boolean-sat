
import Prelude hiding (xor, not, (&&), (||))

-- import qualified Algebra.SAT.FFI as FFI
import Algebra.SAT          (Expr(Var), solveExpr)
import Data.Algebra.Boolean (Boolean(..))


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
    solution <- solveExpr expr
    putStrLn $ "Socrate is " ++
        if solution == Nothing then "right." else "wrong!"
