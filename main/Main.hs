import System.IO
import Test.QuickCheck
import qualified Data.Set as Set

import Abstat.Interface.Semantics
import Abstat.Interface.AST
import Abstat.Interface.State
import qualified Abstat.Common.AbstractState as Abstract
import qualified Abstat.Common.ConcreteState as Concrete
import While.AST
import While.ASTGenerator()
import While.Parser
import While.Compiler
import While.Concrete()
import While.Domain.Int.Semantics()
import While.Domain.Sign.Semantics()
import While.Domain.Interval.Semantics()
import While.Domain.Congruence.Semantics()
import qualified While.Domain.Int.Domain as Int
import qualified While.Domain.Sign.Domain as Sign
import qualified While.Domain.Interval.Domain as Interval
import qualified While.Domain.Congruence.Domain as Congruence

readInput :: IO String
readInput = do
    done <- isEOF
    if done
        then return ""
        else do
            first_inp <- getLine
            then_inp <- readInput
            return $ first_inp ++ "\n" ++ then_inp

main :: IO ()
main = do
    putStrLn "=== Abstat ==="
    input <- readInput

    sourceCode <-
        if input == "" || input == "\n"
        then do
            trees <- sample' (arbitrary :: Gen Stmt)
            let tree = trees !! 10
            return $! compileStmt tree
        else return input

    putStrLn sourceCode

    putStrLn "(*) Parsing..."
    let program = parseString sourceCode
    let vars = Set.toList $ usedVars program

    putStrLn "(*) Abstract interpretation..."
    putStrLn "( ) Int domain:"
    putStrLn $ showVars vars (interpret program :: Abstract.State Int.Domain)
    putStrLn "( ) Sign domain:"
    putStrLn $ showVars vars (interpret program :: Abstract.State Sign.Domain)
    putStrLn "( ) Interval domain:"
    putStrLn $ showVars vars (interpret program :: Abstract.State Interval.Domain)
    putStrLn $ "( ) Congruence " ++ show Congruence.moduleVal ++ ":"
    putStrLn $ showVars vars (interpret program :: Abstract.State Congruence.Domain)

    putStrLn "(*) Concrete interpretation..."
    putStrLn $ Concrete.showState (interpret program :: Concrete.State Integer)
