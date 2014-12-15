module While.Compiler where

import While.AST

compileAExpr :: AExpr -> String
compileAExpr (Var s) = s
compileAExpr (IntConst n) = show n
compileAExpr (AUnary op a) = "(" ++ compileAUnaOp op ++ " " ++ compileAExpr a ++ ")"
compileAExpr (ABinary op a b) = "(" ++ compileAExpr a ++ " " ++ compileABinOp op++ " " ++ compileAExpr b ++ ")"

compileAUnaOp :: AUnaOp -> String
compileAUnaOp Neg = "-"

compileABinOp :: ABinOp -> String
compileABinOp Add = "+"
compileABinOp Subtract = "-"
compileABinOp Multiply = "*"
compileABinOp Divide = "/"

compileBExpr :: BExpr -> String
compileBExpr (BoolConst True) = "true"
compileBExpr (BoolConst False) = "false"
compileBExpr (BUnary op a) = "(" ++ compileBUnaOp op ++ " "  ++ compileBExpr a ++ ")"
compileBExpr (BBinary op a b) = "(" ++ compileBExpr a ++ " " ++ compileBBinOp op ++ " " ++ compileBExpr b ++ ")"
compileBExpr (RBinary op a b) = "(" ++ compileAExpr a ++ " " ++ compileRBinOp op ++ " " ++ compileAExpr b ++ ")"

compileBUnaOp :: BUnaOp -> String
compileBUnaOp Not = "not"

compileBBinOp :: BBinOp -> String
compileBBinOp And = "and"
compileBBinOp Or = "or"

compileRBinOp :: RBinOp -> String
compileRBinOp Equal = "=="
compileRBinOp NotEqual = "<>"
compileRBinOp Greater = ">"
compileRBinOp GreaterEqual = ">="
compileRBinOp Less = "<"
compileRBinOp LessEqual = "<="

compileStmt :: Stmt -> String
compileStmt Skip = "skip"
compileStmt (Seq a@(Seq _ _) b) = "(" ++ compileStmt a ++ "); " ++ compileStmt b
compileStmt (Seq a b) = compileStmt a ++ "; " ++ compileStmt b
compileStmt (Assign var expr) = var ++ " := " ++ compileAExpr expr
compileStmt (If test a b) = "if " ++ compileBExpr test ++ " then (" ++ compileStmt a ++ ") else (" ++ compileStmt b ++ ")"
compileStmt (While test expr) = "while " ++ compileBExpr test ++ " do (" ++ compileStmt expr ++ ")"
