import Control.Exception
import Expr
import Parser
import AlternativeParse
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Parse Expressions" $ do
    it "x" $ do
      runParseExpr "x" `shouldBe` Right (Var "x")
    it "(x)" $ do
      runParseExpr "(x)" `shouldBe` Right (Var "x")
    it "\\x:T. x" $ do
      runParseExpr "\\x:T. x" `shouldBe` Right (Abstraction {param_abs = "x", param_type_abs = TypeVar "T", body_abs = Var "x"})
    it "\\X. x" $ do
      runParseExpr "\\X. x" `shouldBe` Right (TypeAbstraction {param = "X", body = Var "x"})
    it "x [T] (type application)" $ do
      runParseExpr "x [T]" `shouldBe` Right (TypeApplication {function = Var "x", argument = TypeVar "T"})
    it "f x" $ do
      runParseExpr "f x" `shouldBe` Right (Application {function_app = Var "f", argument_app = Var "x"})
    it "f x y z" $ do
      runParseExpr "f x y z" `shouldBe` Right (Application {function_app = Application {function_app = Application {function_app = Var "f", argument_app = Var "x"}, argument_app = Var "y"}, argument_app = Var "z"})
    it "\\f:(X->Y) x:Y. f x" $ do
      runParseExpr "\\f:(X->Y) x:Y. f x" `shouldBe` Right (Abstraction {param_abs = "f", param_type_abs = TypeArrow {param_type = TypeVar "X", body_type = TypeVar "Y"}, body_abs = Abstraction {param_abs = "x", param_type_abs = TypeVar "Y", body_abs = Application {function_app = Var "f", argument_app = Var "x"}}})
    it "(\\x:X . f x) g y" $ do
      runParseExpr "(\\x:X . f x) g y" `shouldBe` Right (Application {function_app = Application {function_app = Abstraction {param_abs = "x", param_type_abs = TypeVar "X", body_abs = Application {function_app = Var "f", argument_app = Var "x"}}, argument_app = Var "g"}, argument_app = Var "y"})
    it "f (\\x:T. x)" $ do
      runParseExpr "f (\\x:T. x)" `shouldBe` Right (Application {function_app = Var "f", argument_app = Abstraction {param_abs = "x", param_type_abs = TypeVar "T", body_abs = Var "x"}})

  describe "Parse Expressions using other parser" $ do
    it "fn x:T => x" $ do
      altRunParseExpr "fn x:T => x" `shouldBe` Right (Abstraction {param_abs = "x", param_type_abs = TypeVar "T", body_abs = Var "x"})
    it "fn X => x" $ do
      altRunParseExpr "fn X => x" `shouldBe` Right (TypeAbstraction {param = "X", body = Var "x"})
    it "x [T] (type application)" $ do
      altRunParseExpr "x [T]" `shouldBe` Right (TypeApplication {function = Var "x", argument = TypeVar "T"})
    it "f x" $ do
      altRunParseExpr "f x" `shouldBe` Right (Application {function_app = Var "f", argument_app = Var "x"})
    it "f x y z" $ do
      altRunParseExpr "f x y z" `shouldBe` Right (Application {function_app = Application {function_app = Application {function_app = Var "f", argument_app = Var "x"}, argument_app = Var "y"}, argument_app = Var "z"})
    it "fn f:(X->Y) x:Y => f x" $ do
      altRunParseExpr "fn f:(X->Y) x:Y => f x" `shouldBe` Right (Abstraction {param_abs = "f", param_type_abs = TypeArrow {param_type = TypeVar "X", body_type = TypeVar "Y"}, body_abs = Abstraction {param_abs = "x", param_type_abs = TypeVar "Y", body_abs = Application {function_app = Var "f", argument_app = Var "x"}}})
    it "(fn x:X => f x) g y" $ do
      altRunParseExpr "(fn x:X => f x) g y" `shouldBe` Right (Application {function_app = Application {function_app = Abstraction {param_abs = "x", param_type_abs = TypeVar "X", body_abs = Application {function_app = Var "f", argument_app = Var "x"}}, argument_app = Var "g"}, argument_app = Var "y"})
    it "f (fn x:T => x)" $ do
      altRunParseExpr "f (fn x:T => x)" `shouldBe` Right (Application {function_app = Var "f", argument_app = Abstraction {param_abs = "x", param_type_abs = TypeVar "T", body_abs = Var "x"}})