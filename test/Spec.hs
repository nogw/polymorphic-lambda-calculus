import AlternativeParse
import Data.Either
import Control.Exception
import Expr
import Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import PrettyPrint
import TypeCheck
import Data.Map

typecheck' :: (Ord n, Eq n, PrettyPrint n)
          => UniqueSupply n 
          -> [(n, Type n)]
          -> Expr n n 
          -> Either String (Type n)
typecheck' uniqs ctx = typecheck uniqs (fromList ctx)

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
    it "fn x:T => x" $
      altRunParseExpr "fn x:T => x" `shouldBe` Right (Abstraction {param_abs = "x", param_type_abs = TypeVar "T", body_abs = Var "x"})
    it "fn X => x" $
      altRunParseExpr "fn X => x" `shouldBe` Right (TypeAbstraction {param = "X", body = Var "x"})
    it "x [T] (type application)" $
      altRunParseExpr "x [T]" `shouldBe` Right (TypeApplication {function = Var "x", argument = TypeVar "T"})
    it "f x" $
      altRunParseExpr "f x" `shouldBe` Right (Application {function_app = Var "f", argument_app = Var "x"})
    it "f x y z" $
      altRunParseExpr "f x y z" `shouldBe` Right (Application {function_app = Application {function_app = Application {function_app = Var "f", argument_app = Var "x"}, argument_app = Var "y"}, argument_app = Var "z"})
    it "fn f:(X->Y) x:Y => f x" $
      altRunParseExpr "fn f:(X->Y) x:Y => f x" `shouldBe` Right (Abstraction {param_abs = "f", param_type_abs = TypeArrow {param_type = TypeVar "X", body_type = TypeVar "Y"}, body_abs = Abstraction {param_abs = "x", param_type_abs = TypeVar "Y", body_abs = Application {function_app = Var "f", argument_app = Var "x"}}})
    it "(fn x:X => f x) g y" $
      altRunParseExpr "(fn x:X => f x) g y" `shouldBe` Right (Application {function_app = Application {function_app = Abstraction {param_abs = "x", param_type_abs = TypeVar "X", body_abs = Application {function_app = Var "f", argument_app = Var "x"}}, argument_app = Var "g"}, argument_app = Var "y"})
    it "f (fn x:T => x)" $
      altRunParseExpr "f (fn x:T => x)" `shouldBe` Right (Application {function_app = Var "f", argument_app = Abstraction {param_abs = "x", param_type_abs = TypeVar "T", body_abs = Var "x"}})
    it "fn n:X => fn f:(A->B) x:Y => f (n f x)" $ 
      altRunParseExpr "fn n:X => fn f:(A->B) x:Y => f (n f x)" `shouldBe` Right (Abstraction {param_abs = "n", param_type_abs = TypeVar "X", body_abs = Abstraction {param_abs = "f", param_type_abs = TypeArrow {param_type = TypeVar "A", body_type = TypeVar "B"}, body_abs = Abstraction {param_abs = "x", param_type_abs = TypeVar "Y", body_abs = Application {function_app = Var "f", argument_app = Application {function_app = Application {function_app = Var "n", argument_app = Var "f"}, argument_app = Var "x"}}}}})
    it "(fn p:(X->Y->Z) x:X y:Y => y) (fn p:(A->B->C) x:B y:C => x)" $
      altRunParseExpr "(fn p:(X->Y->Z) x:X y:Y => y) (fn p:(A->B->C) x:B y:C => x)" `shouldBe` Right (Application {function_app = Abstraction {param_abs = "p", param_type_abs = TypeArrow {param_type = TypeVar "X", body_type = TypeArrow {param_type = TypeVar "Y", body_type = TypeVar "Z"}}, body_abs = Abstraction {param_abs = "x", param_type_abs = TypeVar "X", body_abs = Abstraction {param_abs = "y", param_type_abs = TypeVar "Y", body_abs = Var "y"}}}, argument_app = Abstraction {param_abs = "p", param_type_abs = TypeArrow {param_type = TypeVar "A", body_type = TypeArrow {param_type = TypeVar "B", body_type = TypeVar "C"}}, body_abs = Abstraction {param_abs = "x", param_type_abs = TypeVar "B", body_abs = Abstraction {param_abs = "y", param_type_abs = TypeVar "C", body_abs = Var "x"}}}})
    it "(fn f:(X->Y) => (fn x:X y:Y => f x y) f x y) w x y" $
      altRunParseExpr "(fn f:(X->Y) => (fn x:X y:Y => f x y) f x y) w x y" `shouldBe` Right (Application {function_app = Application {function_app = Application {function_app = Abstraction {param_abs = "f", param_type_abs = TypeArrow {param_type = TypeVar "X", body_type = TypeVar "Y"}, body_abs = Application {function_app = Application {function_app = Application {function_app = Abstraction {param_abs = "x", param_type_abs = TypeVar "X", body_abs = Abstraction {param_abs = "y", param_type_abs = TypeVar "Y", body_abs = Application {function_app = Application {function_app = Var "f", argument_app = Var "x"}, argument_app = Var "y"}}}, argument_app = Var "f"}, argument_app = Var "x"}, argument_app = Var "y"}}, argument_app = Var "w"}, argument_app = Var "x"}, argument_app = Var "y"})
    it "church encoding" $ do
      let encodings =
            [ "fn f: (A->B) => fn x:T => x", -- 0
              "fn f: (A->B) => fn x:T => f x", -- 1
              "fn f: (A->B) => fn x:T => f (f x)", -- 2
              "fn f: (A->B) => fn x:T => f (f (f x))", -- 3
              "fn f: (A->B) => fn x:T => f (f (f (f x)))" ] -- 4, makes sense?

      mapM_ (flip shouldSatisfy isRight . altRunParseExpr) encodings

    describe "Typecheck Expressions" $ do
      it "var" $
        typecheck' [] [("x", TypeVar "X")] (Var "x") `shouldBe` Right (TypeVar "X")