{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as SVM

-- import Data.Maybe
import qualified Data.Map as M
import Control.Applicative
-- import Data.


-- EXERCISE 01

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b 

-- EXERCISE 02

-- evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

-- EXERCISE 03

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- EXERCISE 04

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax l) (MinMax r) = MinMax $ max l r
  mul (MinMax l) (MinMax r) = MinMax $ min l r

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 l) (Mod7 r) = Mod7 $ (l + r) `mod` 7
  mul (Mod7 l) (Mod7 r) = Mod7 $ (l * r) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- EXERCISE 05

instance Expr SVM.Program where
  lit x = [SVM.PushI x]
  add left right = left ++ right ++ [SVM.Add] 
  mul left right = left ++ right ++ [SVM.Mul] 

compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul

-- EXERCISE 06

class HasVars a where
  var :: String -> a

data VarExprT = 
    VLit Integer
  | Var String
  | VAdd VarExprT VarExprT
  | VMul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add f g m = liftA2 (+) (f m) (g m)
  mul f g m = liftA2 (*) (f m) (g m)

withVars :: 
     [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs