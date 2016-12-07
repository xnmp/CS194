-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module LogAnalysis where

import           Data.List
import qualified ExprT as ET
import           Parser
import StackVM
import Data.Maybe


-- evalStr :: String -> Maybe Integer
-- evalStr ss = eval <$> parseExp Lit Add Mul ss

-- eval :: ExprT -> Integer
-- eval (Lit x)   = x
-- eval (Add x y) = eval x + eval y
-- eval (Mul x y) = eval x * eval y

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ET.ExprT where
    lit = ET.Lit
    add = ET.Add
    mul = ET.Mul
    
-- instance Expr Bool where
--     lit x
--         | x <= 0 = False
--         | otherwise = True
--     add = (||)
--     mul = (&&)

-- newtype MinMax = MinMax Integer deriving (Eq, Show, Ord, Num, Integral, Enum, Real)
-- newtype Mod7 = Mod7 Integer deriving (Eq, Show, Ord, Num, Integral, Enum, Real)

-- instance Expr Integer where
--     lit = id
--     add = (+)
--     mul = (*)
    
-- instance Expr MinMax where
--     lit = MinMax . id
--     add = max
--     mul = min

-- instance Expr Mod7 where
--     lit = Mod7 . (`mod` 7)
--     add x y = (x + y) `mod` 7
--     mul x y =(x * y) `mod` 7

instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [Add]
    mul x y = x ++ y ++ [Mul]

compile :: String -> Program
compile x = case parseExp lit add mul x of
    Just y -> y
    Nothing -> []

main = print $ compile "(3 * -4) + 5"
