-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Home8 where
import Data.Monoid
import Employee
import Data.Char
import           Data.Tree
import qualified Data.Map as Map

glCons :: Employee -> GuestList -> GuestList
glCons e (GL gl funscore) = GL (e : gl) (empFun e + funscore)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL gl1 funscore1) (GL gl2 funscore2) = GL (gl1 ++ gl2) (funscore1 + funscore2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = case gl1 `compare` gl2 of
    GT -> gl1
    otherwise -> gl2

                    
treeFold :: (a -> [b]-> b) -> Tree a -> b
treeFold f (Node a sf) = f a (map (treeFold f) $ sf)


nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b [] = (glCons b mempty, mempty)
nextLevel b glps = (glCons b gl2, gl1)
    where 
        gl1 = foldl1 (<>) (map fst glps)
        gl2 = foldl1 (<>) (map snd glps)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel

glToList :: GuestList -> [String]
glToList (GL a e) = map empName a

main = do
    comp <- readFile "company.txt"
    print $ glToList . maxFun . read $ comp