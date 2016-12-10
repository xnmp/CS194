-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Home12 where
import Risk
import Control.Monad.Random
import Data.List
import Control.Applicative
import Control.Monad

dieRolls3 :: Rand StdGen [DieValue]
dieRolls3 = liftA3 (\x y z -> [x,y,z]) getRandom getRandom getRandom

dieRolls :: Int -> Rand StdGen [DieValue]
dieRolls n = replicateM n die

destroy :: Battlefield -> Ordering -> Battlefield
destroy (Battlefield na nd) comp = case comp of
    LT -> Battlefield na (nd - 1)
    GT -> Battlefield (na - 1) nd
    otherwise -> Battlefield na nd

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield na nd) = do
    rollsa <- dieRolls $ max 0 (na - 1)
    rollsd <- dieRolls nd
    return $ foldl destroy bf $ zipWith compare (sort rollsa) (sort rollsd)
        
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield na nd) = if na <= 1 || nd <= 0
    then return bf
    else do
        bfnew <- battle bf
        invade bfnew

successProb :: Battlefield -> Rand StdGen Double
successProb bf = (/10) . sum <$> fmap success <$> replicateM 1000 (invade bf)
    where 
        success bf = if defenders bf <= 0 then 1 else 0

-- Exercise 5 (Optional)
-- Write a function
-- exactSuccessProb :: Battlefield -> Double
-- which computes the exact probability of success based on principles
-- of probability, without running any simulations. (This won’t give you
-- any particular practice with Haskell; it’s just a potentially interesting
-- challenge in probability theory.)


main = do
    bf <- evalRandIO $ successProb (Battlefield 3 3)
    print bf