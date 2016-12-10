-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Home10 where
import AParser
import Data.Maybe
import Data.Char
import Control.Applicative

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
     fmap f (Parser parg) = Parser (fmap (first f) . parg)

instance Applicative Parser where
    pure a = Parser (\x -> Just (a, x))
    p1@(Parser f1) <*> p2@(Parser f2) = Parser g
        where
            g x = case f1 x of
                Nothing -> Nothing
                Just (f, ss) -> runParser (fmap f p2) ss

abParser :: Parser (Char, Char)
abParser = fmap (curry id) (char 'a')  <*> char 'b'

abParser_ :: Parser ()
abParser_ = pure (const ()) <*> abParser

p1 :: Parser [Integer]
p1 =  fmap (\x y z -> [x,z]) posInt <*> char ' ' <*> posInt
-- actually liftA3 (\x y z -> [x, z]) posInt (char ' ') posInt


instance Alternative Parser where
    empty = Parser (\x -> Nothing)
    p1 <|> p2 = Parser g
        where
            g x = runParser p1 x <|> runParser p2 x

intOrUpperCase :: Parser ()
intOrUpperCase =  int <|> uc
    where
        int = pure (const ()) <*> posInt
        uc = pure (const ()) <*> satisfy isUpper



-- main = print $ 
main = print $ runParser (intOrUpperCase) "ASba123 456"