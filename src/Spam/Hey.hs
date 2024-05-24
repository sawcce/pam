{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Spam.Hey where

data Void = Void

class Evaluate ctx input output where
  evaluate :: ctx -> input -> output

class Convert ctx input where
  convert :: ctx -> input -> String

-- class (Evaluate ctx input output, Convert ctx input) => Salute ctx input output
class (Evaluate c i n, Convert c i) => HeyConstraint c i n

instance (Evaluate c i n, Convert c i) => HeyConstraint c i n

instance Evaluate ctx Bool Bool where
  evaluate _ x = x

instance Convert ctx Bool where
  convert _ True = "true"
  convert _ False = "false"

instance Evaluate ctx Int Int where
  evaluate _ x = x

instance Convert ctx Int where
  convert _ = show

data Operation c n where
  Add :: forall c i i' n. (Num n, HeyConstraint c i n, HeyConstraint c i' n) => i -> i' -> Operation c n
  Sub :: forall c i i' n. (Num n, HeyConstraint c i n, HeyConstraint c i' n) => i -> i' -> Operation c n
  Mul :: forall c i i' n. (Num n, HeyConstraint c i n, HeyConstraint c i' n) => i -> i' -> Operation c n

instance Evaluate ctx (Operation ctx n) n where
  evaluate :: ctx -> Operation ctx n -> n
  evaluate c (Add x y) = evaluate c x + evaluate c y
  evaluate c (Sub x y) = evaluate c x - evaluate c y
  evaluate c (Mul x y) = evaluate c x * evaluate c y

instance Convert ctx (Operation ctx n) where
  convert :: ctx -> Operation ctx n -> String
  convert c (Add x y) = "(" <> convert c x <> "+" <> convert c y <> ")"
  convert c (Sub x y) = "(" <> convert c x <> "-" <> convert c y <> ")"
  convert c (Mul x y) = "(" <> convert c x <> "*" <> convert c y <> ")"

-- (.+) :: forall c a b n. (Num n, HeyConstraint c a n, HeyConstraint c b n) => a -> b -> Operation c a b n
a .+ b = Add a b

a .- b = Sub a b

a .* b = Mul a b