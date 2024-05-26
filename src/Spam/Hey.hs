{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Spam.Hey where

import Data.Kind

data Void = Void

type family ContextOf input where
  ContextOf Int = Void

class Evaluate ctx input output | input -> output where
  evaluate :: ContextOf ctx -> input -> output

class Convert ctx input | input -> ctx where
  convert :: input -> String

-- class (Evaluate ctx input output, Convert ctx input) => Salute ctx input output
class (Evaluate c i n, Convert c i) => HeyConstraint c i n

instance (Evaluate c i n, Convert c i) => HeyConstraint c i n

instance Evaluate ctx Bool Bool where
  evaluate :: ContextOf ctx -> Bool -> Bool
  evaluate _ x = x

instance Convert Type Bool where
  convert True = "true"
  convert False = "false"

instance Evaluate ctx Int Int where
  evaluate _ x = x

instance Convert Void Int where
  convert = show

data Operation c n where
  Add :: forall c i i' n. (Num n, HeyConstraint c i n, HeyConstraint (ContextOf c) i' n) => i -> i' -> Operation c n
  Sub :: forall c i i' n. (Num n, HeyConstraint c i n, HeyConstraint (ContextOf c) i' n) => i -> i' -> Operation c n
  Mul :: forall c i i' n. (Num n, HeyConstraint c i n, HeyConstraint (ContextOf c) i' n) => i -> i' -> Operation c n

instance Convert c (Operation c Int) where
  convert (Add x y) = "(" <> convert x <> "+" <> convert y <> ")"
  convert (Sub x y) = "(" <> convert x <> "-" <> convert y <> ")"
  convert (Mul x y) = "(" <> convert x <> "*" <> convert y <> ")"

-- instance (Num n) => Evaluate ctx (Operation ctx n) n where
--   evaluate :: ContextOf ctx -> Operation ctx n -> n
--   evaluate c (Add x y) = evaluate c x + evaluate (ContextOf c) y
--   evaluate c (Sub x y) = evaluate c x - evaluate c y
--   evaluate c (Mul x y) = evaluate c x * evaluate c y

-- data Function c o where
--   Function :: forall c i o. (HeyConstraint c i o) => i -> Function c o

-- instance Convert c (Function c o) where
--   convert :: Function c o -> String
--   convert (Function i) = "(x => " <> convert i <> ")"

-- (.+) :: forall c a b n. (Num n, HeyConstraint c a n, HeyConstraint c b n) => a -> b -> Operation c n
a .+ b = Add a b

a .- b = Sub a b

a .* b = Mul a b