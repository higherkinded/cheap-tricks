{-# LANGUAGE GADTs
           , DataKinds
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}
module TypeLevelInts
  ( TInt (..)
  , Parity (..)
  , Fold
  , Neg
  , type (:+)
  , type (:-)
  , type (:*)
  ) where

infixl 6 :+, :-
infixl 7 :*

data TInt
  -- | Zero.
  = Z
  -- | Successor. `S a` is equivalent to a + 1.
  | S TInt
  -- | Predecessor. `P a` is equivalent to a - 1.
  | P TInt
  -- | Undefined. Can be useful for surjective maps.
  | Undefined
  deriving (Show, Eq)

type N1     = 'S 'Z
type N2     = 'S N1
type N3     = 'S N2
type N4     = 'S N3
type N5     = 'S N4
type N6     = 'S N5
type N7     = 'S N6
type N8     = 'S N7
type N9     = 'S N8

data Parity
  = Odd
  | Even
  deriving (Show, Eq)

-- | Helps dealing with obviously dumb cases by reducing the number
-- to the homogeneous sequence of predecessors or successors.
type family Fold a where
  Fold ('S ('P a)) = a
  Fold ('P ('S a)) = a
  Fold ('S 'Z) = N1
  Fold ('S N1) = N2
  Fold ('S N2) = N3
  Fold ('S N3) = N4
  Fold ('S N4) = N5
  Fold ('S N5) = N6
  Fold ('S N6) = N7
  Fold ('S N7) = N8
  Fold ('S N8) = N9
  Fold a = a

-- | Parity, simple stuff
type family GetParity a where
  GetParity 'Z          = 'Even
  GetParity ('S 'Z)     = 'Odd
  GetParity ('S ('S a)) = GetParity a

-- | Negates a type-level number.
type family Neg b where
  Neg 'Z = 'Z
  Neg ('S a) = 'P (Neg (Fold a))
  Neg ('P a) = 'S (Neg (Fold a))

-- | Sums two type-level numbers.
type family a :+ b where
  -- Identity.
  a :+ 'Z      = (Fold a)
  -- Recursive addition of positives.
  a :+ 'S b    = 'S (Fold a) :+ (Fold b)
  -- Elimination, 1 - 1.
  'S a :+ 'P b = (Fold a) :+ (Fold b)
  -- Recursive addition of negatives.
  a :+ 'P b    = 'P (Fold a) :+ (Fold b)
  -- Commutativity definition.
  a :+ b       = (Fold b) :+ (Fold a)

-- | Subtracts one type-level number from another.
-- Defined in terms of rewrites to `:+`
type family a :- b where
  a :- b    = Fold (Neg (Fold b) :+ (Fold a))

-- | Multiplies two type-level numbers.
type family a :* b where
  a :* 'S 'Z = Fold a
  a :* 'Z = 'Z
  a :* 'P b = Neg (Fold a) :* Fold ('S (Neg b))
  a :* 'S b = Fold (a :+ a :* (Fold b))

-- | Everyone's favourite stuff.
type family Factorial a where
  Factorial 'Z      = 'S 'Z
  Factorial ('P 'Z) = 'Undefined
  Factorial ('P a)  = Factorial (Fold a)
  Factorial a  = (Fold a) :* Factorial (Fold (a :- N1))
