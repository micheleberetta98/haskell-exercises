{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises where

import           Data.Kind (Constraint, Type)

-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.





{- ONE -}

data Nat = Z | S Nat

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':

type family (x :: Nat) + (y :: Nat) :: Nat where
  'Z + y   = y
  'S x + y = 'S (x + y)

-- | b. Write a type family '**' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?

type family (x :: Nat) ** (y :: Nat) :: Nat where
  'Z ** _   = 'Z
  'S x ** y = y + (x ** y)

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | c. Write a function to add two 'SNat' values.

add :: SNat x -> SNat y -> SNat (x + y)
add SZ y     = y
add (SS x) y = SS (add x y)



{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

append :: Vector m a -> Vector n a -> Vector (m + n) a
append VNil ys         = ys
append (VCons x xs) ys = VCons x (append xs ys)

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

flatMap :: Vector n a -> (a -> Vector m b) -> Vector (n ** m) b
flatMap VNil _         = VNil
flatMap (VCons x xs) f = append (f x) (flatMap xs f)




{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.

type family (a :: Bool) && (b :: Bool) :: Bool where
  'False && _ = 'False
  'True && b  = b

-- | b. Write the type-level @||@ function for booleans.

type family (a :: Bool) || (b :: Bool) :: Bool where
  'True || _  = 'True
  'False || b = b

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.

type family All (xs :: [Bool]) :: Bool where
  All '[]       = 'True
  All (b ': bs) = b && All bs


{- FOUR -}

-- | a. Nat fun! Write a type-level 'compare' function using the promoted
-- 'Ordering' type.

type family Compare (x :: Nat) (y :: Nat) :: Ordering where
  Compare 'Z 'Z         = 'EQ
  Compare 'Z ('S n)     = 'LT
  Compare ('S n) 'Z     = 'GT
  Compare ('S n) ('S m) = Compare n m

-- | b. Write a 'Max' family to get the maximum of two natural numbers.

type family Max (x :: Nat) (y :: Nat) :: Nat where
  Max x y = Max' (Compare x y) x y

type family Max' (r :: Ordering) (x :: Nat) (y :: Nat) where
  Max' 'LT _ y = y
  Max' _ x _   = x

-- | c. Write a family to get the maximum natural in a list.

type family Maximum (xs :: [Nat]) :: Nat where
  Maximum '[]       = 'Z
  Maximum (x ': xs) = Max x (Maximum xs)



{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.

type family Insert (x :: Nat) (t :: Tree) :: Tree where
  Insert x 'Empty        = 'Node 'Empty x 'Empty
  Insert x ('Node l n r) = Insert' (Compare x n) x ('Node l n r)

type family Insert' (c :: Ordering) (x :: Nat) (node :: Tree) :: Tree where
  Insert' 'LT x ('Node l n r) = 'Node (Insert x l) n r
  Insert' 'GT x ('Node l n r) = 'Node l n (Insert x r)
  Insert' 'EQ _ node          = node

{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.

type family Delete (x :: Nat) (t :: Tree) :: Tree where
  Delete _ 'Empty        = 'Empty
  Delete x ('Node l n r) = Delete' (Compare x n) x ('Node l n r)

type family Delete' (o :: Ordering) (x :: Nat) (t :: Tree) :: Tree where
  Delete' 'LT x ('Node l n r)      = 'Node (Delete x l) n r
  Delete' 'GT x ('Node l n r)      = 'Node l n (Delete x r)
  Delete' 'EQ _ ('Node 'Empty _ r) = r
  Delete' 'EQ _ ('Node l _ r)      = Repair (Biggest l) r

type family Repair (parts :: (Nat, Tree)) (t :: Tree) :: Tree where
  Repair '(n, l) r = 'Node l n r

type family Biggest (t :: Tree) :: (Nat, Tree) where
  Biggest ('Node l n 'Empty) = '(n, l)
  Biggest ('Node l n r)      = Biggest' l n (Biggest r)

type family Biggest' (l :: Tree) (n :: Nat) (r' :: (Nat, Tree)) :: (Nat, Tree) where
  Biggest' l n '(x, r) = '(x, 'Node l n r)

{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.





{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: [Type]) :: Constraint where
  -- ...

-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?





{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

-- | b. Write a type-level prime number sieve.

-- | c. Why is this such hard work?
