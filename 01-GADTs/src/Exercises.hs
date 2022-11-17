{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
module Exercises where





{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where
  count :: a -> Int

instance Countable Int where
  count = id

instance Countable [a] where
  count = length
instance Countable Bool where
  count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CountNil  :: CountableList
  CountCons :: Countable a => a -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CountNil         = 0
countList (CountCons x xs) = count x + countList xs


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero CountNil         = CountNil
dropZero (CountCons x xs)
  | count x == 0          = dropZero xs
  | otherwise             = x `CountCons` dropZero xs


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

-- Can't write, we don't know the type of the elements
filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil  :: AnyList
  AnyCons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList AnyNil         = AnyNil
reverseAnyList (AnyCons x xs) = reverseAnyList xs `anyConcat` AnyCons x AnyNil
  where
    anyConcat AnyNil ys         = ys
    anyConcat (AnyCons x xs) ys = AnyCons x (anyConcat xs ys)

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = error "Can't implement"

lengthAnyList :: AnyList -> Int
lengthAnyList AnyNil         = 0
lengthAnyList (AnyCons _ xs) = 1 + lengthAnyList xs

foldAnyList :: Monoid m => AnyList -> m
foldAnyList = error "Can't implement"

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList AnyNil = True
isEmptyAnyList _      = False

-- Can't show the elements, but can show the structure

instance Show AnyList where
  show AnyNil         = "[]"
  show (AnyCons _ xs) = "_ : " ++ show xs





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- input, the only thing we can do is transforming it to output

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

-- We can check if the result is the same (provided Eq output)

instance Eq output => Eq (TransformableTo output) where
  TransformWith f x == TransformWith g y = f x == g y

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where
  fmap f (TransformWith g x) = TransformWith (f . g) x



{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

areEqual :: EqPair -> Bool
areEqual (EqPair a b) = a == b

areNotEqual :: EqPair -> Bool
areNotEqual = not . areEqual

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair' a where
  EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

-- We still need the (Eq a) constraint



{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox x _)) = x

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox         = 0
countLayers (IntBox _ xs)    = 1 + countLayers xs
countLayers (StringBox _ xs) = 1 + countLayers xs
countLayers (BoolBox _ xs)   = 1 + countLayers xs

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- removeLayer :: MysteryBox a -> MysteryBox b
-- removeLayer (IntBox _ b) = b
-- Can't write it, we don't know the underlying type in the signature


{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

hHead :: HList (h, t) -> h
hHead (HCons x _) = x

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = error "Can't pattern match on this"

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- Can't do this
-- hAppend :: HList (a, b) -> HList (c, d) -> HList (a, (b, (c, d)))
-- hAppend HNil ys         = ys
-- hAppend (HCons x xs) ys = HCons x (hAppend xs ys)



{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty :: HTree Empty
  HBranch :: HTree l -> centre -> HTree r -> HTree (Branch l centre r)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft :: HTree (Branch l c r) -> HTree (Branch Empty c r)
deleteLeft (HBranch _ centre right) = HBranch HEmpty centre right

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree Empty) where
  _ == _ = True

instance (Eq (HTree l), Eq c, Eq (HTree r)) => Eq (HTree (Branch l c r)) where
  HBranch l1 c1 r1 == HBranch l2 c2 r2 = l1 == l2 && c1 == c2 && r1 == r2


{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil :: AlternatingList a b
  ACons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts ANil         = []
getFirsts (ACons x xs) = x : getSeconds xs

getSeconds :: AlternatingList a b -> [b]
getSeconds ANil         = []
getSeconds (ACons _ xs) = getFirsts xs

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues xs = (mconcat (getFirsts xs), mconcat (getSeconds xs))

foldValues' :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues' ANil         = (mempty, mempty)
foldValues' (ACons x xs) = let (b, a) = foldValues' xs in (x <> a, b)


{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (Equals x y)             = eval x == eval y
eval (Add x y)                = eval x + eval y
eval (If cond ifTrue ifFalse) = if eval cond then eval ifTrue else eval ifFalse
eval (IntValue x)             = x
eval (BoolValue x)            = x

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

data Typed = IntType (Expr Int) | BoolType (Expr Bool)

tidy :: DirtyExpr -> Maybe Typed
tidy (DirtyEquals a b) = do
  x <- tidy a
  y <- tidy b
  case (x, y) of
    (IntType x', IntType y') -> Just (BoolType (Equals x' y'))
    _                        -> Nothing
tidy (DirtyAdd a b) = do
  x <- tidy a
  y <- tidy b
  case (x, y) of
    (IntType x', IntType y') -> Just (IntType (Add x' y'))
    _                        -> Nothing
tidy (DirtyIf a b c) = do
  x <- tidy a
  y <- tidy b
  z <- tidy c
  case (x, y, z) of
    (BoolType x', IntType y', IntType z')   -> Just (IntType (If x' y' z'))
    (BoolType x', BoolType y', BoolType z') -> Just (BoolType (If x' y' z'))
    _                                       -> Nothing
tidy (DirtyIntValue x) = Just (IntType (IntValue x))
tidy (DirtyBoolValue x) = Just (BoolType (BoolValue x))

parse :: DirtyExpr -> Maybe (Expr Int)
parse expr = case tidy expr of
  Just (IntType expr') -> Just expr'
  _                    -> Nothing

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TANil  :: TypeAlignedList a a
  TACons :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c

-- | b. Which types are existential?

-- (a -> b), TypeAlignedList b c

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs fs TANil         = fs
composeTALs fs (TACons g gs) = TACons g (composeTALs fs gs)

