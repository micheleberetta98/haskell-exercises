module Exercises where

class PopQuiz a

-- | Which of the following instances require 'FlexibleInstances'? Don't cheat
-- :D This is a tricky one, but look out for nested concrete types!

-- instance PopQuiz Bool            -- ok
-- instance PopQuiz [Bool]          -- needs extension
-- instance PopQuiz [a]             -- ok
-- instance PopQuiz (a, b)          -- ok
-- instance PopQuiz [(a, b)]        -- needs extension
-- instance PopQuiz (IO a)          -- ok

newtype RIO  r a = RIO (r -> IO a) -- Remember, this is a /new type/.
type    RIO' r a =      r -> IO a

-- instance PopQuiz (RIO Int a)     -- needs extension
-- instance PopQuiz (RIO r a)       -- ok
-- instance PopQuiz (RIO' r a)      -- needs extension
-- instance PopQuiz (r -> IO a)     -- needs extension
-- instance PopQuiz (a -> b)        -- ok
-- instance PopQuiz (a -> b -> c)   -- needs extension
-- instance PopQuiz (a, b, c)       -- ok
-- instance PopQuiz (a, (b, c))     -- needs extension
-- instance PopQuiz ()              -- ok
-- instance PopQuiz (a, b, c, a)    -- needs extension

data Pair  a = Pair  a  a
type Pair' a =      (a, a)

-- instance PopQuiz (a, a)          -- needs extension
-- instance PopQuiz (Pair a)        -- ok
-- instance PopQuiz (Pair' a)       -- needs extension
