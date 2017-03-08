{- | Types are great. Lifting them into some sort of applicative functor makes
them even better. This module is an homage to our favorite applicatives. -}

{-# LANGUAGE NoImplicitPrelude #-} -- Prelude is bad
{-# LANGUAGE DeriveFunctor     #-} -- Deriving Functor is boring

module Acme.Functors
    (
    -- * Lifted-but-why
      LiftedButWhy (..)

    -- * Or-not
    , OrNot (..)

    -- * Two
    , Two (..)

    -- * Any-number-of
    , AnyNumberOf (..)

    -- * One-or-more
    , OneOrMore (..)

    -- * Also-extra-thing
    , Also (..)

    -- * Or-instead-other-thing
    , OrInstead (..)

    -- * Or-instead-other-thing ("first" variant)
    , OrInsteadFirst (..)

    -- * Determined-by-parameter
    , DeterminedBy (..)

    -- * Determined-by-mutable-parameter
    , DeterminedByMutable (..)

    ) where

import Control.Applicative ( Applicative (..) )
import Control.Monad       ( Monad       (..) )
import Data.Functor        ( Functor     (..) )
import Data.Monoid         ( Monoid      (..) )
import Data.Semigroup      ( Semigroup   (..) )

-- | __@LiftedButWhy@__ is a boring functor that just has one value and no other
-- structure or interesting properties.
data LiftedButWhy a =
    LiftedButWhy a -- ^ A value that has been lifted for some damned reason.
                   --
                   -- ... Okay, to be honest, this one is /nobody's/ favorite,
                   -- but it is included here for completeness.

-- | __@OrNot@__ is somehow slightly more interesting than @LiftedButWhy@, even
-- though it may actually contain /less/. Instead of a value, there might /not/
-- be a value.
data OrNot a = ActuallyYes a -- ^ Some normal value.
             | Nope          -- ^ Chuck Testa.

-- | __@Two@__ is /two/ values. Yep. Just two values.
data Two a = Two { firstOfTwo  :: a -- ^ One value.
                 , secondOfTwo :: a -- ^ Another value.
                 }

-- | __@AnyNumberOf@__ starts to get exciting. Any number of values you want.
-- Zero... one ... two ... three ... four ... five ... The possibilities are
-- /truly/ endless.
data AnyNumberOf a =

    OneAndMaybeMore a (AnyNumberOf a)
    -- ^ One value, and maybe even more after that!

    | ActuallyNone -- ^ Oh. Well this is less fun.

{- | __@OneOrMore@__ is more restrictive than AnyNumberOf, yet somehow actually
/more/ interesting, because it excludes that dull situation where there aren't
any values at all. -}

data OneOrMore a = OneOrMore
    { theFirstOfMany :: a -- ^ Definitely at least this one.
    , possiblyMore :: AnyNumberOf a -- ^ And perhaps others.
    }

{- | __@Also extraThing@__ is a functor in which each value has an @extraThing@
of some other type that tags along with it. -}

data (Also extraThing) a = Also
    { theExtraThing     :: extraThing -- ^ An additional thing that tags along.
    , withoutExtraThing :: a          -- ^ A value.
    }
    deriving Functor

-- | Dragging the @extraThing@ along can be a bit of a burden. It prevents @Also
-- extraThing@ from being an applicative functor — unless the @extraThing@ can
-- pull its weight by bringing a monoid to the table.
instance Monoid extraThing => Applicative (Also extraThing) where

    pure = Also mempty

    Also extra1 f <*> Also extra2 a = Also (mappend extra1 extra2) (f a)

-- | __@OrInstead otherThing@__ is a functor in which, instead of having a
-- value, can actually just have some totally unrelated @otherThing@ instead.

data (OrInstead otherThing) a =
      NotInstead a       -- ^ Some normal value.
    | Instead otherThing -- ^ Some totally unrelated other thing.
    deriving Functor

-- | The possibility of having an @otherThing@ obstructs this functor's ability
-- to be applicative, much like the extra thing in @Also extraThing@ does. In
-- this case, since we do not need an empty value for the @otherThing@, it needs
-- only a semigroup to deal with the issue.
instance Semigroup otherThing => Applicative (OrInstead otherThing) where

    pure = NotInstead

    NotInstead f   <*> NotInstead a   = NotInstead (f a)
    Instead other1 <*> Instead other2 = Instead (other1 <> other2)
    Instead other  <*> _              = Instead other
    _              <*> Instead other  = Instead other

-- | __@OrInsteadFirst otherThing@__ looks a lot like @OrInstead otherThing@,
-- but it manages to always be an applicative functor — and even a monad too —
-- by handling the @otherThing@s a bit more hamfistedly. @OrInsteadFirst
-- otherThing@ merely retains the first @otherThing@ it encounters, ignoring any
-- additional @otherThings@ that may subsequently pop up while dealing with
-- lifted values.

data (OrInsteadFirst otherThing) a =
      NotInsteadFirst a       -- ^ Some normal value.
    | InsteadFirst otherThing -- ^ Some totally unrelated other thing.
    deriving Functor

instance Applicative (OrInsteadFirst otherThing) where

    pure = NotInsteadFirst

    NotInsteadFirst f  <*> NotInsteadFirst a  = NotInsteadFirst (f a)
    InsteadFirst other <*> _                  = InsteadFirst other
    _                  <*> InsteadFirst other = InsteadFirst other

instance Monad (OrInsteadFirst otherThing) where

    InsteadFirst other >>= _ = InsteadFirst other
    NotInsteadFirst a  >>= f = f a

-- | todo
data DeterminedBy parameter a = Function ((->) parameter a)
    deriving Functor

instance Applicative (DeterminedBy parameter) where

    pure a = Function (\_ -> a)

    Function f <*> Function a = Function (\x -> f x (a x))

instance Monad (DeterminedBy parameter) where

    Function fa >>= ff = Function (\x -> let Function f = ff (fa x) in f x)

-- | todo
data DeterminedByMutable parameter a =
    DeterminedByMutable (parameter -> (Also parameter) a)
    deriving Functor

-- todo - unit?

-- | __@Just otherThing@__ - todo
data Just otherThing a = Just otherThing
