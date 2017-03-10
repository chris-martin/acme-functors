-- | Types are great. Lifting them into some sort of applicative functor makes
-- them even better. This module is an homage to our favorite applicatives.

{-# LANGUAGE NoImplicitPrelude #-} -- Prelude is bad
{-# LANGUAGE DeriveFunctor     #-} -- Writing Functor instances is boring

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

    ) where

import Control.Applicative ( Applicative (..) )
import Control.Monad       ( Monad       (..) )
import Data.Functor        ( Functor     (..) )
import Data.Monoid         ( Monoid      (..) )
import Data.Semigroup      ( Semigroup   (..) )


--------------------------------------------------------------------------------
--  Lifted-but-why
--------------------------------------------------------------------------------

-- | __@LiftedButWhy@__ is a boring functor that just has one value and no other
-- structure or interesting properties.

data LiftedButWhy a =
    LiftedButWhy a -- ^ A value that has been lifted for some damned reason.
                   --
                   -- ... Okay, to be honest, this one is /nobody's/ favorite,
                   -- but it is included here for completeness.
    deriving Functor

instance Applicative LiftedButWhy where

    pure = LiftedButWhy

    LiftedButWhy f <*> LiftedButWhy a = LiftedButWhy (f a)

instance Monad LiftedButWhy where

    LiftedButWhy a >>= f = f a

instance Semigroup a => Semigroup (LiftedButWhy a) where

    LiftedButWhy x <> LiftedButWhy y = LiftedButWhy (x <> y)

instance Monoid a => Monoid (LiftedButWhy a) where

    mempty = LiftedButWhy mempty

    LiftedButWhy x `mappend` LiftedButWhy y = LiftedButWhy (mappend x y)


--------------------------------------------------------------------------------
--  Or-not
--------------------------------------------------------------------------------

-- | __@OrNot@__ is somehow slightly more interesting than @LiftedButWhy@, even
-- though it may actually contain /less/. Instead of a value, there might /not/
-- be a value.

data OrNot a = ActuallyYes a -- ^ Some normal value.
             | Nope          -- ^ Chuck Testa.
    deriving Functor

instance Applicative OrNot where

    pure = ActuallyYes

    ActuallyYes f <*> ActuallyYes a = ActuallyYes (f a)
    _             <*> _             = Nope

instance Monad OrNot where

    ActuallyYes a  >>= f = f a
    Nope           >>= _ = Nope

instance Semigroup a => Semigroup (OrNot a) where

    ActuallyYes x  <> ActuallyYes y  = ActuallyYes (x <> y)
    ActuallyYes x  <> Nope           = ActuallyYes x
    Nope           <> ActuallyYes y  = ActuallyYes y
    Nope           <> Nope           = Nope

-- | This is cool, though - All you need is a semigroup for @a@, you @OrNot a@
-- gets upgraded to a monoid for free.

instance Semigroup a => Monoid (OrNot a) where

    mempty = Nope

    mappend = (<>)


--------------------------------------------------------------------------------
--  Two
--------------------------------------------------------------------------------

-- | __@Two@__ is /two/ values. Yep. Just two values.

data Two a = Two { firstOfTwo  :: a -- ^ One value.
                 , secondOfTwo :: a -- ^ Another value.
                 }
    deriving Functor

instance Applicative Two where

    pure a = Two a a

    Two f g <*> Two x y = Two (f x) (g y)


--------------------------------------------------------------------------------
--  Any-number-of
--------------------------------------------------------------------------------

-- | __@AnyNumberOf@__ starts to get exciting. Any number of values you want.
-- Zero... one ... two ... three ... four ... five ... The possibilities are
-- /truly/ endless.

data AnyNumberOf a =

    OneAndMaybeMore a (AnyNumberOf a)
    -- ^ One value, and maybe even more after that!

    | ActuallyNone -- ^ Oh. Well this is less fun.

    deriving Functor


--------------------------------------------------------------------------------
--  One-or-more
--------------------------------------------------------------------------------

-- | __@OneOrMore@__ is more restrictive than AnyNumberOf, yet somehow actually
-- /more/ interesting, because it excludes that dull situation where there
-- aren't any values at all.

data OneOrMore a = OneOrMore
    { theFirstOfMany :: a -- ^ Definitely at least this one.
    , possiblyMore :: AnyNumberOf a -- ^ And perhaps others.
    }


--------------------------------------------------------------------------------
--  Also-extra-thing
--------------------------------------------------------------------------------

-- | __@Also extraThing@__ is a functor in which each value has an @extraThing@
-- of some other type that tags along with it.

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


--------------------------------------------------------------------------------
--  Or-instead-other-thing
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
--  Or-instead-first-thing
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
--  Determined-by-parameter
--------------------------------------------------------------------------------

-- | __@DeterminedBy parameter@__ is a value that... well, we're not really sure
-- what it is. We'll find out once a @parameter@ is provided.
--
-- The mechanism for deciding /how/ the value is determined from the
-- @parameter@ is opaque; all you can do is test it with different parameters
-- and see what results. There isn't even an @Eq@ instance, which is annoying.

data DeterminedBy parameter a = Function ((->) parameter a)
    deriving Functor

instance Applicative (DeterminedBy parameter) where

    pure a = Function (\_ -> a)

    Function f <*> Function a = Function (\x -> f x (a x))

instance Monad (DeterminedBy parameter) where

    Function fa >>= ff = Function (\x -> let Function f = ff (fa x) in f x)

{-

--------------------------------------------------------------------------------
--  Notes
--------------------------------------------------------------------------------

LiftedButWhy is Identity.

OrNot is Maybe.

Two doesn't have an analogue in the standard library as far as I know.

AnyNumberOf is [], the standard cons list.

OneOrMore is NonEmpty.

Also is (,), the 2-tuple.

OrInstead is AccValidation from the 'validation' package.

OrInsteadFirst is Either.

DeterminedBy is (->) also known as a function, whose functor is also known as
Reader.

-}
