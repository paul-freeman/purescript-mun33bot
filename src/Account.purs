module Account
  ( Account
  , mkAccount
  )
where

import Prelude

import Data.Foldable (class Foldable, foldr)

newtype Account =
  Account
  { n :: Int
  }

instance showAccount :: Show Account where
  show (Account a) = show $ "Account: " <> show a

mkAccount :: forall f a. Foldable f => f a -> Account
mkAccount entries =
  Account
  { n: foldr (\_ a -> 1 + a) 0 entries
  }

-- Lenses

_Account :: forall f. Functor f => ({ n :: Int } -> f { n :: Int }) -> Account -> f Account
_Account f (Account o) = Account <$> f o

n :: forall f a b. Functor f => (a -> f b) -> { n :: a } -> f { n :: b }
n f o = f o.n <#> \n' -> o{n = n'}
