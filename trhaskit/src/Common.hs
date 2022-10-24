module Common (
(|>),
apply,
applyTo,
andThen,
) where

import Control.Arrow qualified

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x
{-# INLINE (|>) #-}

-- | A named version of the applicative apply operator `<*>`.
apply :: Applicative f => f (a -> b) -> f a -> f b
apply fun val = fun <*> val

-- | A named version of the reverse applicative apply operator `<**>`.
-- | This function is pipable because it takes the function as last argument.
applyTo :: Applicative f => f a -> f (a -> b) -> f b
applyTo fun val = val <*> fun

-- | A version of `>>=` that is pipable
-- | because it takes the function as first argument
andThen :: Monad m => (a -> m b) -> m a -> m b
andThen fun val = val >>= fun