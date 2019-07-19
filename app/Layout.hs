{-# LANGUAGE FlexibleInstances #-}

module Layout(Layout,lay,scope,newline,toString) where

import Control.Monad (ap,liftM)
import qualified Data.List as List

-- TODO: recode using Monad Transformers
data Layout a = Layout (Int -> Int -> (a, Int, [String]))

instance Show (Layout ()) where
    show = toString

instance Functor Layout where fmap = liftM
instance Applicative Layout where pure = return; (<*>) = ap

instance Monad Layout where
  return x = Layout $ \_ n -> (x, n, [])
  (>>=) = bind

bind :: Layout a -> (a -> Layout b) -> Layout b
bind (Layout left) f = Layout $ \m n -> do
    let (a,n',xs1) = left m n
    let (Layout right) = f a
    let (b,n'',xs2) = right m n'
    (b,n'',xs1 ++ xs2)

lay :: String -> Layout ()
lay s = Layout $ \_ n -> ((), n + length s, [s])

newline :: Layout ()
newline = Layout $ \m _ -> ((), m, ["\n" <> List.take m (List.repeat ' ')])

scope :: Layout () -> Layout ()
scope (Layout f) = Layout $ \_ n -> f n n

toString :: Layout () -> String
toString (Layout f) = clean $ concat xs where ((),_,xs) = f 0 0

clean :: String -> String
clean = unlines . map (reverse . dropWhile (== ' ') . reverse) . lines
