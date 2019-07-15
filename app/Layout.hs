
module Layout(Layout,lay,mark,newline,toString) where

import Control.Monad (ap,liftM)
import qualified Data.List as List

-- TODO: recode using Monad Transformers
data Layout a = Layout (Int -> Int -> (a, Int, [String]))

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

mark :: Layout () -> Layout ()
mark (Layout f) = Layout $ \_ n -> f n n

toString :: Layout () -> String
toString (Layout f) = concat xs where ((),_,xs) = f 0 0
