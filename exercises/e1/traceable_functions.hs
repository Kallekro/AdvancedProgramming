import Control.Monad

newtype Trace a = T (a, String)

instance Monad Trace where
    (T p) >>= f = let (x, t) = p
                      T (x2, t2) = f x
                  in T (x2, t++t2)
    return x = T (x, "")

instance Functor Trace where
    fmap = liftM
instance Applicative Trace where
    pure = return; (<*>) = ap

traceable :: String -> (t -> a) -> t -> Trace a
traceable name f = \x -> T(f x, name ++" called. ")

double :: Int -> Int
double x = x + x
triple :: Int -> Int
triple x = x + x + x

tdouble = (traceable "double" double)
ttriple = (traceable "triple" triple)

x = 2

T (test1_val, test1_trace) = do
    y <- tdouble x
    z <- ttriple y
    w <- ttriple z
    u <- tdouble w
    return u
