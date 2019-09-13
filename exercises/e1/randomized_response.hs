import qualified System.Random as R
import Control.Monad.State.Lazy

rng :: R.StdGen
rng = R.mkStdGen 123

binary_random_response :: R.StdGen -> Bool -> (Bool, R.StdGen)
binary_random_response g true_answer =
  if first_coin then (true_answer, g'')
  else (second_coin, g'')
  where (first_coin, g') = R.random g
        (second_coin, g'') = R.random g'

test0 = binary_random_response rng False
test1 = binary_random_response rng True

random_response :: R.StdGen -> a -> [a] -> (a, R.StdGen)
random_response g true_answer all_answers =
  if coin then (true_answer, g'')
  else (all_answers !! idx, g'')
  where (coin, g') = R.random g
        (idx, g'') = R.randomR (0, (length all_answers)-1) g'

test2 = random_response rng "Porthos" ["Athos", "Porthos", "Aramis"]

genRandom :: (R.StdGen -> (a, R.StdGen)) -> State R.StdGen a
genRandom f = do
  g <- get
  let (rnd, g') = f g
  put g'
  return rnd

binary_random_response_StateMonad :: R.StdGen -> Bool -> (Bool, R.StdGen)
binary_random_response_StateMonad g true_answer =
  runState (
    do
      first <- genRandom R.random
      second <- genRandom R.random
      if first then return true_answer
      else return second
  ) g

random_response_StateMonad :: R.StdGen -> a -> [a] -> (a, R.StdGen)
random_response_StateMonad g true_answer all_answers =
  runState (
    do
      first <- genRandom R.random
      idx <- genRandom (R.randomR (0, (length all_answers)-1))
      if first then return true_answer
      else return (all_answers !! idx)
  ) g
