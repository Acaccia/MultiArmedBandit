module Main where

import           Protolude

import           Control.Monad.Random
import           Data.Random.Normal
import qualified Data.Sequence                          as Seq
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy          hiding (identity)

main :: IO ()
main = do
    chart 1 =<< concurrently
          (evalRandReader (iterateM5000 softmaxStep initialJAS) (repeat 1) sigmasA <$> getStdGen)
          (evalRandReader (iterateM5000 (fmqStep 2) initialJAS) decreasingT sigmasA <$> getStdGen)
    chart 2 =<< concurrently
          (evalRandReader (iterateM5000 softmaxStep initialJAS) decreasingT sigmasB <$> getStdGen)
          (evalRandReader (iterateM5000 (fmqStep 2) initialJAS) (repeat 1) sigmasB <$> getStdGen)
    chart 3 =<< concurrently
          (evalRandReader (iterateM5000 softmaxStep initialJAS) (repeat 1) sigmasC <$> getStdGen)
          (evalRandReader (iterateM5000 (fmqStep 2) initialJAS) decreasingT sigmasC <$> getStdGen)
  where decreasingT = [exp x * 500 + 1 | x <- [0,(-1)..]]

data Sigmas = Sigmas {s0 :: Double, s1 :: Double, s :: Double}
data Action = One | Two | Three deriving (Eq, Ord, Enum, Show)
data JoinedActions = JA { action1     :: Action
                        , action2     :: Action
                        , totalReward :: Double
                        , played      :: Int
                        } deriving (Show, Eq)

instance Ord JoinedActions where
  compare = compare `on` getRatio

getRatio :: JoinedActions -> Double
getRatio (JA _ _ _ 0) = 0
getRatio (JA _ _ m n) = m / fromIntegral n

type RandReader = StateT [Double] (ReaderT Sigmas (Rand StdGen))
evalRandReader :: RandReader a -> [Double] -> Sigmas -> StdGen -> a
evalRandReader rrdr ds = evalRand . runReaderT (evalStateT rrdr ds)

data JAS = JAS { actions     :: Seq JoinedActions
               , turnRewards :: Seq Double
               } deriving (Show)

jasIndex :: (Action, Action) -> Int
jasIndex (a, b) = fromEnum a * 3 + fromEnum b

sigmasA = Sigmas 0.2 0.2 0.2
sigmasB = Sigmas 4 0.1 0.1
sigmasC = Sigmas 0.1 4 0.1

initialJAS :: JAS
initialJAS = JAS (Seq.fromList [JA a b 0 0 | [a, b] <- replicateM 2 [One, Two, Three]]) (Seq.singleton 0)

reward :: Action -> Action -> RandReader Double
reward a b = case (a, b) of
    (One, One)     -> norm 11 s0
    (One, Two)     -> norm (-30) s
    (One, Three)   -> norm 0 s
    (Two, One)     -> norm (-30) s
    (Two, Two)     -> norm 7 s1
    (Two, Three)   -> norm 0 s
    (Three, One)   -> norm 0 s
    (Three, Two)   -> norm 6 s
    (Three, Three) -> norm 5 s
  where norm mu sig = asks sig >>= lift . lift . liftRand . normal' . (,) mu

updateJA :: JoinedActions -> RandReader (JoinedActions, Double)
updateJA (JA a b r p) = do
    gain <- reward a b
    return (JA a b (r+gain) (p+1), gain)

updateJAS :: JAS -> Int -> RandReader JAS
updateJAS (JAS j rs) idx = do
    (a, r) <- updateJA $! Seq.index j idx
    return $! JAS (Seq.update idx a j) (rs Seq.|> r)

softmaxStep ::JAS -> RandReader JAS
softmaxStep j = do
  (temp:rest) <- get
  put rest
  softmaxActionsSelection temp j >>= updateJAS j

softmaxActionsSelection :: Double -> JAS -> RandReader Int
softmaxActionsSelection temp (JAS jas _) = do
    let (JA a1 a2 _ _) = maximum jas
    (g, h) <- liftA2 (,) getRandom getRandom
    let jas1 = Seq.filter ((== a2) . action2) jas
    let b1 = actionSelect action1 jas1 g
    let jas2 = Seq.filter ((== a1) . action1) jas
    let b2 = actionSelect action2 jas2 h
    return $ jasIndex (b1, b2)
  where probs j = let p = (exp . (/ temp) . getRatio) <$!> j
                      t = sum p
                  in Seq.scanl1 (+) $! (/t) <$!> p
        actionSelect a j p = a . Seq.index j . length . Seq.takeWhileL (<= p) $ probs j

fmqStep :: Double -> JAS -> RandReader JAS
fmqStep c j = do
  (temp:rest) <- get
  put rest
  fmqSelectionAction temp c j >>= updateJAS j

fmqSelectionAction :: Double -> Double -> JAS -> RandReader Int
fmqSelectionAction temp c (JAS jas _) = do
    let maxja@(JA a1 a2 _ _) = maximum jas
    let jas1 = rowForAction a1
    let jas2 = colForAction a2
    (g, h) <- liftA2 (,) getRandom getRandom
    let b1 = actionSelect action1 jas1 g evRow
    let b2 = actionSelect action2 jas2 h evCol
    return $ jasIndex (b1, b2)
  where rowForAction a = Seq.fromList $ (Seq.index jas . jasIndex) <$> [(a, b) | b <- [One, Two, Three]]
        colForAction b = Seq.fromList $ (Seq.index jas . jasIndex) <$> [(a, b) | a <- [One, Two, Three]]
        maxRowForAct = maximum . rowForAction
        maxColForAct = maximum . colForAction
        evRow ja = let j = maxColForAct (action2 ja) in getRatio ja + c * fromIntegral (played j) * getRatio j
        evCol ja = let j = maxRowForAct (action1 ja) in getRatio ja + c * fromIntegral (played j) * getRatio j
        probs j ev = let p = (exp . (/ temp) . ev) <$!> j
                         t = sum p
                     in Seq.scanl1 (+) $! (/t) <$!> p
        actionSelect a j p ev = a . Seq.index j . length . Seq.takeWhileL (<= p) $ probs j ev

iterateMN :: (Monad m) => Int -> (a -> m a) -> a -> m a
iterateMN n f = go n
  where go 0 acc = return acc
        go i acc = f acc >>= go (i - 1)

iterateM5000 :: (Monad m) => (a -> m a) -> a -> m a
iterateM5000 = iterateMN 5000

chart :: Int -> (JAS, JAS) -> IO ()
chart n (JAS _ r, JAS _ s) = toFile fo ("reward" `mappend` show n `mappend` ".svg") $ do
    layout_title .= "Collected reward over time (case " `mappend` show n `mappend` ")"
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= opaque black
    layout_left_axis_visibility . axis_show_ticks .= False
    plot (line "softmax 1" [addIndexes (values r)])
    plot (line "FMQ" [addIndexes (values s)])
  where fo = FileOptions (800, 600) SVG
        values = toList . Seq.scanl1 (+)
