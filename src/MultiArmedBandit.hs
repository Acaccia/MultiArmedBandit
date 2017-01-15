{-# LANGUAGE BangPatterns #-}

module Main where

import           Protolude

import           Control.Monad.Random
import           Data.Maybe                             (fromJust)
import           Data.Random.Normal
import qualified Data.Sequence                          as Seq
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy          hiding (identity)
import           System.Environment                     (getArgs)

{-# ANN module ("HLint: ignore Reduce duplication" :: FilePath) #-}

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    ("1":_) -> runMAB mab1
    ("2":_) -> runMAB mab2
    ("3":_) -> runMAB' mab1
    _       -> print "Please put the exercise number (1, 2 or 3)"
  print "Goodbye"

runMAB :: MultiArmedBandit -> IO ()
runMAB mab = do
  (rdm, eg1, eg2, eg3, sm1, sm2) <- runConcurrently $ (,,,,,)
        <$> Concurrently (run1000algo randomAlgo mab [] <$!> getStdGen)
        <*> Concurrently (run1000algo egreedyAlgo mab (repeat 0) <$!> getStdGen)
        <*> Concurrently (run1000algo egreedyAlgo mab (repeat 0.1) <$!> getStdGen)
        <*> Concurrently (run1000algo egreedyAlgo mab (repeat 0.2) <$!> getStdGen)
        <*> Concurrently (run1000algo softmaxAlgo mab (repeat 1) <$!> getStdGen)
        <*> Concurrently (run1000algo softmaxAlgo mab (repeat 0.1) <$!> getStdGen)
  rewardsMeanChart rdm eg1 eg2 eg3 sm1 sm2
  meanPerArmPlot 0 rdm eg1 eg2 eg3 sm1 sm2
  meanPerArmPlot 1 rdm eg1 eg2 eg3 sm1 sm2
  meanPerArmPlot 2 rdm eg1 eg2 eg3 sm1 sm2
  meanPerArmPlot 3 rdm eg1 eg2 eg3 sm1 sm2
  actionsPerAlgo rdm "random"
  actionsPerAlgo eg1 "e-greedy (0)"
  actionsPerAlgo eg2 "e-greedy (0.1)"
  actionsPerAlgo eg3 "e-greedy (0.2)"
  actionsPerAlgo sm1 "softmax (1)"
  actionsPerAlgo sm2 "softmax (0.1)"

runMAB' :: MultiArmedBandit -> IO ()
runMAB' mab = do
  (rdm, eg1, eg2, eg3, sm1, sm2, egt, smt) <- runConcurrently $ (,,,,,,,)
        <$> Concurrently (run1000algo randomAlgo mab [] <$!> getStdGen)
        <*> Concurrently (run1000algo egreedyAlgo mab (repeat 0) <$!> getStdGen)
        <*> Concurrently (run1000algo egreedyAlgo mab (repeat 0.1) <$!> getStdGen)
        <*> Concurrently (run1000algo egreedyAlgo mab (repeat 0.2) <$!> getStdGen)
        <*> Concurrently (run1000algo softmaxAlgo mab (repeat 1) <$!> getStdGen)
        <*> Concurrently (run1000algo softmaxAlgo mab (repeat 0.1) <$!> getStdGen)
        <*> Concurrently (run1000algo egreedyAlgo mab ((1/) . sqrt <$> [1..]) <$!> getStdGen)
        <*> Concurrently (run1000algo softmaxAlgo mab ((\t -> 4*(1000-t)/1000) <$> [1..]) <$!> getStdGen)
  rewardsMeanChart' rdm eg1 eg2 eg3 sm1 sm2 egt smt
  meanPerArmPlot' 0 rdm eg1 eg2 eg3 sm1 sm2 egt smt
  meanPerArmPlot' 1 rdm eg1 eg2 eg3 sm1 sm2 egt smt
  meanPerArmPlot' 2 rdm eg1 eg2 eg3 sm1 sm2 egt smt
  meanPerArmPlot' 3 rdm eg1 eg2 eg3 sm1 sm2 egt smt
  actionsPerAlgo rdm "random"
  actionsPerAlgo eg1 "e-greedy (0)"
  actionsPerAlgo eg2 "e-greedy (0.1)"
  actionsPerAlgo eg3 "e-greedy (0.2)"
  actionsPerAlgo sm1 "softmax (1)"
  actionsPerAlgo sm2 "softmax (0.1)"
  actionsPerAlgo egt "e-greedy (inverse of sqrt(t))"
  actionsPerAlgo smt "softmax (4*((1000-t) by 1000))"

data Arm = Arm { qa           :: !Double
               , sigma        :: !Double
               , totalRewards :: !Double
               , totalPlayed  :: !Int
               , rewards      :: Seq Double
               } deriving (Show)

instance Eq Arm where
  a == b = getRatio a == getRatio b

instance Ord Arm where
  compare = compare `on` getRatio

getRatio :: Arm -> Double
getRatio (Arm _ _ _ 0 _) = 0
getRatio (Arm _ _ r p _) = r / fromIntegral p

createArm :: Double -> Double -> Arm
createArm q s = Arm q s 0 0 (Seq.singleton 0)

updateArm :: Arm -> StateRand (Arm, Double)
updateArm (Arm qa s r p v) = do
    !win <- lift $ liftRand (normal' (qa, s))
    let !r' = r + win
    let !p' = p + 1
    let !v' = v Seq.|> (r' / fromIntegral p')
    return (Arm qa s r' p' v', win)

data  MultiArmedBandit = MAB { arms            :: Seq Arm
                             , rewardsOverTime :: Seq Double
                             } deriving Show

type StateRand = StateT [Double] (Rand StdGen)
evalStateRand :: StateRand a -> [Double] -> StdGen -> a
evalStateRand strnd stream seed = flip evalRand seed $ evalStateT strnd stream

maximumIdx :: Seq Arm -> Int
maximumIdx xs = fromJust $ foldr1May max' [0..length xs - 1]
  where rs = getRatio <$!> xs
        max' x y = case compare (Seq.index rs x) (Seq.index rs y) of
                      LT -> y
                      _  -> x

updateMAB :: MultiArmedBandit -> Int -> StateRand MultiArmedBandit
updateMAB (MAB as rs) idx = do
    (a, r) <- updateArm $! Seq.index as idx
    return $! MAB (Seq.update idx a (updateNoSelArm <$> as)) (rs Seq.|> r)
  where last = Seq.index <$> identity <*> pred . length
        updateNoSelArm (Arm q s r p rs) = Arm q s r p (rs Seq.|> last rs)

randomStep :: MultiArmedBandit -> StateRand MultiArmedBandit
randomStep mab = getRandomR (0, length (arms mab) - 1) >>= updateMAB mab

egreedyStep :: MultiArmedBandit -> StateRand MultiArmedBandit
egreedyStep mab = do
    r <- getRandom
    (eps:rest) <- get
    put rest
    if r < eps
      then randomStep mab
      else updateMAB mab (maximumIdx $! arms mab)

softmaxStep :: MultiArmedBandit -> StateRand MultiArmedBandit
softmaxStep !mab = do
    (temp:rest) <- get
    put rest
    softmaxArmSelection temp mab >>= updateMAB mab

softmaxArmSelection :: Double -> MultiArmedBandit -> StateRand Int
softmaxArmSelection temp mabs = do
    g <- getRandom
    return . length $! Seq.takeWhileL (<= g) probs_cumul
  where !probs = (exp . (/ temp) . getRatio) <$!> arms mabs
        !total_probs = sum probs
        !probs_cumul = Seq.scanl1 (+) $! (/ total_probs) <$!> probs

iterateMN :: Int -> (MultiArmedBandit -> StateRand MultiArmedBandit) -> MultiArmedBandit -> StateRand MultiArmedBandit
iterateMN n algo !mab = go n mab
  where go 0 !mb = return mb
        go i !mb = algo mb >>= go (i-1)

iterateM1000 :: (MultiArmedBandit -> StateRand MultiArmedBandit) -> MultiArmedBandit -> StateRand MultiArmedBandit
iterateM1000 = iterateMN 1000

mab1 :: MultiArmedBandit
mab1 = MAB (Seq.fromList $! uncurry createArm <$!> [(2.3, 0.9), (2.1, 0.6) , (1.5, 0.4), (1.3, 2)]) Seq.empty

mab2 :: MultiArmedBandit
mab2 = MAB (Seq.fromList $! (uncurry createArm . second (*2)) <$!> [(2.3, 0.9), (2.1, 0.6) , (1.5, 0.4), (1.3, 2)]) Seq.empty

randomAlgo = iterateM1000 randomStep
egreedyAlgo = iterateM1000 egreedyStep
softmaxAlgo = iterateM1000 softmaxStep

run1000algo :: (MultiArmedBandit -> StateRand MultiArmedBandit) -> MultiArmedBandit -> [Double] -> StdGen -> [MultiArmedBandit]
run1000algo algo mab stream seed = evalStateRand (algo mab) stream <$!> bunchofseed
  where bunchofseed = mkStdGen <$> evalRand (replicateM 999 getRandom) seed

mean :: Foldable f => f Double -> Double
mean = (/) <$!> sum <*> (fromIntegral . length)

rewardsMeanChart :: [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> IO ()
rewardsMeanChart rdm eg1 eg2 eg3 sm1 sm2 = toFile fo "average_reward.svg" $ do
    layout_title .= "Average Rewards"
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= opaque black
    layout_left_axis_visibility . axis_show_ticks .= False
    plot (line "random" [addIndexes (rwsMeans rdm)])
    plot (line "e-greedy (0)" [addIndexes (rwsMeans eg1)])
    plot (line "e-greedy (0.1)" [addIndexes (rwsMeans eg2)])
    plot (line "e-greedy (0.2)" [addIndexes (rwsMeans eg3)])
    plot (line "softmax (1)" [addIndexes (rwsMeans sm1)])
    plot (line "softmax (0.1)" [addIndexes (rwsMeans sm2)])
  where rwsMeans mabs = let !rws = rewardsOverTime <$!> mabs
                            !len = let (x:_) = rws in length x
                        in (\i -> mean $! (`Seq.index` i) <$!> rws) <$!> [0..len-1]
        fo = FileOptions (800, 600) SVG

rewardsMeanChart' :: [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> IO ()
rewardsMeanChart' rdm eg1 eg2 eg3 sm1 sm2 egt smt = toFile fo "average_reward.svg" $ do
    layout_title .= "Average Rewards"
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= opaque black
    layout_left_axis_visibility . axis_show_ticks .= False
    plot (line "random" [addIndexes (rwsMeans rdm)])
    plot (line "e-greedy (0)" [addIndexes (rwsMeans eg1)])
    plot (line "e-greedy (0.1)" [addIndexes (rwsMeans eg2)])
    plot (line "e-greedy (0.2)" [addIndexes (rwsMeans eg3)])
    plot (line "softmax (1)" [addIndexes (rwsMeans sm1)])
    plot (line "softmax (0.1)" [addIndexes (rwsMeans sm2)])
    plot (line "e-greedy (1/sqrt(t))" [addIndexes (rwsMeans egt)])
    plot (line "softmax (4*((1000-t)/1000))" [addIndexes (rwsMeans smt)])
  where rwsMeans mabs = let !rws = rewardsOverTime <$!> mabs
                            !len = let (x:_) = rws in length x
                        in (\i -> mean $! (`Seq.index` i) <$!> rws) <$!> [0..len-1]
        fo = FileOptions (800, 600) SVG

meanPerArmPlot :: Int -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> IO ()
meanPerArmPlot n rdm eg1 eg2 eg3 sm1 sm2 = toFile fo ("estimated_mean_action_" `mappend` show n `mappend` ".svg") $ do
    layout_title .= "Estimated mean for action " `mappend` show n
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= opaque black
    layout_left_axis_visibility . axis_show_ticks .= False
    plot (line "Qa*" [addIndexes (replicate 1000 qai)])
    plot (line "random" [addIndexes (rwsMeans rdm)])
    plot (line "e-greedy (0)" [addIndexes (rwsMeans eg1)])
    plot (line "e-greedy (0.1)" [addIndexes (rwsMeans eg2)])
    plot (line "e-greedy (0.2)" [addIndexes (rwsMeans eg3)])
    plot (line "softmax (1)" [addIndexes (rwsMeans sm1)])
    plot (line "softmax (0.1)" [addIndexes (rwsMeans sm2)])
  where !fo = FileOptions (800, 600) SVG
        !qai = qa . fromJust $! (((`Seq.index` n) . arms) <$!> head rdm)
        rwsMeans mabs = let !as = ((`Seq.index` n) . arms) <$!> mabs
                            !rws = rewards <$!> as
                            !len = let (x:_) = rws in length x
                         in (\i -> mean $! (`Seq.index` i) <$!> rws) <$!> [0..len-1]

meanPerArmPlot' :: Int -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> [MultiArmedBandit] -> IO ()
meanPerArmPlot' n rdm eg1 eg2 eg3 sm1 sm2 egt smt = toFile fo ("estimated_mean_action_" `mappend` show n `mappend` ".svg") $ do
   layout_title .= "Estimated mean for action " `mappend` show n
   layout_background .= solidFillStyle (opaque white)
   layout_foreground .= opaque black
   layout_left_axis_visibility . axis_show_ticks .= False
   plot (line "Qa*" [addIndexes (replicate 1000 qai)])
   plot (line "random" [addIndexes (rwsMeans rdm)])
   plot (line "e-greedy (0)" [addIndexes (rwsMeans eg1)])
   plot (line "e-greedy (0.1)" [addIndexes (rwsMeans eg2)])
   plot (line "e-greedy (0.2)" [addIndexes (rwsMeans eg3)])
   plot (line "softmax (1)" [addIndexes (rwsMeans sm1)])
   plot (line "softmax (0.1)" [addIndexes (rwsMeans sm2)])
   plot (line "e-greedy (1/sqrt(t))" [addIndexes (rwsMeans egt)])
   plot (line "softmax (4*((1000-t)/1000))" [addIndexes (rwsMeans smt)])
 where !fo = FileOptions (800, 600) SVG
       !qai = qa . fromJust $! (((`Seq.index` n) . arms) <$!> head rdm)
       rwsMeans mabs = let !as = ((`Seq.index` n) . arms) <$!> mabs
                           !rws = rewards <$!> as
                           !len = let (x:_) = rws in length x
                        in (\i -> mean $! (`Seq.index` i) <$!> rws) <$!> [0..len-1]

actionsPerAlgo :: [MultiArmedBandit] -> FilePath -> IO ()
actionsPerAlgo mabs title = toFile fo (title `mappend` ".svg") $ do
    layout_title .= "Number of times arms are selected in " `mappend` title
    layout_x_axis . laxis_generate .= autoIndexAxis ["action " `mappend` show n | n <- [0..length values]]
    plot $! plotBars <$!> bars (replicate (length values) "") (addIndexes values)
  where !fo = FileOptions (800, 600) SVG
        !as = (toList . fmap (fromIntegral . totalPlayed) . arms) <$!> mabs
        values = [[round $ mean a] | a <- transpose as] :: [[Int]]
