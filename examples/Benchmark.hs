import Criterion.Main
import Control.Exception
import Control.Monad.ST
import Control.Monad (replicateM)
import Data.Int
import Data.Word
import qualified System.Random as R
import qualified System.Random.MWC as MWC
import qualified System.Random.Mersenne as M
import qualified System.Random.SPRNG.LFG as LFG

main = do
  mwc <- MWC.create
  mtg <- M.newMTGen . Just =<< MWC.uniform mwc
  lfg <- LFG.new 42
  defaultMain
    [ bgroup "random"
      [
        bench "Double" (R.randomIO >>= evaluate :: IO Double)
      ]
    , bgroup "mwc"
      [ bench "Double"  (MWC.uniform mwc :: IO Double)
      ]
    , bgroup "mersenne"
      [
        bench "Double" (M.random mtg :: IO Double)
      ]
    , bgroup "SPRNG"
      [
        bench "Double" (LFG.randomDouble lfg :: IO Double)
      ]
    ]
