module Main (main) where

import           Criterion.Main (bgroup, defaultMain)
import qualified SweetrollBench
import qualified Sweetroll.UtilBench
-- HASKELETON: import qualified New.ModuleBench

main :: IO ()
main = defaultMain
    [ bgroup "Sweetroll" SweetrollBench.benchmarks
    , bgroup "Sweetroll.Util" Sweetroll.UtilBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
