import Control.Concurrent.STM
import GHC.Conc (unsafeIOToSTM)

launchTorpedoes :: IO ()
launchTorpedoes = undefined

doStuff :: STM ()
doStuff = undefined

mightRetry :: STM ()
mightRetry = undefined

notActuallyAtomic :: STM ()
notActuallyAtomic = do
  doStuff
  unsafeIOToSTM launchTorpedoes
  mightRetry
