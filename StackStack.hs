import Control.Monad.State
import Control.Monad.Reader

type Foo = StateT Int (State String)

outerPut :: Int -> Foo ()
outerPut = put

innerPut :: String -> Foo ()
innerPut = lift . put

type Bar = ReaderT Bool Foo

barPut :: String -> Bar ()
barPut = lift . lift . put

