import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State
import CountEntries (listDirectory)

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  (AppConfig cfg) <- ask
  rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfg
              then do
                let newDepth = curDepth + 1
                (AppState st) <- get
                when (st < newDepth) $
                  put $ AppState newDepth
                -- st <- get
                -- when (stDeepestReached st < newDepth) $
                --   put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
              else return []
  return $ (path, length contents) : concat rest

implicitGet :: App AppState
implicitGet = get

explicitGet :: App AppState
explicitGet = lift get
