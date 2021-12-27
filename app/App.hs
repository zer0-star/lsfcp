module App where

import RIO
import RIO.Process

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  }

data Options = Options
  { inputFile :: !FilePath
  , outputFile :: !FilePath
  }

class HasOptions env where
  optionsL :: Lens' env Options

instance HasOptions App where
  optionsL = lens appOptions (\x y -> x{appOptions = y})

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x{appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x{appProcessContext = y})