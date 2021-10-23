module Types where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data RioApp = RioApp
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc RioApp where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext RioApp where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

