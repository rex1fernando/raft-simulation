{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader

type Degrees = Double
type Latitude = Degrees
type Longitude = Degrees

data Meetup = Meetup { _name :: String, _location :: (Latitude, Longitude) } deriving Show
makeLenses ''Meetup

test :: ReaderT () (State Meetup) ()
test = do
  name .= "hi"

main = putStrLn $ show $ runState (runReaderT test ()) (Meetup "" (0,0))
