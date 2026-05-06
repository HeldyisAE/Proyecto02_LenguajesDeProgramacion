module Types.Event where

data Event = Event
    { idEvent   :: Int
    , category  :: String
    , value     :: Float
    , timestamp :: Int
    , amount    :: Int
    , tax       :: Float
    , tag       :: Bool
    } deriving (Show, Eq)