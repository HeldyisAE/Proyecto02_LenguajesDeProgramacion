module Types.Event where

data Event = Event
    { idEvent  :: Int
    , category :: String
    , value     :: Float
    , timestamp :: Int
    } deriving (Show, Eq)