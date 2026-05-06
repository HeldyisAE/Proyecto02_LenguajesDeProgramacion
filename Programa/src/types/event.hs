{-
Modulo: Types.Event 

Descripción: Contiene la definición del objeto principal del programa, evento
-}
module Types.Event where

data Event = Event
    { idEvent   :: Int
    , category  :: String
    , value     :: Float
    , timestamp :: Int
    , amount    :: Int
    , tax       :: Float
    } deriving (Show, Eq)