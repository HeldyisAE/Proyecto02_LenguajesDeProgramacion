{- HLINT ignore "Eta reduce" -}
module Helpers.OperacionesFechas where

import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Char (isDigit)

formatoValido :: String -> Bool
formatoValido str =
    case str of 
        [d1,d2,'-',m1,m2,'-',y1,y2,y3,y4] ->
            all isDigit [d1,d2,m1,m2,y1,y2,y3,y4]
        _ -> False

formatearFecha :: Int -> String
formatearFecha timestamp =
    formatTime defaultTimeLocale "%d-%m-%Y" -- Formato de fecha
        (posixSecondsToUTCTime (fromIntegral timestamp)) -- Pasa de segundos a fecha

parsearFecha :: String -> Maybe UTCTime
parsearFecha str =
    parseTimeM True defaultTimeLocale "%d-%m-%Y" str -- Lee el string de entrada como una fecha

timestampConvertir :: UTCTime -> Int 
timestampConvertir fecha = 
    round (utcTimeToPOSIXSeconds fecha) -- Convierte de fecha a segundos

convertirFecha :: String -> Maybe Int 
convertirFecha str =
    if not (formatoValido str)
        then Nothing
        else do
            fecha <- parsearFecha str -- Convierte el string a fecha
            return (timestampConvertir fecha) -- Convierte el string el timestamp para buscar

