module Logic.AnalisisDatos where

import Types.Event
import Helpers.OperacionesDatos
import Data.List
import Data.Ord (comparing)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time 


montoTotal :: [Event] -> Float
montoTotal = sum . map calcularImporte

extraeYear :: Int -> Integer
extraeYear timestamp =
    let tiempoUtc = posixSecondsToUTCTime (fromIntegral timestamp)
        (year, _, _) = toGregorian (utctDay tiempoUtc)
    in year

promedioCategoriaAnual :: [Event] -> [(String, Integer, Float)]
promedioCategoriaAnual eventos =
    let grupo = groupBy (\a b -> category a == category b && extraeYear (timestamp a) == extraeYear (timestamp b))
                  $ sortBy (comparing (\e -> (category e, extraeYear (timestamp e)))) eventos
    in map resumir grupo
    where
        resumir grupo =
            let categoria = category(head grupo)
                year = extraeYear (timestamp (head grupo))
                prom = sum(map calcularImporte grupo) / fromIntegral (length grupo)
            in (categoria, year, prom)