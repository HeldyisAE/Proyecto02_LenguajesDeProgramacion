module Logic.AnalisisDatos where

import Types.Event
import Helpers.OperacionesDatos
import Data.List
import Data.Ord (comparing)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time 

data ResumenFinanciero = ResumenFinanciero
    { ganancias :: Float
    , devoluciones :: Float
    , total :: Float
    } deriving (Show)

montoTotal :: [Event] -> Float
montoTotal = sum . map calcularImporte

calcularResumen :: [Event] -> ResumenFinanciero
calcularResumen eventos = 
    let eventosSinDevs = filter (\e -> category e /= "devolucion") eventos
        eventosDev = filter (\e -> category e == "devolucion") eventos
        totalGanancias = sum (map calcularImporte eventosSinDevs)
        totalDevoluciones = sum (map calcularImporte eventosDev)
    in ResumenFinanciero
        { ganancias = totalGanancias
        , devoluciones = -totalDevoluciones
        , total = montoTotal eventos
        }

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