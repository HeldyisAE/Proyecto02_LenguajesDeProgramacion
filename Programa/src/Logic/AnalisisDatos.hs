{-
Módulo: Logic.AnalisisDatos
Descripción: Contiene funciones para el análisis financiero y estadístico de los eventos
-}
module Logic.AnalisisDatos where

import Types.Event
import Helpers.OperacionesDatos
import Data.List
import Data.Ord (comparing)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time 

-- Utilizado para referencias un tipo de dato para imprimir después
data ResumenFinanciero = ResumenFinanciero
    { ganancias :: Float
    , devoluciones :: Float
    , total :: Float
    } deriving (Show)

{-
Nombre: montoTotal

Esta función se encarga de calcular el monto total acumulado de una lista de eventos,
    considerando el importe de cada uno.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve un valor de tipo Float que representa el monto total
-}
montoTotal :: [Event] -> Float
montoTotal = sum . map calcularImporte

{-
Nombre: calcularResumen

Esta función se encarga de generar un resumen financiero a partir de una lista de eventos,
    separando las ganancias y devoluciones para obtener un total consolidado.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve un valor de tipo ResumenFinanciero con los resultados calculados
-}
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

{-
Nombre: extraeYear

Esta función se encarga de extraer el año a partir de un timestamp en formato Unix.

Parámetros: Recibe un valor de tipo Int que representa el timestamp

Retorno: Devuelve un valor de tipo Integer que representa el año
-}
extraeYear :: Int -> Integer
extraeYear timestamp =
    let tiempoUtc = posixSecondsToUTCTime (fromIntegral timestamp)
        (year, _, _) = toGregorian (utctDay tiempoUtc)
    in year

{-
Nombre: promedioCategoriaAnual

Esta función se encarga de agrupar los eventos por categoría y año,
    calculando el promedio del importe para cada grupo.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve una lista de tuplas de tipo (String, Integer, Float)
    que representan la categoría, el año y el promedio calculado
-}
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