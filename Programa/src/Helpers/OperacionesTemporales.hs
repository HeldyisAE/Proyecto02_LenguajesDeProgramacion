{- HLINT ignore "Eta reduce" -}
module Helpers.OperacionesTemporales where 

import Data.Time
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import Helpers.OperacionesFechas

extraerAnioMes :: Int -> (Integer, Int)
extraerAnioMes timestamp =
    let (a, m, _) = toGregorian (utctDay (timestampToUTC timestamp))
    in (a, m)

extraerAnio :: Int -> Integer
extraerAnio timestamp = fst3 (toGregorian (utctDay (timestampToUTC timestamp)))
    where fst3 (a, _, _) = a

extraerFechaExacta :: Int -> String 
extraerFechaExacta timestamp =
    let (a, m, d) = toGregorian (utctDay (timestampToUTC timestamp))
    in pad d ++ "/" ++ pad m ++ "/" ++ show a
    where pad n = if n < 10 then "0" ++ show n else show n

extraerDiaSemana :: Int -> (String, String)
extraerDiaSemana timestamp = 
    let dia = utctDay (timestampToUTC timestamp)
        nombre = case dayOfWeek dia of 
            Monday    -> "Lunes"
            Tuesday   -> "Martes"
            Wednesday -> "Miercoles"
            Thursday  -> "Jueves"
            Friday    -> "Viernes"
            Saturday  -> "Sabado"
            Sunday    -> "Domingo"
        fecha = extraerFechaExacta timestamp
    in (nombre, fecha)

nombreMes :: Int -> String 
nombreMes mes = case mes of 
    1  -> "Enero";   2  -> "Febrero"; 3  -> "Marzo"
    4  -> "Abril";   5  -> "Mayo";    6  -> "Junio"
    7  -> "Julio";   8  -> "Agosto";  9  -> "Septiembre"
    10 -> "Octubre"; 11 -> "Noviembre"; _ -> "Diciembre"

agruparPor :: Ord k => (a -> k) -> [a] -> [(k, [a])]
agruparPor f xs =
    map (\g -> (f (head g), g))
    . groupBy ((==) `on` f)
    . sortBy (comparing f)
    $ xs
