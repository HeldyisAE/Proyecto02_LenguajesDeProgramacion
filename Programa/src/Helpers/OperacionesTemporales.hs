{-
Módulo: Helpers.OperacionesTemporales
Descripción: Contiene funciones para la extracción y manipulación de información temporal a partir de timestamps
-}
{- HLINT ignore "Eta reduce" -}
module Helpers.OperacionesTemporales where 

import Data.Time
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import Helpers.OperacionesFechas
import Types.Event

{-
Nombre: extraerAnioMes

Esta función se encarga de obtener el año y el mes a partir de un timestamp,
    facilitando la agrupación de eventos por periodo.

Parámetros: Recibe un valor de tipo Int que representa el timestamp

Retorno: Devuelve una tupla de tipo (Integer, Int) que representa el año y el mes
-}
extraerAnioMes :: Int -> (Integer, Int)
extraerAnioMes timestamp =
    let (a, m, _) = toGregorian (utctDay (timestampToUTC timestamp))
    in (a, m)

{-
Nombre: extraerAnio

Esta función se encarga de obtener únicamente el año a partir de un timestamp.

Parámetros: Recibe un valor de tipo Int que representa el timestamp

Retorno: Devuelve un valor de tipo Integer que representa el año
-}
extraerAnio :: Int -> Integer
extraerAnio timestamp = fst3 (toGregorian (utctDay (timestampToUTC timestamp)))
    where fst3 (a, _, _) = a

{-
Nombre: extraerFechaExacta

Esta función se encarga de convertir un timestamp a una fecha exacta en formato "dd/mm/yyyy",
    asegurando que los valores de día y mes tengan dos dígitos.

Parámetros: Recibe un valor de tipo Int que representa el timestamp

Retorno: Devuelve un valor de tipo String que representa la fecha formateada
-}
extraerFechaExacta :: Int -> String 
extraerFechaExacta timestamp =
    let (a, m, d) = toGregorian (utctDay (timestampToUTC timestamp))
    in pad d ++ "/" ++ pad m ++ "/" ++ show a
    where pad n = if n < 10 then "0" ++ show n else show n

{-
Nombre: extraerDiaSemana

Esta función se encarga de obtener el nombre del día de la semana correspondiente a un timestamp,
    junto con la fecha exacta asociada.

Parámetros: Recibe un valor de tipo Int que representa el timestamp

Retorno: Devuelve una tupla de tipo (String, String) que contiene el nombre del día y la fecha exacta
-}
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

{-
Nombre: nombreMes

Esta función se encarga de convertir un número de mes a su representación en texto.

Parámetros: Recibe un valor de tipo Int que representa el mes

Retorno: Devuelve un valor de tipo String que representa el nombre del mes
-}
nombreMes :: Int -> String 
nombreMes mes = case mes of 
    1  -> "Enero";   2  -> "Febrero"; 3  -> "Marzo"
    4  -> "Abril";   5  -> "Mayo";    6  -> "Junio"
    7  -> "Julio";   8  -> "Agosto";  9  -> "Septiembre"
    10 -> "Octubre"; 11 -> "Noviembre"; _ -> "Diciembre"

{-
Nombre: agruparPor

Esta función se encarga de agrupar una lista de elementos en función de una clave,
    permitiendo organizar los datos según un criterio específico.

Parámetros: Recibe una función que define la clave de agrupación y una lista de elementos

Retorno: Devuelve una lista de tuplas donde cada tupla contiene la clave y la lista de elementos agrupados
-}
agruparPor :: Ord k => (a -> k) -> [a] -> [(k, [a])]
agruparPor f xs =
    map (\g -> (f (head g), g))
    . groupBy ((==) `on` f)
    . sortBy (comparing f)
    $ xs

{-
Nombre: obtenerTotalDias

Esta función se encarga de calcular la cantidad total de días entre el evento más antiguo
    y el más reciente dentro de una lista de eventos.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve un valor de tipo Integer que representa la cantidad de días entre los eventos
-}
obtenerTotalDias :: [Event] -> Integer
obtenerTotalDias eventos =
    let tiempos = map timestamp eventos
        tsMin = minimum tiempos
        tsMax = maximum tiempos
        diferencia = tsMax - tsMin
    in fromIntegral (diferencia `div` 86400)
