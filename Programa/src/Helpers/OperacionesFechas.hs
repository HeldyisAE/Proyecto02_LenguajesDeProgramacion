{-
Módulo: Helpers.OperacionesFechas
Descripción: Contiene funciones para la validación, conversión y formateo de fechas
-}
{- HLINT ignore "Eta reduce" -}
module Helpers.OperacionesFechas where

import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Char (isDigit)

{-
Nombre: formatoValido

Esta función se encarga de validar si un string cumple con el formato de fecha "dd-mm-yyyy",
    verificando tanto la estructura como que todos los caracteres correspondientes sean dígitos.

Parámetros: Recibe un valor de tipo String que representa la entrada del usuario

Retorno: Devuelve un valor de tipo Bool que indica si el formato es válido o no
-}
formatoValido :: String -> Bool
formatoValido str =
    case str of 
        [d1,d2,'-',m1,m2,'-',y1,y2,y3,y4] ->
            all isDigit [d1,d2,m1,m2,y1,y2,y3,y4]
        _ -> False

{-
Nombre: formatearFecha

Esta función se encarga de convertir un timestamp en formato Unix a un string con formato "dd-mm-yyyy"
    para su correcta visualización.

Parámetros: Recibe un valor de tipo Int que representa el timestamp

Retorno: Devuelve un valor de tipo String que representa la fecha formateada
-}
formatearFecha :: Int -> String
formatearFecha timestamp =
    formatTime defaultTimeLocale "%d-%m-%Y" -- Formato de fecha
        (posixSecondsToUTCTime (fromIntegral timestamp)) -- Pasa de segundos a fecha

{-
Nombre: parsearFecha

Esta función se encarga de convertir un string con formato "dd-mm-yyyy" a un valor de tipo UTCTime,
    permitiendo su uso en operaciones de fechas.

Parámetros: Recibe un valor de tipo String que representa una fecha de entrada

Retorno: Devuelve un valor de tipo Maybe UTCTime que representa la fecha parseada si es válida
-}
parsearFecha :: String -> Maybe UTCTime
parsearFecha str =
    parseTimeM True defaultTimeLocale "%d-%m-%Y" str -- Lee el string de entrada como una fecha

{-
Nombre: timestampConvertir

Esta función se encarga de convertir una fecha de tipo UTCTime a un timestamp en formato Unix,
    representado en segundos.

Parámetros: Recibe un valor de tipo UTCTime

Retorno: Devuelve un valor de tipo Int que representa el timestamp
-}
timestampConvertir :: UTCTime -> Int 
timestampConvertir fecha = 
    round (utcTimeToPOSIXSeconds fecha) -- Convierte de fecha a segundos

{-
Nombre: timestampToUTC

Esta función se encarga de convertir un timestamp en formato Unix a un valor de tipo UTCTime.

Parámetros: Recibe un valor de tipo Int que representa el timestamp

Retorno: Devuelve un valor de tipo UTCTime que representa la fecha correspondiente
-}
timestampToUTC :: Int -> UTCTime
timestampToUTC timestamp = posixSecondsToUTCTime (fromIntegral timestamp)

{-
Nombre: convertirFecha

Esta función se encarga de validar y convertir un string de fecha en formato "dd-mm-yyyy"
    a un timestamp en formato Unix, facilitando su uso en búsquedas y comparaciones.

Parámetros: Recibe un valor de tipo String

Retorno: Devuelve un valor de tipo Maybe Int que representa el timestamp si la conversión es exitosa
-}
convertirFecha :: String -> Maybe Int 
convertirFecha str =
    if not (formatoValido str)
        then Nothing
        else do
            fecha <- parsearFecha str -- Convierte el string a fecha
            return (timestampConvertir fecha) -- Convierte el string el timestamp para buscar

