{-
Módulo: Data.Registro
Descripción: Contiene funciones para la persistencia de eventos en archivos CSV,
    permitiendo cargar y guardar información desde almacenamiento
-}
module Data.Registro where

import Types.Event 
import System.IO 
import System.Directory (doesFileExist)
import Data.List

-- Constante que representa la ruta de guardado para persistencia de datos
rutaArchivo :: FilePath
rutaArchivo = "src/Storage/Registro.csv"

-- Constante que representa el encabezado del archivo de guardado de persistencia de datos
encabezado :: String 
encabezado = "idEvent,category,value,timestamp,amount,tax,tag"

{-
Nombre: cargarEventos

Esta función se encarga de cargar los eventos desde un archivo CSV,
    validando su existencia y transformando cada línea en un evento.

Parámetros: No recibe parámetros

Retorno: Devuelve un tipo IO [Event], que representa una acción que produce una lista de eventos
-}
cargarEventos :: IO [Event]
cargarEventos = do 
    existe <- doesFileExist rutaArchivo
    if not existe 
        then return []
        else do
            contenido <- readFile rutaArchivo
            let lineas = drop 1 (lines contenido) --Se omite el encabezado
            return (filter validarEvento (map parsearLinea lineas))

{-
Nombre: guardarEventos

Esta función se encarga de guardar una lista de eventos en un archivo CSV,
    sobrescribiendo el contenido existente.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve un tipo IO (), que representa una acción de salida hacia el sistema de archivos
-}
guardarEventos :: [Event] -> IO ()
guardarEventos eventos = do 
    withFile rutaArchivo WriteMode $ \h -> do
        hPutStrLn h encabezado 
        mapM_ (hPutStrLn h . eventoACSV) eventos 
    putStrLn $ "Registro guardado: " ++ show (length eventos) ++ " eventos"

{-
Nombre: eventoACSV

Esta función se encarga de convertir un evento a una representación en formato CSV,
    separando sus atributos por comas.

Parámetros: Recibe un valor de tipo Event

Retorno: Devuelve un valor de tipo String que representa el evento en formato CSV
-}
eventoACSV :: Event -> String
eventoACSV e = intercalate ","
    [ show (idEvent e)
    , category e
    , show (value e)
    , show (timestamp e)
    , show (amount e)
    , show (tax e)
    , if tag e then "True" else "False"
    ]

{-
Nombre: parsearLinea

Esta función se encarga de convertir una línea del archivo CSV en un valor de tipo Event,
    asignando cada campo a su atributo correspondiente.

Parámetros: Recibe un valor de tipo String que representa una línea del archivo

Retorno: Devuelve un valor de tipo Event
-}
parsearLinea :: String -> Event
parsearLinea linea =
    let campos = splitCSV linea
    in Event
        { idEvent   = read (campos !! 0)
        , category  = campos !! 1
        , value     = read (campos !! 2)
        , timestamp = read (campos !! 3)
        , amount    = read (campos !! 4)
        , tax       = read (campos !! 5)
        , tag       = (campos !! 6) == "True"
        }

{-
Nombre: validarEvento

Esta función se encarga de validar un evento,
    asegurando que cumpla con condiciones básicas como un valor positivo.

Parámetros: Recibe un valor de tipo Event

Retorno: Devuelve un valor de tipo Bool que indica si el evento es válido
-}
validarEvento :: Event -> Bool
validarEvento e = value e > 0

{-
Nombre: splitCSV

Esta función se encarga de dividir una cadena en formato CSV en una lista de campos,
    utilizando la coma como separador.

Parámetros: Recibe un valor de tipo String

Retorno: Devuelve una lista de valores de tipo String que representan los campos separados
-}
splitCSV :: String -> [String]
splitCSV str = foldr f [[]] str
  where
    f ',' acc     = [] : acc
    f c   (h:t)   = (c:h) : t
    f _   []      = [[]]
