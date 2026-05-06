{-
Módulo: Data.GeneradorEventos
Descripción: Contiene la lógica para la generación dinámica de eventos
-}
module Data.GeneradorEventos where

import Types.Event
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Categorias disponibles en el sistema
categorias :: [String]
categorias = ["visualizacion", "visualizacion", "apartado", "apartado", "apartado", "compra", "compra", "compra",
                "devolucion", "seguimiento"]

{-
Nombre: generarCantidad

Genera un número aleatorio entre 1 y 50 en función de la categoría del evento,
    para visualización y seguimiento se inicia en 1 por interpretación lógica.

Parámetros: Recibe un String

Retorno: Retorna un valor de tipo IO Int que devuelve una acción numérica
-}
generarCantidad :: String -> IO Int
generarCantidad categoria
    | categoria `elem` ["apartado", "compra", "devolucion"] = randomRIO (1, 50)
    | otherwise = return 1

{-
Nombre: generarEvento

Esta función se encarga de rellenar los espacios de los atributos de un evento y devolver una instancia evento

Parámetros: Recibe un número entero, que corresponde al ID del evento

Retorno: Devuelve un tipo IO Evento, que representa una acción secundaria de tipo Event
-}
generarEvento :: Int -> IO Event
generarEvento idEvento = do

    indiceCat <- randomRIO (0, length categorias - 1) :: IO Int --Elige una categoría al azar
    let cat = categorias !! indiceCat

    valor <- randomRIO (500, 75000) :: IO Int --Valor aleatorio

    tiempoActual <- round <$> getPOSIXTime --Toma el tiempo actual del sistema
    incremento <- randomRIO (0, 2 * 365 * 24 * 60 * 60) -- Calcula hasta dentro de 2 años a partir del tiempo actual / Puede ser 0
    let timestamp = tiempoActual + incremento

    cantidad <- generarCantidad cat

    return $ Event idEvento cat (fromIntegral valor) timestamp cantidad 0.0

{-
Nombre: generarEventos

Esta función se encarga de generar dinámicamente un conjunto de eventos con un intervalo entre 10 y 25

Parámetros: Recibe un entero, que corresponde al ID incremental de los eventos

Retorno: Devuelve un tipo IO de una lista de eventos
-}
generarEventos :: Int -> IO [Event]
generarEventos incrementalId = do
    cantidadEventos <- randomRIO (10, 25) :: IO Int --Genera una cantidad random de eventos a generar
    sequence [generarEvento (incrementalId + i) | i <- [0..cantidadEventos-1]] --Ejecuta generarEvento n veces