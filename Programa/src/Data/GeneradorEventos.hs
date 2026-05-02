module Data.GeneradorEventos where

import Types.Event
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Categorias
categorias :: [String]
categorias = ["visualizacion", "apartado", "compra", "devolucion", "seguimiento"]

generarCantidad :: String -> IO Int
generarCantidad categoria
    | categoria `elem` ["apartado", "compra", "devolucion"] = randomRIO (1, 50)
    | otherwise = return 1

-- Rellena atributos de evento con datos random
generarEvento :: Int -> IO Event
generarEvento idEvento = do

    indiceCat <- randomRIO (0, length categorias - 1) :: IO Int --Elige una categoría al azar
    let cat = categorias !! indiceCat

    valor <- randomRIO (500, 75000) :: IO Int --Valor aleatorio

    tiempoActual <- round <$> getPOSIXTime --Toma el tiempo actual del sistema
    incremento <- randomRIO (0, 2 * 365 * 24 * 60 * 60) -- Calcula hasta dentro de 2 años a partir del tiempo actual / Puede ser 0
    let timestamp = tiempoActual + incremento

    cantidad <- generarCantidad cat

    return $ Event idEvento cat (fromIntegral valor) timestamp cantidad

generarEventos :: Int -> IO [Event]
generarEventos incrementalId = do
    cantidadEventos <- randomRIO (10, 25) :: IO Int --Genera una cantidad random de eventos a generar
    sequence [generarEvento (incrementalId + i) | i <- [0..cantidadEventos-1]] --Ejecuta generarEvento n veces