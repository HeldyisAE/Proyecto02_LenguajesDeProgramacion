module Data.GeneradorEventos where

import Types.Event
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Categorias
categorias :: [String]
categorias = ["visualizacion", "apartado", "compra", "devolucion", "seguimiento"]

-- Rellena atributos de evento con datos random
generarEvento :: IO Event
generarEvento = do
    idEvento <- randomRIO (0, 9000000) :: IO Int --Numero random entre 0 y 9000000

    indiceCat <- randomRIO (0, length categorias - 1) :: IO Int --Elige una categoría al azar
    let cat = categorias !! indiceCat

    valor <- randomRIO (500, 75000) :: IO Int --Valor aleatorio

    tiempoActual <- round <$> getPOSIXTime --Toma el tiempo actual del sistema
    incremento <- randomRIO (0, 2 * 365 * 24 * 60 * 60) -- Calcula hasta dentro de 2 años a partir del tiempo actual / Puede ser 0
    let timestamp = tiempoActual + incremento

    return $ Event idEvento cat (fromIntegral valor) timestamp

generarEventos :: IO [Event]
generarEventos = do
    cantidad <- randomRIO (10, 25) :: IO Int --Genera una cantidad random de eventos a generar
    sequence [generarEvento | _ <- [1..cantidad]] --Ejecuta generarEvento n veces