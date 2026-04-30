module Data.GeneradorEventos where

import Types.Event
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (nub)

-- Categorias
categorias :: [String]
categorias = ["visualizacion", "apartado", "compra", "devolucion", "seguimiento"]

generarIds :: Int -> IO [Int]
generarIds n = do
    ids <- sequence [randomRIO (0, 9000000) | _ <- [1..n]]
    let idsUnicos = nub ids -- Se eliminan los ids repetidos
    if length idsUnicos == n -- Se verifica la cantidad solicitada
        then return idsUnicos
        else generarIds n

-- Rellena atributos de evento con datos random
generarEvento :: Int -> IO Event
generarEvento idEvento = do

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
    ids <- generarIds cantidad
    sequence [generarEvento idEvento | idEvento <- ids] --Ejecuta generarEvento n veces