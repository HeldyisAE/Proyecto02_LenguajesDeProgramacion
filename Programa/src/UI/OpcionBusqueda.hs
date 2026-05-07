{-
Módulo: UI.OpcionBusqueda
Descripción: Contiene la interfaz de usuario para la búsqueda de eventos por rango de fechas
-}
module UI.OpcionBusqueda where

import System.IO (hFlush, stdout)
import Types.Event
import Helpers.OperacionesFechas
import Logic.Busqueda

{-
Nombre: pedirFechas

Esta función se encarga de solicitar al usuario dos fechas por consola,
    validarlas y convertirlas a formato timestamp.

Parámetros: No recibe parámetros

Retorno: Devuelve un valor de tipo IO (Maybe (Int, Int)),
    que representa una acción que puede producir un rango de fechas válido
-}
pedirFechas :: IO (Maybe (Int, Int))
pedirFechas = do
    putStrLn ""
    putStrLn "Ingrese fecha inicio (dd-mm-aaaa):"
    putStr "> "
    hFlush stdout
    f1 <- getLine

    putStrLn "Ingrese fecha fin (dd-mm-aaaa):"
    putStr "> "
    hFlush stdout
    f2 <- getLine

    let inicio = convertirFecha f1
    let fin = convertirFecha f2 

    case (inicio, fin) of 
        (Just i, Just f) -> return (Just (i, f)) -- Devuelve las fechas si se pudieron convertir
        _ -> do 
            putStrLn "Formato de fecha invalido"
            return Nothing

{-
Nombre: busquedaPorFecha

Esta función se encarga de gestionar la búsqueda de eventos por un rango de fechas,
    solicitando los datos al usuario y mostrando los resultados.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve un tipo IO (), que representa una acción de interacción en consola
-}
busquedaPorFecha :: [Event] -> IO ()
busquedaPorFecha eventos = do 
    resultado <- pedirFechas 
    case resultado of 
        Just (inicio, fin) ->
            imprimirEventosPorFecha inicio fin eventos 
        Nothing -> do
            putStrLn "Intente nuevamente"
            busquedaPorFecha eventos