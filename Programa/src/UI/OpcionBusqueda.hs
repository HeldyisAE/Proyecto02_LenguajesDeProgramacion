module UI.OpcionBusqueda where

import System.IO (hFlush, stdout)
import Types.Event
import Helpers.OperacionesFechas
import Logic.Busqueda

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

busquedaPorFecha :: [Event] -> IO ()
busquedaPorFecha eventos = do 
    resultado <- pedirFechas 
    case resultado of 
        Just (inicio, fin) ->
            imprimirEventosPorFecha inicio fin eventos 
        Nothing -> do
            putStrLn "Intente nuevamente"
            busquedaPorFecha eventos