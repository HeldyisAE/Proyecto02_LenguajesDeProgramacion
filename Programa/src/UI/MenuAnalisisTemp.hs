module UI.MenuAnalisisTemp where

import System.IO (hFlush, stdout)
import Types.Event

menuAnalisisTemp :: [Event] -> IO () 
menuAnalisisTemp eventos = do
    putStrLn "--- Analisis temporal ---"
    putStrLn "1. Maximo monto mensual y frecuencia por dia de semana"
    putStrLn "2. Ver evento (Mas antiguo/Mas reciente)"
    putStrLn "3. Resumen de montos por intervalo"
    putStrLn "0. Volver"
    putStr "> "
    hFlush stdout
    
    op <- getLine
    procesarTempOp op eventos

procesarTempOp :: String -> [Event] -> IO ()
procesarTempOp opcion eventos =
    case opcion of 
        "1" -> do
            putStrLn "Monto maximo por mes y mas activo de la semana [Está pendiente]"
            putStrLn ""
            menuAnalisisTemp eventos

        "2" -> do
            putStrLn "Ver evento mas antiguo y mas reciente [Está pendiente]"
            putStrLn ""
            menuAnalisisTemp eventos

        "3" -> do
            putStrLn "Resumen de los montos [Está pendiente]"
            putStrLn ""
            menuAnalisisTemp eventos

        "0" -> do
            putStrLn ""
            return ()

        _ -> do
            putStrLn "Opción inválida"
            putStrLn ""
            menuAnalisisTemp eventos
