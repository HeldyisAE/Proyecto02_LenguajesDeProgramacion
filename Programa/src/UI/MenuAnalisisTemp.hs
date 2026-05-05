module UI.MenuAnalisisTemp where

import System.IO (hFlush, stdout)
import Types.Event
import Logic.AnalisisTemporal

menuAnalisisTemp :: [Event] -> IO () 
menuAnalisisTemp eventos = do
    putStrLn "--- Analisis temporal ---"
    putStrLn "1. Maximo monto mensual y dia más activo"
    putStrLn "2. Ver evento más reciente y más antiguo"
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
            putStrLn ""
            analizarMensualYDiario eventos
            menuAnalisisTemp eventos

        "2" -> do
            putStrLn ""
            obtenerEventosExtremos eventos
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
