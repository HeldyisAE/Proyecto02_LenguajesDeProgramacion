module UI.MenuAnalisisDatos where

import System.IO (hFlush, stdout)
import Helpers.HerramientasImpresion
import Types.Event
import Logic.AnalisisDatos

menuAnalisisDatos :: [Event] -> IO ()
menuAnalisisDatos eventos = do
    putStrLn "--- Analisis de datos ---"
    putStrLn "1. Monto total (Importe total de todos los eventos)"
    putStrLn "2. Promedio anual por categoría"
    putStrLn "0. Volver"
    putStr "> "
    hFlush stdout
    
    op <- getLine
    procesarDataOp op eventos

procesarDataOp :: String -> [Event] -> IO ()
procesarDataOp opcion eventos =
    case opcion of
        "1" -> do
            let resumen = calcularResumen eventos
            putStrLn ""
            putStrLn "   Desglose del total de importes   "
            putStrLn "------------------------------------"
            putStrLn $ "Ganancias: " ++ mostrarMonto (ganancias resumen) ++ " colones"
            putStrLn $ "Devoluciones: " ++ mostrarMonto (devoluciones resumen) ++ " colones"
            putStrLn $ "Total general: " ++ mostrarMonto (total resumen) ++ " colones"
            putStrLn "------------------------------------"
            putStrLn ""
            menuAnalisisDatos eventos

        "2" -> do
            let promedios = promedioCategoriaAnual eventos
            putStrLn ""
            putStrLn "   Promedio anual por categoria   "
            putStrLn "----------------------------------"
            mapM_ (\(cat, year, prom) -> 
                putStrLn $ cat ++ " (" ++ show year ++ "): " ++ show prom
                ) promedios
            putStrLn "----------------------------------"
            putStrLn ""
            menuAnalisisDatos eventos

        "0" -> do
            putStrLn ""
            return ()

        _ -> do
            putStrLn "Opción inválida"
            putStrLn ""
            menuAnalisisDatos eventos
