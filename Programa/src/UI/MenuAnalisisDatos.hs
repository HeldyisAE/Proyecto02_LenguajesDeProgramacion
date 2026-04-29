module UI.MenuAnalisisDatos where

import System.IO (hFlush, stdout)

menuAnalisisDatos :: IO ()
menuAnalisisDatos = do
    putStrLn "--- Analisis de datos ---"
    putStrLn "1. Monto total (Importe de todos los eventos)"
    putStrLn "2. Promedio anual por categoría"
    putStrLn "0. Volver"
    putStr "> "
    hFlush stdout
    
    op <- getLine
    procesarDataOp op

procesarDataOp :: String -> IO ()
procesarDataOp opcion =
    case opcion of
        "1" -> do
            putStrLn "Monto total [Está pendiente]"
            putStrLn ""
            menuAnalisisDatos

        "2" -> do
            putStrLn "Promedio anual por categoria [Está pendiente]"
            putStrLn ""
            menuAnalisisDatos

        "0" -> do
            putStrLn ""
            return ()

        _ -> do
            putStrLn "Opción inválida"
            putStrLn ""
            menuAnalisisDatos
