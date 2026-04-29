module UI.MenuTransformacion where

import System.IO (hFlush, stdout)

menuTransformacion:: IO()
menuTransformacion = do
    putStrLn "--- Transformación de eventos ---"
    putStrLn "1. Aplicar impuesto a compras"
    putStrLn "2. Etiquetar eventos de alto valor"
    putStrLn "0. Volver"
    putStr "> "
    hFlush stdout
    
    op <- getLine
    procesarTransOp op

procesarTransOp :: String -> IO () --Recibe string y retorna una acción del SO
procesarTransOp opcion =
    case opcion of
        "1" -> do
            putStrLn "Impuesto a las compras [Está pendiente]"
            putStrLn ""
            menuTransformacion

        "2" -> do
            putStrLn "Etiquetar eventos de alto valor [Está pendiente]"
            putStrLn ""
            menuTransformacion

        "0" -> do 
            putStrLn ""
            return ()

        _ -> do
            putStrLn "Opción inválida"
            putStrLn ""
            menuTransformacion
            