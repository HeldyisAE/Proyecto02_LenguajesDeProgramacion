module UI.MenuTransformacion where

import System.IO (hFlush, stdout)
import Types.Event
import Logic.Transformacion
import Logic.Busqueda (mostrarEvento)

menuTransformacion :: [Event] -> IO [Event]
menuTransformacion eventos = do
    putStrLn "\n--- Transformación de eventos ---"
    putStrLn "1. Aplicar impuesto a compras (13%)"
    putStrLn "2. Etiquetar eventos de alto valor"
    putStrLn "0. Volver"
    putStr "> "
    hFlush stdout
    
    opcion <- getLine
    procesarTransOp opcion eventos

procesarTransOp :: String -> [Event] -> IO [Event]
procesarTransOp opcion eventos =
    case opcion of
        "1" -> do
            let eventosTransformados = aplicarImpuesto eventos
            putStrLn "\n[!] Impuesto(13%) aplicado a las compras."
            menuTransformacion eventosTransformados

        "2" -> do
            -- Filtra los eventos que superan el promedio
            let altosValores = eventosAltoValor eventos
            putStrLn "\n--- Eventos de Alto Valor---"
            if null altosValores
                then putStrLn "No hay eventos que superen el promedio actual."
                else mapM_ (putStr . mostrarEvento) altosValores
            menuTransformacion eventos

        "0" -> do
            return eventos

        _ -> do
            putStrLn "Opción inválida."
            menuTransformacion eventos