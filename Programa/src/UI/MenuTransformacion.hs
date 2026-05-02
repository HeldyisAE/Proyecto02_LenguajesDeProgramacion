module UI.MenuTransformacion where

import System.IO (hFlush, stdout)
import Types.Event
import Logic.Transformacion
import Logic.Busqueda (mostrarEvento)

-- Esta función maneja el menú y devuelve la lista (actualizada o no) al menú principal
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

-- Aquí se procesa la elección del usuario
procesarTransOp :: String -> [Event] -> IO [Event]
procesarTransOp opcion eventos =
    case opcion of
        "1" -> do
            let eventosTransformados = aplicarImpuesto eventos
            putStrLn "\n[!] Impuesto del 13% aplicado a las compras con éxito."
            -- RECURSIÓN: Te quedas en el submenú con los datos nuevos
            menuTransformacion eventosTransformados

        "2" -> do
            -- Filtramos los eventos que superan el promedio de su categoría[cite: 11]
            let altosValores = eventosAltoValor eventos
            putStrLn "\n--- Eventos de Alto Valor (Sobre el promedio) ---"
            if null altosValores
                then putStrLn "No hay eventos que superen el promedio actual."
                else mapM_ (putStr . mostrarEvento) altosValores
            -- Aquí no modificamos la lista, solo la mostramos, así que pasamos la original[cite: 12]
            menuTransformacion eventos

        "0" -> do
            -- Retornamos la lista final (con o sin impuestos) para que el Principal la use[cite: 11, 12]
            return eventos

        _ -> do
            putStrLn "Opción inválida."
            menuTransformacion eventos