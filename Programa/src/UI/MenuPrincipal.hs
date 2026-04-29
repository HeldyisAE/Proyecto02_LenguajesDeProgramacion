module UI.MenuPrincipal where

import System.IO (hFlush, stdout)
import UI.MenuTransformacion
import UI.MenuAnalisisDatos
import UI.MenuAnalisisTemp

menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "--- Menu Principal ---"
    putStrLn "1. Transformacion de eventos"
    putStrLn "2. Analisis de datos"
    putStrLn "3. Analisis temporal"
    putStrLn "4. Busqueda"
    putStrLn "5. Estadisticas"
    putStrLn "0. Salir"
    putStr "> "
    hFlush stdout -- Fuerza a imprimir primero el "> " vaciando el buffer

    op <- getLine --Obtiene la entrada del usuario
    procesarOpcion op

procesarOpcion :: String -> IO () --Recibe string y retorna una acción del SO
procesarOpcion opcion =
    case opcion of
        "1" -> do
            putStrLn ""
            menuTransformacion
            menuPrincipal

        "2" -> do
            putStrLn ""
            menuAnalisisDatos
            menuPrincipal

        "3" -> do
            putStrLn ""
            menuAnalisisTemp
            menuPrincipal

        "4" -> do
            putStrLn "Opciones de búsqueda [Está pendiente]"
            menuPrincipal

        "5" -> do
            putStrLn "Estadisticas [Está pendiente]"
            menuPrincipal

        "0" -> do
            putStrLn "Saliendo del programa..."

        _ -> do
            putStrLn "Opción inválida"
            menuPrincipal