module UI.Menu where

menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "--- Menu Principal ---"
    putStrLn "1. Transformacion de eventos"
    putStrLn "2. Analisis de datos"
    putStrLn "3. Busqueda"
    putStrLn "4. Estadisticas"
    putStrLn "0. Salir"

    op <- getLine --Obtiene la entrada del usuario

    procesarOpcion op

procesarOpcion :: String -> IO () --Recibe string y retorna una acción del SO
procesarOpcion opcion =
    case opcion of
        "1" -> do
            putStrLn "Transformacion de eventos [Está pendiente]"
            menuPrincipal

        "2" -> do
            putStrLn "Analisis de datos [Está pendiente]"
            menuPrincipal

        "3" -> do
            putStrLn "Opciones de búsqueda [Está pendiente]"
            menuPrincipal

        "4" -> do
            putStrLn "Estadisticas [Está pendiente]"
            menuPrincipal

        "0" -> do
            putStrLn "Saliendo del programa..."

        _ -> do
            putStrLn "Opción inválida"
            menuPrincipal