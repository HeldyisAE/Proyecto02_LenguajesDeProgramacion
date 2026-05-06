module UI.MenuPrincipal where

import System.IO (hFlush, stdout)
import Types.Event
import Data.GeneradorEventos
import UI.MenuTransformacion
import UI.MenuAnalisisDatos
import UI.MenuAnalisisTemp
import UI.OpcionBusqueda
import UI.MenuEstadisticas

menuPrincipal :: [Event] -> IO ()
menuPrincipal arrEventos = do
    let idIncremental = length arrEventos
    if idIncremental >= 9000000
        then do
            mostrarMenu arrEventos
        else do
            eventos <- generarEventos idIncremental --Se generan eventos cada vez que inicia el menu principal
            let newArrEventos = arrEventos ++ eventos
            mostrarMenu newArrEventos
    

mostrarMenu :: [Event] -> IO ()
mostrarMenu eventos = do 
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
    procesarOpcion op eventos

procesarOpcion :: String -> [Event] -> IO () --Recibe string y retorna una acción del SO
procesarOpcion opcion eventos =
    case opcion of
        "1" -> do
            putStrLn ""
            eventosActualizados <- menuTransformacion eventos
            menuPrincipal eventosActualizados           

        "2" -> do
            putStrLn ""
            menuAnalisisDatos eventos
            menuPrincipal eventos

        "3" -> do
            putStrLn ""
            menuAnalisisTemp eventos
            menuPrincipal eventos

        "4" -> do
            busquedaPorFecha eventos
            menuPrincipal eventos 

        "5" -> do
            menuEstadisticas eventos
            menuPrincipal eventos

        "0" -> do
            putStrLn "Saliendo del programa..."

        _ -> do
            putStrLn "Opción inválida"
            menuPrincipal eventos