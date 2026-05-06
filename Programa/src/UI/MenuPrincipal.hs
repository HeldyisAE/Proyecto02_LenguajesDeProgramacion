{-
Módulo: UI.MenuPrincipal
Descripción: Contiene la interfaz principal del sistema, permitiendo navegar entre las diferentes
    funcionalidades como transformación, análisis, búsqueda y generación de eventos
-}
module UI.MenuPrincipal where

import System.IO (hFlush, stdout)
import Types.Event
import Data.GeneradorEventos
import UI.MenuTransformacion
import UI.MenuAnalisisDatos
import UI.MenuAnalisisTemp
import UI.OpcionBusqueda
import UI.MenuEstadisticas

{-
Nombre: menuPrincipal

Esta función se encarga de inicializar el menú principal del sistema,
    generando dinámicamente nuevos eventos y actualizando la lista existente.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve un tipo IO (), que representa una acción de interacción en consola
-}
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
    
{-
Nombre: mostrarMenu

Esta función se encarga de mostrar las opciones disponibles del menú principal
    y capturar la entrada del usuario.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve un tipo IO (), que representa una acción de interacción en consola
-}
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

{-
Nombre: procesarOpcion

Esta función se encarga de procesar la opción seleccionada por el usuario
    dentro del menú principal, redirigiendo a la funcionalidad correspondiente.

Parámetros: Recibe un valor de tipo String que representa la opción ingresada
    y una lista de valores de tipo Event

Retorno: Devuelve un tipo IO (), que representa una acción de interacción en consola
-}
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