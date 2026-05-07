{-
Módulo: UI.MenuAnalisisDatos
Descripción: Contiene la interfaz de usuario para el análisis de datos, permitiendo visualizar
    métricas como el monto total y promedios por categoría
-}
module UI.MenuAnalisisDatos where

import System.IO (hFlush, stdout)
import Helpers.HerramientasImpresion
import Types.Event
import Logic.AnalisisDatos

{-
Nombre: menuAnalisisDatos

Esta función se encarga de mostrar el menú de análisis de datos en consola,
    permitiendo al usuario seleccionar diferentes opciones de análisis.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve un tipo IO (), que representa una acción de interacción en consola
-}
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

{-
Nombre: procesarDataOp

Esta función se encarga de procesar la opción seleccionada por el usuario
    dentro del menú de análisis de datos, ejecutando la acción correspondiente.

Parámetros: Recibe un valor de tipo String que representa la opción ingresada
    y una lista de valores de tipo Event

Retorno: Devuelve un tipo IO (), que representa una acción de interacción en consola
-}
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
