{-
Módulo: UI.MenuEstadisticas
Descripción: Contiene la interfaz de usuario para la visualización de estadísticas generales
    de los eventos y la exportación de reportes
-}
module UI.MenuEstadisticas where

import System.IO (hFlush, stdout)
import Types.Event
import Logic.Estadisticas
import Logic.Busqueda (mostrarEvento)

{-
Nombre: menuEstadisticas

Esta función se encarga de mostrar un resumen estadístico de los eventos en consola,
    incluyendo cantidad por categoría, eventos extremos y el día más activo.
    Además, permite exportar el reporte en formato JSON.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve un tipo IO (), que representa una acción de interacción en consola
-}
menuEstadisticas :: [Event] -> IO ()
menuEstadisticas [] = do
    putStrLn "No hay eventos cargados"
    putStrLn "Presione Enter para volver..."
    _ <- getLine
    return ()

menuEstadisticas eventos = do
    let (cats, (alto, bajo), (dia, cant)) = obtenerResumen eventos
    
    putStrLn "\n========================================"
    putStrLn "       RESUMEN DE ESTADISTICAS"
    putStrLn "========================================"
    putStrLn "Eventos por Categoria:"
    mapM_ (\(c, n) -> putStrLn $ " - " ++ c ++ ": " ++ show n) cats
    
    putStrLn "\n>>> EVENTO DE MAYOR VALOR <<<"
    putStr $ mostrarEvento alto
    
    putStrLn ">>> EVENTO DE MENOR VALOR <<<"
    putStr $ mostrarEvento bajo
    
    putStrLn $ "Dia mas activo: " ++ dia ++ " (" ++ show cant ++ " eventos)"
    putStrLn "========================================"
    
    putStrLn "\n¿Desea exportar el reporte a JSON?"
    putStrLn "1. Si"
    putStrLn "0. No"
    putStr "> "
    hFlush stdout
    
    op <- getLine
    case op of
        "1" -> do
            let contenido = generarJSON cats (alto, bajo) (dia, cant)
            writeFile "reporte.json" contenido
            putStrLn "\n[!] 'reporte.json' creado."
            putStrLn "Presione Enter para continuar..."
            _ <- getLine
            return ()
        _ -> return ()