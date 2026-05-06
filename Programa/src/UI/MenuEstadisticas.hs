module UI.MenuEstadisticas where

import System.IO (hFlush, stdout)
import Types.Event
import Logic.Estadisticas
import Logic.Busqueda (mostrarEvento)

menuEstadisticas :: [Event] -> IO ()
menuEstadisticas [] = putStrLn "No hay eventos cargados"
menuEstadisticas eventos = do
    let (cats, (alto, bajo), (dia, cant)) = obtenerResumen eventos
    
    putStrLn "\n========================================"
    putStrLn "              ESTADISTICAS"
    putStrLn "========================================"
    putStrLn "Eventos por Categoria:"
    mapM_ (\(c, n) -> putStrLn $ " - " ++ c ++ ": " ++ show n) cats
    
    putStrLn "\n>>> EVENTO DE MAYOR VALOR <<<"
    putStr $ mostrarEvento alto
    
    putStrLn ">>> EVENTO DE MENOR VALOR <<<"
    putStr $ mostrarEvento bajo
    
    putStrLn $ "Dia mas activo: " ++ dia ++ " (" ++ show cant ++ " eventos)"
    putStrLn "========================================"
    
    putStrLn "\n¿Desea exportar el reporte completo a JSON?"
    putStrLn "1. Si"
    putStrLn "0. No"
    putStr "> "
    hFlush stdout
    
    op <- getLine
    if op == "1" 
        then do
            writeFile "reporte.json" (generarJSON cats (alto, bajo) (dia, cant))
            putStrLn "\n[!] 'reporte.json' guardado."
            getLine >> return ()
        else return ()