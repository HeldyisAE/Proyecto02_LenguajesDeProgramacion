module Logic.AnalisisTemporal where

import Data.List
import Data.Ord
--import Data.Function (on)
import Types.Event (Event(..))
import Helpers.OperacionesTemporales
import Logic.AnalisisDatos (montoTotal)
import Logic.Busqueda

---------------------------------------Mes con mayor monto y dia de la semana más activo-------------------------------------------

mesMayorImporte :: [Event] -> [((Integer, Int), Float)]
mesMayorImporte eventos =
    let porMes = agruparPor (extraerAnioMes . timestamp) eventos
        contadorMontos = map (\(k, evts) -> (k, montoTotal evts)) porMes
        maxMonto = maximum (map snd contadorMontos)
    in filter (\(_,m) -> m == maxMonto) contadorMontos

diaMasActivo :: [Event] -> [(String, Int)]
diaMasActivo eventos =
    let porDia      = agruparPor (fst . extraerDiaSemana . timestamp) eventos
        contador = map (\(k, evts) -> (k, length evts)) porDia
        maxContador = maximum (map snd contador)
    in filter (\(_, c) -> c == maxContador) contador

analizarMensualYDiario :: [Event] -> IO ()
analizarMensualYDiario [] = putStrLn "No hay eventos para analizar."
analizarMensualYDiario eventos = do
    putStrLn "--- Mes(es) con mayor monto ---"
    mapM_ imprimirMes (mesMayorImporte eventos)
    putStrLn ""
    putStrLn "--- Día(s) de la semana más activo ---"
    mapM_ imprimirDia (diaMasActivo eventos)
    putStrLn ""

imprimirMes :: ((Integer, Int), Float) -> IO ()
imprimirMes ((y, m), monto) =
    putStrLn $ "  " ++ nombreMes m ++ " " ++ show y ++ " --- " ++ show monto ++ " Colones"

imprimirDia :: (String, Int) -> IO ()
imprimirDia (dia, count) =
    putStrLn $ "  " ++ dia ++ " --- " ++ show count ++ " eventos"

---------------------------------------Evento más antiguo y más reciente-------------------------------------------

obtenerEventosExtremos :: [Event] -> IO ()
obtenerEventosExtremos eventos = do 
    let antiguo = minimumBy (comparing timestamp) eventos
        reciente = maximumBy (comparing timestamp) eventos
    putStrLn "--- Evento más antiguo ---"
    putStr (mostrarEvento antiguo)
    putStrLn ""
    putStrLn "--- Evento más reciente ---"
    putStr (mostrarEvento reciente)
    putStrLn ""

