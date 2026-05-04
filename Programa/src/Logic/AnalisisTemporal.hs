module Logic.AnalisisTemporal where

import Data.List
import Data.Ord
--import Data.Function (on)
import Types.Event (Event(..))
import Helpers.OperacionesTemporales
import Logic.AnalisisDatos (montoTotal)

---------------------------------------Mes con mayor monto y dia de la semana más activo-------------------------------------------

data ResultadoMes = ResultadoMes
    { anio :: Integer
    , mes :: Int
    , montoMes :: Float
    , diaActivo :: String
    , eventosDia :: [Event]
    } deriving (Show)

diaMasActivo :: [Event] -> (String, [Event])
diaMasActivo eventos =
    let porDia      = agruparPor (extraerFechaExacta . timestamp) eventos
        masActivo   = maximumBy (comparing (length . snd)) porDia
        fecha       = fst masActivo
        evsDia      = snd masActivo
        ts          = timestamp (head evsDia)
        (nombreDia, _) = extraerDiaSemana ts
    in (nombreDia ++ " " ++ fecha, evsDia)

construirResultado :: ((Integer, Int), [Event], Float) -> ResultadoMes
construirResultado ((y, m), eventos, monto) =
    let (fecha, evsDia) = diaMasActivo eventos
    in ResultadoMes
        { anio       = y
        , mes        = m
        , montoMes   = monto
        , diaActivo  = fecha
        , eventosDia = evsDia
        }

analizarAnio :: [Event] -> [ResultadoMes]
analizarAnio eventos =
    let porMes      = agruparPor (extraerAnioMes . timestamp) eventos
        conMontos   = map (\(k, evs) -> (k, evs, montoTotal evs)) porMes
        maxMonto    = maximum (map (\(_, _, m) -> m) conMontos)
        mejores     = filter (\(_, _, m) -> m == maxMonto) conMontos
    in map construirResultado mejores

analizarMensualYDiario :: [Event] -> IO ()
analizarMensualYDiario eventos = do 
    let porAnio = agruparPor (extraerAnio . timestamp) eventos 
        resultados = concatMap (analizarAnio . snd) porAnio
    mapM_ imprimirResultado resultados

imprimirEvento :: Event -> IO ()
imprimirEvento e =
    putStrLn $ "    [ID: " ++ show (idEvent e) ++ "] " ++ category e

imprimirResultado :: ResultadoMes -> IO ()
imprimirResultado r = do
    putStrLn $ "Año: " ++ show (anio r)
    putStrLn $ "  Mes con mayor monto: " ++ nombreMes (mes r)
    putStrLn $ "  Monto total: " ++ show (montoMes r)
    putStrLn $ "  Día más activo: " ++ diaActivo r
    putStrLn $ "  Eventos ese día: " ++ show (length (eventosDia r))
    mapM_ imprimirEvento (eventosDia r)
    putStrLn ""

---------------------------------------Mes con mayor monto y dia de la semana más activo-------------------------------------------