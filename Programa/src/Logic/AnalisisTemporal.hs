{-
Módulo: Logic.AnalisisTemporal
Descripción: Contiene funciones para el análisis temporal de los eventos, incluyendo métricas por mes, día e intervalos de tiempo
-}
module Logic.AnalisisTemporal where


import Data.List
import Data.Ord
import Types.Event
import Helpers.OperacionesTemporales
import Logic.AnalisisDatos (montoTotal)
import Logic.Busqueda
import Helpers.HerramientasImpresion (mostrarMonto)

---------------------------------------Mes con mayor monto y dia de la semana más activo-------------------------------------------

{-
Nombre: mesMayorImporte

Esta función se encarga de agrupar los eventos por año y mes,
    calculando el monto total de cada grupo y retornando aquellos con el mayor importe.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve una lista de tuplas de tipo ((Integer, Int), Float)
    que representan el año, mes y el monto máximo encontrado
-}
mesMayorImporte :: [Event] -> [((Integer, Int), Float)]
mesMayorImporte eventos =
    let porMes = agruparPor (extraerAnioMes . timestamp) eventos
        contadorMontos = map (\(k, evts) -> (k, montoTotal evts)) porMes
        maxMonto = maximum (map snd contadorMontos)
    in filter (\(_,m) -> m == maxMonto) contadorMontos

{-
Nombre: diaMasActivo

Esta función se encarga de agrupar los eventos por día de la semana,
    contando la cantidad de eventos por cada día y retornando los más activos.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve una lista de tuplas de tipo (String, Int)
    que representan el día de la semana y la cantidad máxima de eventos
-}
diaMasActivo :: [Event] -> [(String, Int)]
diaMasActivo eventos =
    let porDia      = agruparPor (fst . extraerDiaSemana . timestamp) eventos
        contador = map (\(k, evts) -> (k, length evts)) porDia
        maxContador = maximum (map snd contador)
    in filter (\(_, c) -> c == maxContador) contador

{-
Nombre: analizarMensualYDiario

Esta función se encarga de mostrar en consola el mes con mayor monto
    y el día de la semana con mayor actividad de eventos.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve un tipo IO (), que representa una acción de salida en consola
-}
analizarMensualYDiario :: [Event] -> IO ()
analizarMensualYDiario [] = putStrLn "No hay eventos para analizar."
analizarMensualYDiario eventos = do
    putStrLn "--- Mes(es) con mayor monto ---"
    mapM_ imprimirMes (mesMayorImporte eventos)
    putStrLn ""
    putStrLn "--- Día(s) de la semana más activo ---"
    mapM_ imprimirDia (diaMasActivo eventos)
    putStrLn ""

{-
Nombre: imprimirMes

Esta función se encarga de imprimir en consola la información de un mes
    con su respectivo año y monto total.

Parámetros: Recibe una tupla de tipo ((Integer, Int), Float)

Retorno: Devuelve un tipo IO (), que representa una acción de salida en consola
-}
imprimirMes :: ((Integer, Int), Float) -> IO ()
imprimirMes ((y, m), monto) =
    putStrLn $ "  " ++ nombreMes m ++ " " ++ show y ++ " --- " ++ show monto ++ " Colones"

{-
Nombre: imprimirDia

Esta función se encarga de imprimir en consola el día de la semana
    junto con la cantidad de eventos asociados.

Parámetros: Recibe una tupla de tipo (String, Int)

Retorno: Devuelve un tipo IO (), que representa una acción de salida en consola
-}
imprimirDia :: (String, Int) -> IO ()
imprimirDia (dia, count) =
    putStrLn $ "  " ++ dia ++ " --- " ++ show count ++ " eventos"

---------------------------------------Evento más antiguo y más reciente-------------------------------------------

{-
Nombre: obtenerEventosExtremos

Esta función se encarga de obtener y mostrar en consola el evento más antiguo
    y el más reciente dentro de una lista de eventos.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve un tipo IO (), que representa una acción de salida en consola
-}
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

---------------------------------------Resumen por intervalo-------------------------------------------

{-
Nombre: resumenPorIntervalo

Esta función se encarga de dividir los eventos en bloques de tiempo según un intervalo de días,
    mostrando un resumen de cada bloque.

Parámetros: Recibe una lista de valores de tipo Event y un entero que representa el intervalo en días

Retorno: Devuelve un tipo IO (), que representa una acción de salida en consola
-}
resumenPorIntervalo :: [Event] -> Int -> IO ()
resumenPorIntervalo eventos intervalo =
    let tsMin = minimum (map timestamp eventos)
        tsMax = maximum (map timestamp eventos)
        bloques = generarBloques tsMin tsMax (intervalo * 86400)
        resultados = map (resumenBloque eventos) bloques
    in do 
        putStrLn $ "Intervalo: " ++ show intervalo ++ " días"
        putStrLn (replicate 55 '-')
        mapM_ imprimirBloque resultados

{-
Nombre: generarBloques

Esta función se encarga de generar intervalos de tiempo a partir de un rango inicial y final,
    utilizando un tamaño de bloque determinado.

Parámetros: Recibe el timestamp inicial, el timestamp final y el tamaño del bloque en segundos

Retorno: Devuelve una lista de tuplas de tipo (Int, Int) que representan los intervalos generados
-}
generarBloques :: Int -> Int -> Int -> [(Int, Int)]
generarBloques inicio fin size 
    | inicio > fin = []
    | otherwise =
        let finBloque = min (inicio + size - 1) fin 
        in (inicio, finBloque) : generarBloques (inicio + size) fin size

{-
Nombre: resumenBloque

Esta función se encarga de calcular un resumen de eventos dentro de un intervalo específico,
    incluyendo la cantidad de eventos y el monto total.

Parámetros: Recibe una lista de valores de tipo Event y una tupla que representa el intervalo

Retorno: Devuelve una tupla de tipo (String, String, Int, Float)
    que representa la fecha inicial, fecha final, cantidad de eventos y monto total
-}
resumenBloque :: [Event] -> (Int, Int) -> (String, String, Int, Float)
resumenBloque eventos (inicio, fin) = 
    let rango = filter (\e -> timestamp e >= inicio && timestamp e <= fin) eventos
        cantidadEventos = length rango 
        monto = montoTotal rango 
        fechaInicio = extraerFechaExacta inicio 
        fechaFin = extraerFechaExacta fin 
    in (fechaInicio, fechaFin, cantidadEventos, monto)

{-
Nombre: imprimirBloque

Esta función se encarga de imprimir en consola la información de un bloque de tiempo,
    incluyendo el rango de fechas, la cantidad de eventos y el monto total.

Parámetros: Recibe una tupla de tipo (String, String, Int, Float)

Retorno: Devuelve un tipo IO (), que representa una acción de salida en consola
-}
imprimirBloque :: (String, String, Int, Float) -> IO ()
imprimirBloque (fechaInicio, fechaFin, cantidadEventos, monto) =
    putStrLn $ "Del " ++ fechaInicio ++ " al " ++ fechaFin
            ++ " | " ++ show cantidadEventos ++ " eventos"
            ++ " | " ++ mostrarMonto monto ++ " Colones"
