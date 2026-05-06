{-
Módulo: Logic.Estadisticas
Descripción: Contiene funciones para el cálculo de estadísticas generales de los eventos
    y la generación de reportes en formato JSON
-}
module Logic.Estadisticas where

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Types.Event (Event(..))
import Helpers.OperacionesTemporales (agruparPor, extraerFechaExacta)
import Helpers.HerramientasImpresion (mostrarMonto)

{-
Nombre: obtenerResumen

Esta función se encarga de calcular un resumen estadístico de una lista de eventos,
    incluyendo el conteo por categoría, el evento de mayor y menor valor,
    y el día con mayor cantidad de eventos.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve una tupla de tipo ([(String, Int)], (Event, Event), (String, Int))
    que representa el conteo por categoría, los eventos extremos y el día más activo
-}
obtenerResumen :: [Event] -> ([(String, Int)], (Event, Event), (String, Int))
obtenerResumen eventos = 
    let cats = map (\(c, evs) -> (c, length evs)) (agruparPor category eventos)
        alto = maximumBy (comparing value) eventos
        bajo = minimumBy (comparing value) eventos
        diaPico = maximumBy (comparing snd) $ map (\(f, evs) -> (f, length evs)) (agruparPor (extraerFechaExacta . timestamp) eventos)
    in (cats, (alto, bajo), diaPico)

{-
Nombre: generarJSON

Esta función se encarga de generar un reporte en formato JSON a partir de los datos estadísticos,
    incluyendo el conteo por categoría, eventos extremos y el día más activo.

Parámetros: Recibe una lista de categorías con sus cantidades, una tupla con los eventos extremos
    y una tupla que representa el día más activo

Retorno: Devuelve un valor de tipo String que representa el contenido del archivo JSON
-}
generarJSON :: [(String, Int)] -> (Event, Event) -> (String, Int) -> String
generarJSON cats (alto, bajo) (dia, cant) =
    "{\n" ++
    "  \"reporte_estadisticas\": {\n" ++
    "    \"conteo_categorias\": [\n" ++
    intercalarStrings "," (map (\(c, n) -> "      {\"categoria\": \"" ++ c ++ "\", \"cantidad\": " ++ show n ++ "}") cats) ++
    "\n    ],\n" ++
    "    \"evento_monto_maximo\": {\n" ++
    "      \"id\": " ++ show (idEvent alto) ++ ",\n" ++
    "      \"valor\": " ++ mostrarMonto (value alto) ++ ",\n" ++
    "      \"fecha\": \"" ++ extraerFechaExacta (timestamp alto) ++ "\"\n" ++
    "    },\n" ++
    "    \"evento_monto_minimo\": {\n" ++
    "      \"id\": " ++ show (idEvent bajo) ++ ",\n" ++
    "      \"valor\": " ++ mostrarMonto (value bajo) ++ ",\n" ++
    "      \"fecha\": \"" ++ extraerFechaExacta (timestamp bajo) ++ "\"\n" ++
    "    },\n" ++
    "    \"dia_mas_activo\": {\n" ++
    "      \"fecha\": \"" ++ dia ++ "\",\n" ++
    "      \"total_eventos\": " ++ show cant ++ "\n" ++
    "    }\n" ++
    "  }\n" ++
    "}"

{-
Nombre: intercalarStrings

Esta función se encarga de unir una lista de strings utilizando un separador,
    insertando saltos de línea entre cada elemento.

Parámetros: Recibe un valor de tipo String como separador y una lista de strings

Retorno: Devuelve un valor de tipo String que representa los elementos concatenados
-}
intercalarStrings :: String -> [String] -> String
intercalarStrings _ [] = ""
intercalarStrings _ [x] = x
intercalarStrings sep (x:xs) = x ++ sep ++ "\n" ++ intercalarStrings sep xs