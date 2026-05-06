module Logic.Estadisticas where

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Types.Event (Event(..))
import Helpers.OperacionesTemporales (agruparPor, extraerFechaExacta)
import Helpers.HerramientasImpresion (mostrarMonto)

-- estadísticas
obtenerResumen :: [Event] -> ([(String, Int)], (Event, Event), (String, Int))
obtenerResumen eventos = 
    let cats = map (\(c, evs) -> (c, length evs)) (agruparPor category eventos)
        alto = maximumBy (comparing value) eventos
        bajo = minimumBy (comparing value) eventos
        diaPico = maximumBy (comparing snd) $ map (\(f, evs) -> (f, length evs)) (agruparPor (extraerFechaExacta . timestamp) eventos)
    in (cats, (alto, bajo), diaPico)

-- Genera JSON 
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

intercalarStrings :: String -> [String] -> String
intercalarStrings _ [] = ""
intercalarStrings _ [x] = x
intercalarStrings sep (x:xs) = x ++ sep ++ "\n" ++ intercalarStrings sep xs