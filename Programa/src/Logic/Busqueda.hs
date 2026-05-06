{-
Modulo: Logic.Busqueda
Descripción: Este módulo contiene toda la lógica relacionada con la búsqueda por 
    fechas de eventos
-}
{- HLINT ignore "Eta reduce" -}
module Logic.Busqueda where

import Types.Event
import Helpers.OperacionesFechas

{-
Nombre: filtrarFecha

Esta función se encarga de filtrar una lista de eventos en función de un rango de fechas,
    considerando únicamente aquellos eventos cuyo timestamp se encuentre dentro del intervalo.

Parámetros: Recibe un valor de tipo Int como fecha inicial, un Int como fecha final
    y una lista de valores de tipo Event

Retorno: Devuelve una lista de valores de tipo Event que cumplen con el rango indicado
-}
filtrarFecha :: Int -> Int -> [Event] -> [Event]
filtrarFecha inicio fin eventos =
    filter (\e -> timestamp e >= inicio && timestamp e <= fin) eventos -- Se filtran las fechas

{-
Nombre: mostrarEvento

Esta función se encarga de convertir un evento a un formato de texto legible,
    mostrando todos sus atributos de forma estructurada.

Parámetros: Recibe un valor de tipo Event

Retorno: Devuelve un valor de tipo String que representa el evento formateado
-}
mostrarEvento :: Event -> String 
mostrarEvento evento =
    "\nID: " ++ show (idEvent evento) ++ "\n" ++
    "Categoria: " ++ category evento ++ "\n" ++
    "Valor: " ++ show (value evento) ++ " colones \n" ++
    "Fecha: " ++ formatearFecha (timestamp evento) ++ "\n" ++
    "Cantidad: " ++ show (amount evento) ++ "\n" ++
    "Impuesto aplicado: " ++ show (tax evento) ++ "\n" ++
    "---------------------------\n"

{-
Nombre: imprimirEventosPorFecha

Esta función se encarga de mostrar en consola los eventos que se encuentran dentro
    de un rango de fechas específico.

Parámetros: Recibe un valor de tipo Int como fecha inicial, un Int como fecha final
    y una lista de valores de tipo Event

Retorno: Devuelve un tipo IO (), que representa una acción de salida en consola
-}
imprimirEventosPorFecha :: Int -> Int -> [Event] -> IO ()
imprimirEventosPorFecha inicio fin eventos = do
    let eventosFiltrados = filtrarFecha inicio fin eventos
    mapM_ (putStr . mostrarEvento) eventosFiltrados --Imprime cada evento hacia abajo
