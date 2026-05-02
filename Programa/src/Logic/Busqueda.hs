{- HLINT ignore "Eta reduce" -}
module Logic.Busqueda where

import Types.Event
import Helpers.OperacionesFechas

filtrarFecha :: Int -> Int -> [Event] -> [Event]
filtrarFecha inicio fin eventos =
    filter (\e -> timestamp e >= inicio && timestamp e <= fin) eventos -- Se filtran las fechas

mostrarEvento :: Event -> String 
mostrarEvento evento =
    "\nID: " ++ show (idEvent evento) ++ "\n" ++
    "Categoria: " ++ category evento ++ "\n" ++
    "Valor: " ++ show (value evento) ++ " colones \n" ++
    "Fecha: " ++ formatearFecha (timestamp evento) ++ "\n" ++
    "Cantidad: " ++ show (cantidad evento) ++ "\n" ++
    "---------------------------\n"

imprimirEventosPorFecha :: Int -> Int -> [Event] -> IO ()
imprimirEventosPorFecha inicio fin eventos = do
    let eventosFiltrados = filtrarFecha inicio fin eventos
    mapM_ (putStr . mostrarEvento) eventosFiltrados --Imprime cada evento hacia abajo
