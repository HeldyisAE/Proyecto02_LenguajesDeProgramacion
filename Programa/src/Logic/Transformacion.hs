module Logic.Transformacion where

import Types.Event
import Logic.AnalisisDatos (promedioCategoriaAnual)

--Aplicar impuesto(13%)
-- En esta se maneja la lista de eventos y cuando se calcula todo lo que devuelve es la lista pero con los cambios aplicados
-- tecnicamente lo que hace es a partir de la lista original va creando una con los cambios 
-- y esa es la que manda de vuelta al menu
aplicarImpuesto :: [Event] -> [Event]
aplicarImpuesto eventos = map transformar eventos
  where
    transformar e
        | category e == "compra" = e { value = value e * 1.13 }
        | otherwise              = e

--Etiquetar eventos de alto valor
eventosAltoValor :: [Event] -> [Event]
eventosAltoValor eventos = 
    filter (\e -> value e > obtenerPromedio (category e)) eventos
  where
    promedios = promedioCategoriaAnual eventos 
    obtenerPromedio cat = 
        case filter (\(c, _, _) -> c == cat) promedios of
            ((_, _, p):_) -> p
            []            -> 0