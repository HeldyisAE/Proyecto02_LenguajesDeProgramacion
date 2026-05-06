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
        | category e == "compra" && tax e == 0 = 
            let montoImpuesto = value e * (0.13 :: Float)
            in e { value = value e + montoImpuesto, tax = montoImpuesto }
        
        | otherwise = e

--Etiquetar eventos de alto valor
-- actualiza las etiquetas de los de alto valor para saber cuales si y cuales no con elo booleano
eventosAltoValor :: [Event] -> [Event]
eventosAltoValor eventos = map etiquetar eventos
  where
    promedios = promedioCategoriaAnual eventos
    
    obtenerPromedio cat = 
        case filter (\(c, _, _) -> c == cat) promedios of
            ((_, _, p):_) -> p
            []            -> 0

    etiquetar e =
        let promedio = obtenerPromedio (category e)
        in if value e > promedio
           then e { tag = True }
           else e