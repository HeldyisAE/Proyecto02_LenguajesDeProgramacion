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
-- revisa en la lista los de mayor al promedio pero ademas para saber cuales son nuevos eventos se agrega tag que viene en
-- false y se cambia a true si el evento es mayor al promedio y asi se mantiene la lista actualizada
eventosAltoValor :: [Event] -> [Event]
eventosAltoValor eventos =
    let 
        valores = map value eventos
        promedio = sum valores / fromIntegral (length valores)
        
        marcar e = if tag e || value e > promedio 
                   then e { tag = True } 
                   else e
    in 
        map marcar eventos