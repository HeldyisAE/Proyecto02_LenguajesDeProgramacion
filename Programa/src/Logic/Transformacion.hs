{-
Módulo: Logic.Transformacion
Descripción: Contiene funciones para la transformación de eventos, incluyendo la aplicación de impuestos
    y la identificación de eventos de alto valor
-}
module Logic.Transformacion where

import Types.Event
import Logic.AnalisisDatos (promedioCategoriaAnual)

{-
Nombre: aplicarImpuesto

Esta función se encarga de aplicar un impuesto del 13% a los eventos de tipo compra
    que no tengan un impuesto previamente aplicado, generando una nueva lista con los cambios.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve una lista de valores de tipo Event con el impuesto aplicado cuando corresponde
-}
aplicarImpuesto :: [Event] -> [Event]
aplicarImpuesto eventos = map transformar eventos
  where
    transformar e
        | category e == "compra" && tax e == 0 = 
            let montoImpuesto = value e * (0.13 :: Float)
            in e { value = value e + montoImpuesto, tax = montoImpuesto }
        
        | otherwise = e

{-
Nombre: eventosAltoValor

Esta función se encarga de identificar los eventos cuyo valor supera el promedio de su categoría,
    considerando los promedios calculados de forma anual.

Parámetros: Recibe una lista de valores de tipo Event

Retorno: Devuelve una lista de valores de tipo Event que representan los eventos de alto valor
-}
eventosAltoValor :: [Event] -> [Event]
eventosAltoValor eventos = 
    filter (\e -> value e > obtenerPromedio (category e)) eventos
  where
    promedios = promedioCategoriaAnual eventos 
    obtenerPromedio cat = 
        case filter (\(c, _, _) -> c == cat) promedios of
            ((_, _, p):_) -> p
            []            -> 0