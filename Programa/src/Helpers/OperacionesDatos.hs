{-
Módulo: Helpers.OperacionesDatos
Descripción: Contiene la lógica de funciones auxiliares para el manejo de datos
-}
module Helpers.OperacionesDatos where

import Types.Event

{-
Nombre: calcularImporte

Esta función se encarga de calcular el importe total de un evento en función de su categoría,
    aplicando reglas específicas para cada tipo de evento
Para eventos de tipo compra y apartado se calcula el valor total multiplicando el valor por la cantidad,
    en el caso de devolución se retorna el valor en negativo, y para apartado se aplica un porcentaje del 20%

Parámetros: Recibe un valor de tipo Event que representa un objeto evento

Retorno: Devuelve un valor de tipo Float que representa el importe calculado del evento
-}
calcularImporte :: Event -> Float
calcularImporte evento = 
    case category evento of
        "compra" -> value evento * fromIntegral (amount evento)
        "devolucion" -> -(value evento * fromIntegral (amount evento))
        "apartado" -> value evento * fromIntegral (amount evento) * 0.20
        _ -> value evento * fromIntegral (amount evento)

