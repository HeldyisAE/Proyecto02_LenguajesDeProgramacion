module Helpers.OperacionesDatos where

import Types.Event

calcularImporte :: Event -> Float
calcularImporte evento = 
    case category evento of
        "compra" -> value evento * fromIntegral (amount evento)
        "devolucion" -> -(value evento * fromIntegral (amount evento))
        "apartado" -> value evento * fromIntegral (amount evento) * 0.20
        _ -> value evento * fromIntegral (amount evento)

