module Logic.Transformacion where

import Types.Event
import Logic.AnalisisDatos (promedioCategoriaAnual)

-- 1. Aplicar impuesto del 13% solo a compras
aplicarImpuesto :: [Event] -> [Event]
aplicarImpuesto eventos = map transformar eventos
  where
    transformar e
        | category e == "compra" = e { value = value e * 1.13 }
        | otherwise              = e

-- 2. Etiquetar eventos de alto valor[cite: 1]
-- Filtra eventos que superan el promedio de su categoría
eventosAltoValor :: [Event] -> [Event]
eventosAltoValor eventos = 
    filter (\e -> value e > obtenerPromedio (category e)) eventos
  where
    promedios = promedioCategoriaAnual eventos 
    obtenerPromedio cat = 
        case filter (\(c, _, _) -> c == cat) promedios of
            ((_, _, p):_) -> p
            []            -> 0