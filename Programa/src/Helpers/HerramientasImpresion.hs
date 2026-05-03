module Helpers.HerramientasImpresion where 

import Text.Printf

mostrarMonto :: Float -> String
mostrarMonto numero = printf "%.2f" numero