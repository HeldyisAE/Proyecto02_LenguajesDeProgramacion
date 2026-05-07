{-
Módulo: Helpers.HerramientasImpresion
Descripción: Contiene funciones auxiliares para la impresión y formateo de datos
-}
module Helpers.HerramientasImpresion where 

import Text.Printf

{-
Nombre: mostrarMonto

Esta función se encarga de formatear un número decimal a dos cifras decimales
    para su correcta visualización como monto

Parámetros: Recibe un número de tipo Float

Retorno: Devuelve un valor de tipo String que representa el número formateado
-}
mostrarMonto :: Float -> String
mostrarMonto numero = printf "%.2f" numero