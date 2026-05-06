{-
Módulo: Main
Descripción: Implementa la función main para la ejecución correcta del programa
-}
module Main where

import UI.MenuPrincipal 

{-
Nombre: main

Función main para la ejecución del programa

Retorna:
    Devuelve tipo IO que representa una acción con efectos secundarios
-}
main :: IO ()
main = do
    menuPrincipal []
