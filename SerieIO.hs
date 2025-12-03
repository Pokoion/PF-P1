module SerieIO
  ( imprimirCatalogoConsola
  , cargarCatalogosDesdeFichero
  ) where

import Serie
import SerieParser (convertirLineaEnSerie)

imprimirCatalogoConsola :: [Serie] -> IO ()
imprimirCatalogoConsola lista =
  putStr ("Catálogo de Series\n" ++
          concat [show i ++ ". " ++ printSerie s | (i, s) <- zip [1..] lista])

cargarCatalogosDesdeFichero :: FilePath -> IO [Serie]
cargarCatalogosDesdeFichero filepath = do
  contenido <- readFile filepath
  let lineas = lines contenido
  return (map convertirLineaEnSerie lineas)