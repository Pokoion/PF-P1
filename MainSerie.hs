module MainSerie where

import SerieIO (cargarCatalogosDesdeFichero, imprimirCatalogoConsola)
import SerieManager (rankingSeriesMasBreves, miSeleccionDeSeriesMasCortasQue)

mainCatalogo :: IO ()
mainCatalogo = do
    putStrLn "Escribe el nombre del fichero a cargar"
    fichero <- getLine
    series <- cargarCatalogosDesdeFichero fichero
    imprimirCatalogoConsola series

    putStrLn "\nOrdenación por duración:"
    imprimirCatalogoConsola (map (\x -> fst x) (rankingSeriesMasBreves series))

    putStrLn "\nFiltrado por duración:"
    imprimirCatalogoConsola (miSeleccionDeSeriesMasCortasQue 5 1000 series)