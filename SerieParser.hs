module SerieParser
  ( splitOn
  , parseGeneroS
  , convertirLineaEnSerie
  ) where

import Serie (Serie, GeneroS(..))

splitOn :: String -> String -> [String]
splitOn delim = split'
  where
    split' [] = [""]
    split' s = 
      case startsWith delim s of
        Just rest -> "" : split' rest
        Nothing -> let (c:cs) = s
                       (x:xs) = split' cs 
                   in (c:x):xs
    
    startsWith [] s = Just s
    startsWith _ [] = Nothing
    startsWith (d:ds) (c:cs)
      | d == c = startsWith ds cs
      | otherwise = Nothing

parseGeneroS:: String -> GeneroS
parseGeneroS generoS =
    case generoS of 
        "Accion"-> Accion
        "Animacion"->Animacion
        "Comedia"->Comedia
        "Drama" -> Drama
        "Documental" -> Documental
        "SciFic" -> SciFic
        "Suspense" -> Suspense
        "Romance" -> Romance
        "Terror" -> Terror

convertirLineaEnSerie :: String -> Serie
convertirLineaEnSerie linea = (titulo, ntemporadas, episodios, duracion, genero, edad)
  where
    [titulo, temp, epis, dur, gen, ed] = splitOn ";" linea
    ntemporadas = read temp :: Int
    episodios   = read epis :: Int
    duracion    = read dur  :: Int
    genero      = parseGeneroS gen
    edad        = read ed   :: Int