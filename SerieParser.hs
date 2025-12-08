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

parseGeneroS:: String -> Either String GeneroS
parseGeneroS generoS =
    case generoS of 
        "Accion"-> Right Accion
        "Animacion"-> Right Animacion
        "Comedia"-> Right Comedia
        "Drama" -> Right Drama
        "Documental" -> Right Documental
        "SciFic" -> Right SciFic
        "Suspense" -> Right Suspense
        "Romance" -> Right Romance
        "Terror" -> Right Terror
        _ -> Left ("Genero no válido: '" ++ generoS ++ "'")

readEither :: String -> String -> Either String Int
readEither campo s =
  case reads s of
    [(v,"")] -> Right v
    _        -> Left ("Campo " ++ campo ++ " inválido: '" ++ s ++ "'")

convertirLineaEnSerieE :: String -> Either String Serie
convertirLineaEnSerieE linea =
  case splitOn ";" linea of
    [titulo, temp, epis, dur, gen, ed] -> do
      ntemporadas <- readEither "NTemporadas" temp
      episodios   <- readEither "EpisodiosXTemporada" epis
      duracion    <- readEither "DuracionM" dur
      genero      <- parseGeneroS gen
      edad        <- readEither "Edad" ed
      Right (titulo, ntemporadas, episodios, duracion, genero, edad)
    xs -> Left ("Se esperaban 6 campos; llegaron " ++ show (length xs))

convertirLineaEnSerie :: String -> Serie
convertirLineaEnSerie linea =
  case convertirLineaEnSerieE linea of
    Right s  -> s
    Left msg -> error ("Error de parseo: " ++ msg)