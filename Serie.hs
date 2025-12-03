module Serie
  ( Titulo, Serie, GeneroS(..)
  , Edad, NTemporadas, EpisodiosXTemporada, DuracionM
  , getTituloS, getTemporadas, getEpisodiosXT, getDuracionEp
  , getGeneroS, getEdad
  , printSerie, printSeries
  , qsortBy
  ) where

type Titulo = String
type Serie = (Titulo, NTemporadas, EpisodiosXTemporada, DuracionM, GeneroS, Edad )
data GeneroS = Accion | Animacion | Comedia | Drama | Documental | SciFic | Suspense | Romance | Terror deriving(Eq,Ord,Show)
type Edad = Int                -- edad minima para consumir el contenido 
type NTemporadas = Int
type EpisodiosXTemporada = Int -- promedio
type DuracionM = Int           -- minutos, promedio de duracion de los episodios

-- Extrae el titulo de la serie.
getTituloS :: Serie -> Titulo
getTituloS (titulo, _, _, _, _, _) = titulo

-- Extrae el num. de temporadas.
getTemporadas :: Serie -> NTemporadas
getTemporadas (_, ntemporadas, _, _, _, _) = ntemporadas

--Extrae los capitulos por temporada
getEpisodiosXT :: Serie -> EpisodiosXTemporada
getEpisodiosXT (_, _, episodios, _, _, _) = episodios

-- Extrae la duracion por episodio.
getDuracionEp :: Serie -> DuracionM
getDuracionEp (_, _, _, duracionm, _, _) = duracionm

-- Extrae el genero de la serie AV.
getGeneroS :: Serie -> GeneroS
getGeneroS (_, _, _, _, generos, _) = generos

-- Extrae la edad minima recomendada.
getEdad :: Serie -> Edad
getEdad (_, _, _, _, _, edad) = edad

-- Titulo, Nº de Temporadas, y Edad minima de la serie, seguido de salto de linea
printSerie :: Serie -> String
printSerie serie = 
    getTituloS serie ++ " - Numero de temporadas: " ++ show (getTemporadas serie) ++ 
    " - Edad minima recomendada: " ++ show (getEdad serie) ++ "\n"

-- Imprime la lista completa de series (playlist), formateada
printSeries :: [Serie] -> IO ()
printSeries = putStr .concat .map printSerie

-- Implementacion del quicksort por clave
qsortBy :: Ord b => (a -> b) -> [a] -> [a]
qsortBy _ [] = []
qsortBy f (x:xs) =
    qsortBy f [y | y <- xs, f y < f x] ++
    (x : qsortBy f [y | y <- xs, f y >= f x])
