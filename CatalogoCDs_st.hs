-------------------------------------------------------------------------------
--  PRACTICA: Gestion de Catalogo de Contenido Digital     
--  PF  2025-2026

--  Num. del equipo registrado en la egela: Eq_25
-- Apellidos del primer integrante: Oñatibia Garmendia
-- Apellidos del segundo integrante: Olea Goenaga
-------------------------------------------------------------------------------
-- GRUPO C: Desarrollo sobre Series
-------------------------------------------------------------------------------
module CatalogoCD where
-- import Data.XXX
type Titulo = String
type Serie = (Titulo, NTemporadas, EpisodiosXTemporada, DuracionM, GeneroS, Edad )
data GeneroS = Accion | Animacion | Comedia | Drama | Documental | SciFic | Suspense | Romance | Terror deriving(Eq,Ord,Show)
type Edad = Int                -- edad minima para consumir el contenido 
type NTemporadas = Int
type EpisodiosXTemporada = Int -- promedio
type DuracionM = Int           -- minutos, promedio de duracion de los episodios

-- ====================================
-- Funciones extractoras y auxiliares BASICAS: Series
-- ====================================


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


-- ====================================
-- Funciones principales sobre Series
-- ====================================


-- 1
-- Dado un listado de series, calcula en numero de series por genero
-- incluido en el mismo
contarNumSeriesXGenero :: [Serie] -> [(GeneroS, Int)]
contarNumSeriesXGenero series = [(genero, length(filter(\s -> getGeneroS s == genero) series)) | genero <- eliminarGenerosRepetidos (map getGeneroS series)]

--2	
-- Dada la edad y un listado de series, selecciona todas las series cuya edad
-- recomendada sea igual o superior a la dada
seriesParaMayoresDe:: Edad -> [Serie]-> [Serie]
seriesParaMayoresDe edad series = filter (\s -> getEdad s >= edad) series

-- 3
-- Dado un numero de temporadas y un listado de series, extrae los títulos de 
-- lass series que tienen a los sumo ese numero de temporadas
titulosSconPocasTemporadas:: NTemporadas -> [Serie] -> [Titulo]
titulosSconPocasTemporadas ntemporadas series = map (\s -> getTituloS s) (filter (\s -> getTemporadas s <= ntemporadas) series)

-- 4
-- Dado n el numero de series, dm la duracion maxima en minutos y un listado de 
-- series, selecciona n series del listado con duracion menor o igual a dm 
miSeleccionDeSeriesMasCortasQue:: Int -> DuracionM -> [Serie]-> [Serie]
miSeleccionDeSeriesMasCortasQue n dm series = take n (filter (\s -> getDuracionEp s <= dm) series)

-- 5
-- Dado un listado de series, determina la duración total (en minutos)
-- de todos los episodios de todas sus temporadas
totalMinutosCatalogo :: [Serie] -> DuracionM
totalMinutosCatalogo = foldr ((+) . duracionTotal) 0

-- 6
-- Dado un listado de series, identifica el genero (de series) con el más series
generoSMasProlifico:: [Serie] -> GeneroS 
generoSMasProlifico series = fst (last (qsortBy snd (contarNumSeriesXGenero series)))   

-- 7	
-- Listado de series ordenado decrecientemente por número total de episodios
rankingSeriesPorNumTotalEpisodios:: [Serie] -> [(Serie, Int)]
rankingSeriesPorNumTotalEpisodios x =  [(s, getEpisodiosTotales s) | s <- reverse (qsortBy getEpisodiosTotales x)]

-- 8 	
-- Listado de series ordenado crecientemente por duración total (en minutos), 
-- considerando todos los episodios de todas sus temporadas
rankingSeriesMasBreves:: [Serie] -> [(Serie, Int)]
rankingSeriesMasBreves x = [(s , duracionTotal s) | s <-qsortBy duracionTotal x]

-- 9
-- Dado un listado de series, identifica los generos (de serie) que NO estan 
-- representados (que faltan) con respecto al conjunto completo de generos definidos
generosSerieSinRepresentacion :: [Serie] -> [GeneroS]
generosSerieSinRepresentacion [] = getAllGeneroS
generosSerieSinRepresentacion series = filter (\g -> g `notElem` eliminarGenerosRepetidos (map (getGeneroS) series)) getAllGeneroS 


-- =============================
-- Resto de funciones auxiliares (para gestionar el catalogo de series)
-- ============================
eliminarGenerosRepetidos :: [GeneroS] -> [GeneroS]
eliminarGenerosRepetidos [] = []
eliminarGenerosRepetidos (x:xs) = x : eliminarGenerosRepetidos (filter(\g -> g /= x) xs)

getEpisodiosTotales :: Serie -> Int
getEpisodiosTotales (_, ntemporadas, episodios, _, _, _) = ntemporadas * episodios

getAllGeneroS :: [GeneroS]
getAllGeneroS = [Accion, Animacion, Comedia, Drama, Documental, SciFic, Suspense, Romance, Terror]

duracionTotal :: Serie -> DuracionM
duracionTotal x = getEpisodiosTotales x * getDuracionEp x 
