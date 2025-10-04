-------------------------------------------------------------------------------
--  PRACTICA: Gestion de Catalogo de Contenido Digital     
--  PF  2025-2026

--  Num. del equipo registrado en la egela: Eq_
-- Apellidos del primer integrante:
-- Apellidos del segundo integrante:  
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
contarNumSeriesXGenero series = [(x, length(filter(\s -> getGeneroS s == x) series)) | x <- eliminarGenerosRepetidos (map (\x -> getGeneroS x) series)] 

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
{-
-- 5
-- Dado un listado de series, determina la duración total (en minutos)
-- de todos los episodios de todas sus temporadas
totalMinutosCatalogo:: [Serie] -> DuracionM

-- 6
-- Dado un listado de series, identifica el genero (de series) con el más series
generoSMasProlifico:: [Serie] -> GeneroS 

-- 7	
-- Listado de series ordenado decrecientemente por número total de episodios
rankingSeriesPorNumTotalEpisodios:: [Serie] -> [(GeneroS, Int)]

-- 8 	
-- Listado de series ordenado crecientemente por duración total (en minutos), 
-- considerando todos los episodios de todas sus temporadas
rankingSeriesMasBreves:: [Serie]→[(Serie, Int)]

-- 9
-- Dado un listado de series, identifica los generos (de serie) que NO estan 
-- representados (que faltan) con respecto al conjunto completo de generos definidos
generosSerieSinRepresentacion :: [Serie]→[GeneroS]
-}


-- =============================
-- Resto de funciones auxiliares (para gestionar el catalogo de series)
-- ============================
eliminarGenerosRepetidos :: [GeneroS] -> [GeneroS]
eliminarGenerosRepetidos [] = []
eliminarGenerosRepetidos (x:xs) = x : eliminarGenerosRepetidos (filter(\g -> g /= x) xs)


-- ======================================
-- Catalogos/Listados de ejemplos: Datos de prueba de series
-- ======================================
{-
misSeries::[Serie]
misSeries = [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10]
-}
s1, s2, s3, s4, s5, s6, s7, s8, s9, s10 :: Serie
s1 = ("Breaking Bad", 5, 13, 47, Drama, 18)
s2 = ("Friends", 10, 24, 22, Comedia, 12)
s3 = ("Game of Thrones", 8, 10, 57, Drama, 18)
s4 = ("The Simpsons", 34, 22, 22, Animacion, 10)
s5 = ("Stranger Things", 4, 9, 50, SciFic, 16)
s6 = ("The Office (US)", 9, 22, 22, Comedia, 12)
s7 = ("Narcos", 3, 10, 49, Suspense, 18)
s8 = ("Planet Earth", 2, 11, 50, Documental, 7)
s9 = ("The Big Bang Theory", 12, 22, 22, Comedia, 12)
s10 = ("The Walking Dead", 11, 16, 45, Terror, 18)

misSeries :: [Serie]
misSeries = [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10]
