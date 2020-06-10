module Lib where
import Text.Show.Functions

laVerdad = True


{- 1) 2)
Modelar el festival, las bandas y los géneros de manera de poder definir la función tocar, 
que hace que la banda toque y altere al público del festival de acuerdo a su género. (Definir también esa función)  -}

--Representar las bandas, géneros y el festival dados como ejemplo. Agregar una banda que sea trash metal.

{-
    Festivales
De cada festival se sabe el lugar donde se realiza, la cantidad y estado de ánimo inicial del público
y las bandas que tocan en él, ordenadas cronológicamente.
Por ejemplo, el festival Hullabalooza, en el que tocan Miranda, Los Redondos, Metallica y Soda, 
tiene un público de 20000 personas con ánimo inicial “indiferente”. -}

type Animo = [String]
type Descripcion = String
type Decibeles = Float
type Genero = Festival->Festival

data Festival = UnFestival {
    lugar :: String,
    publico :: Float,
    animoInicialPublico :: Animo,
    bandas :: [Banda]
}deriving (Show)


hullabalooza :: Festival
hullabalooza = UnFestival {
    lugar = "Argentina",
    publico = 20000,
    animoInicialPublico = ["indiferente"],
    bandas = [miranda, losRedondos,metallica,soda, megadeth]
}

{-
    Las bandas
Las bandas tienen un conjunto de descripciones realizadas por los críticos y los decibeles a los que suelen tocar. 
Además, cada vez que tocan, las bandas movilizan al público del festival de acuerdo al género al que pertenezcan. 
-}


data Banda = UnaBanda {
    nombreBanda :: String,
    descripciones :: [Descripcion],
    decibeles :: Decibeles,
    genero :: Genero
}deriving (Show)


--Algunas bandas son:
-- Los redondos, que está descripta como “legendaria” y “pogosa”, toca a 45 decibeles y se considera de rock nacional.
losRedondos :: Banda
losRedondos = UnaBanda "Los Redondos" ["legendaria", "pogosa"] 45 rockNacional

-- Soda está descripta como "irrepetible", toca a 40 decibeles y también es de rock nacional.
soda :: Banda
soda = UnaBanda "Soda" ["irrepetible"] 40 rockNacional
-- Miranda es una banda de pop que toca a 60 decibeles y los críticos la describieron como "insípida", "incolora" e "inodora".
miranda :: Banda 
miranda = UnaBanda "Miranda" ["insipida", "incolora", "inodora"] 60 pop
-- Metallica está descripta como “legendaria” y “vendida” y toca a 60 decibeles. Es una de las mayores exponentes del heavy metal.
metallica :: Banda 
metallica = UnaBanda "Metallica" ["legendaria", "vendida"] 60 heavyMetal

--Como se observa con el rock nacional, puede haber muchas bandas de cualquiera de los géneros.

megadeth :: Banda
megadeth = UnaBanda "Megadeth" ["unicos", "pogosa"] 80 trashMetal

{-
    Los géneros
Las bandas, cada vez que tocan, movilizan al público del festival de acuerdo al género al que pertenezcan. Por ejemplo:
Otro género que suele estar presente en los festivales es el metal, que tiene variantes que los especialistas denominan subgéneros
Existen otros subgéneros del metal que también alteran al público de igual manera, pero agregando otros calificativos al estado de ánimo. 
-}

-- rock nacional: hace que el público aumente en 100 personas
rockNacional :: Genero
rockNacional = modificarPublico (+) 100

modificarPublico :: (Float -> Float -> Float) -> Float -> Festival -> Festival
modificarPublico f cantidad festival = festival{publico = f (publico festival) cantidad}

{- pop: generalmente no afectan al público, sólo en caso que el estado de ánimo sea "indiferente", 
duplican la cantidad y dejan el público "eufórico". -}
pop :: Genero
pop festival | head (animoInicialPublico festival) == "indiferente" =  ((modificarPublico (+) (publico festival)).modificarEstadoAnimo ["euforico"]) festival
             | otherwise = festival

modificarEstadoAnimo :: Animo-> Festival -> Festival
modificarEstadoAnimo animo festival = festival {animoInicialPublico = animo ++ drop 1 (animoInicialPublico festival) }


-- Heavy metal: hace que el público aumente 1% cada vez que toca, y a su estado de ánimo se le agregue “pesado” al final.
heavyMetal :: Genero
heavyMetal = (agregarEstadoAnimo ["pesado"]).(aumentarPublicoPorcentual 1)

agregarEstadoAnimo :: Animo -> Festival -> Festival
agregarEstadoAnimo animo festival = festival {animoInicialPublico = animoInicialPublico festival ++ animo}

-- Trash metal: Hace que el público aumente 1% al tocar y se le agrega "basura" al final del estado de ánimo. 
trashMetal :: Genero
trashMetal = (agregarEstadoAnimo ["basura"]).(aumentarPublicoPorcentual 1)

aumentarPublicoPorcentual :: Float -> Festival -> Festival
aumentarPublicoPorcentual n festival = modificarPublico (+) (publico festival * n/100) festival

--función tocar : Hace que la banda toque y altere al público del festival de acuerdo a su género. (Definir también esa función)

tocar :: Banda->Festival->Festival
tocar = alterarPublico

alterarPublico :: Banda -> Festival -> Festival
alterarPublico banda = genero banda

{- 3)
Agregar la siguiente banda:
The strokes, que toca a 45 decibeles y está descripta como 
“suicidio asistido”, “emocional” y “linda”. No pertenece a ninguno de los géneros conocidos, 
pero hay expertos que afirman que es una fusión musical entre el pop y el heavy metal.  
-}

theStrokes :: Banda
theStrokes = UnaBanda "The Strokes" ["suicidio asistido","emocional","linda"] 45 (fusionMusical pop heavyMetal)

fusionMusical :: Genero->Genero->Genero
fusionMusical genero1 genero2 = genero1.genero2

{- 4)
Definir la función suceder, que hace que suceda un festival.
El resultado debe ser el mismo festival pero con el público en su situación final, luego de haber tocado todas las bandas.  -}

suceder :: Festival->Festival
suceder festival = foldr tocar festival (bandas festival)

{-5)
Definir las funciones que permitan clasificar a las bandas. 
Se conocen ciertos criterios de clasificación de bandas, de los cuales depende su popularidad. Por ejemplo: 
-}
type Clasificacion = Banda -> Bool
--Vendida: Debe tener tres o más descripciones o bien una descripción que sea “vendida”. 
vendida :: Clasificacion
vendida banda = ((>=3).length.descripciones) banda || descriptaComo "vendida" banda

--Acústica: Es la que toca a más de 55 decibeles. 
acustica :: Clasificacion
acustica = tocaAMasDecibeles 55

--Legendaria. Debe estar descripta como “legendaria” y tocar a más de 40 decibeles.
legendaria :: Clasificacion
legendaria banda = tocaAMasDecibeles 40 banda && descriptaComo "legendaria" banda

tocaAMasDecibeles :: Float -> Banda -> Bool
tocaAMasDecibeles decibel = (>decibel).decibeles

descriptaComo :: String->Banda->Bool
descriptaComo descripcion banda= elem descripcion (descripciones banda) 

--Una banda puede clasificarse de más de una manera a la vez o ninguna.

criteriosDeClasificacion :: [Clasificacion]
criteriosDeClasificacion = [vendida, acustica, legendaria]

{- 6)
Definir la función popularidad, que, dada una lista de clasificaciones, 
permite conocer la popularidad de una banda. 
La popularidad se calcula así: 100 puntos por cada clasificación a la que la banda aplique.

Ejemplo:
popularidad metallica [vendida, acustica, legendaria]
300
popularidad redondos [vendida, acustica, legendaria]
100
-}

popularidad :: Banda -> [Clasificacion]->Int
popularidad banda clasificaciones = (*100) . length $ filter (evaluarClasificacion banda) clasificaciones

-- filter ($ banda) clasificaciones
evaluarClasificacion :: Banda->Clasificacion->Bool
evaluarClasificacion banda clasificacion = clasificacion banda 

{- 7)
Definir la función buenFest, que dado un festival y un conjunto de clasificaciones posibles dice si es un buen fest. 
Esto sucede cuando cronológicamente cada banda es más popular que la anterior,
y además la popularidad total (la popularidad acumulada de sus bandas) supera los 1000 puntos.
-}

buenFest :: Festival -> [Clasificacion] -> Bool
buenFest festival clasificaciones = popularidadTotal clasificaciones (bandas festival) >1000 && popularidadCreciente (bandas festival) clasificaciones

popularidadTotal :: [Clasificacion]->[Banda]->Int
popularidadTotal clasificaciones = sum . map (`popularidad` clasificaciones)

popularidadCreciente :: [Banda]->[Clasificacion]->Bool
popularidadCreciente [banda] _ = True
popularidadCreciente (banda1:banda2:bandas) clasificaciones = (popularidad banda1 clasificaciones < popularidad banda2 clasificaciones) && popularidadCreciente (banda2:bandas) clasificaciones