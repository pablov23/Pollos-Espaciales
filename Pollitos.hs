module Pollitos where

import Data.List
import Text.Show.Functions

type NombrePollo = String
type DiasVivo = Int
type PesoEnGramos = Float
type ArteMarcial = String 
type Entrenador = Pollo->Pollo

data Pollo = UnPollo {
    nombrePollo::NombrePollo,
    diasDeVida::DiasVivo,
    pesoEnGramos::PesoEnGramos,
    listaArteMarcial::[ArteMarcial],
    theChickenOne::Bool
    } deriving Show

data Planeta = UnPlaneta {
    entrenador::Pollo->Pollo,
    pollos::[Pollo]
    } deriving Show

gin = UnPollo "ginger" 10 150 [] False
roc = UnPollo "rocky" 1 1200 [] False
kod = UnPollo "kodos" 10 1500 ["karate"] False
kan = UnPollo "kang" 100 4000 ["karate","kungfu","boxeo"] False
pau = UnPollo "Paula" 2 300 ["judo"] False
pan = UnPollo "Panda" 20 100000 ["kungfu"] False
jac = UnPollo "JackieChan" 1000 800 [] False
jet = UnPollo "JetLi" 2000 600 [] False
chun = UnPollo "Chun Li" 200 10 ["kungfu","superpatada"] False
liu = UnPollo "Liu Kang" 1000 800 ["mortalk","RayoPeronizador"] False
ryu = UnPollo "Ryu" 2000 6000 ["hadoken","ninjakick"] False

data Raton = UnRaton{
    pesoRaton::Float,
    alturaRaton::Float,
    bigotesRaton::Int
    } deriving Show

mickymouse = UnRaton 20 10 80
carpincho = UnRaton 300 20 8

--Pollos 1: Engordar a un pollo una cierta cantidad de gramos
engordarPollo :: Pollo->Float->Pollo
engordarPollo pollo gramos =  pollo {pesoEnGramos = pesoEnGramos pollo + gramos} 

--Pollos 2: Saber si un pollo es mayor de edad: se dice que un pollo es mayor de edad si tiene más de 6 meses de vida
esMayor :: Pollo -> Bool 
esMayor pollo = diasDeVida pollo > 6*30

--Pollos 3: Saber si el último atributo de un pollo es vacío. ¡Ojo! Cuando sepamos qué es esa última lista, el nombre debe cambiar
tieneArtesMarcialesPollo :: Pollo -> Bool
tieneArtesMarcialesPollo pollo = length (listaArteMarcial pollo) == 0

--Pollos 4: Cruzar un conjunto de pollos: dada una lista de pollos, obtener un pollo que es la combinación de todos los anteriores. 
--Inventar una lógica para esto, usando al menos una expresión lambda.
combinacionPollos :: [Pollo]-> Pollo
combinacionPollos pollo = 
    UnPollo (concat (map(\xs->nombrePollo xs ++" ") pollo ))  (sumaParam diasDeVida pollo) (sumaParam pesoEnGramos pollo) (unionArtes pollo) False

sumaParam::Num a=>(Pollo->a)->[Pollo]->a
sumaParam param pollos= foldl1 (+) (map param pollos)

unionArtes::[Pollo]->[ArteMarcial]
unionArtes pollos = foldl1 (++) (map listaArteMarcial pollos)   

--Pollos Ninjas
--Pollos Ninja 1: Modelar los siguientes entrenadores:

--arguiniano Engorda 100 gramos al pollo que entrena.
arguiniano::Pollo->Pollo
arguiniano pollo = engordarPollo pollo 100

--miyagi Si no sabe, le enseña karate al pollo.
miyagui::Pollo->Pollo
miyagui pollo | elem "judo" (listaArteMarcial pollo) = pollo
             | otherwise = pollo {listaArteMarcial ="judo":(listaArteMarcial pollo)}

--marcelito Hace que el pollo se olvide todas las artes marciales y después lo manda a aprender de Miyagi
marcelito::Pollo->Pollo
marcelito pollo = miyagui pollo {listaArteMarcial = []}

--brujaTapita Alimenta al pollo dándole de comer un ratón. El alimento que provee el ratón se calcula como 
--su peso por su altura menos la cantidad de bigotes 
brujaTapita::Raton->Pollo->Pollo
brujaTapita raton pollo = engordarPollo pollo (alimentoRaton raton)

alimentoRaton::Raton->Float
alimentoRaton raton = (pesoRaton raton * alturaRaton raton) - fromIntegral(bigotesRaton raton)

--marioBros: Toma al pollo y le agrega al nombre la frase “super mario ”. Le enseña a saltar y una nueva arte marcial que
--también se indica por parámetro, si es que el pollo no sabe hacerlo. Por ejemplo, marioBros “judo” le podría enseñar a rocky judo.
marioBros::ArteMarcial->Pollo->Pollo
marioBros arte (UnPollo nombre dias peso lista chone)= 
    UnPollo (nombre++" supermario") dias peso (nub(agregarArte (lista) ["hola",arte] )) False
   -- (agregarDosArtes [arte,"saltar"] lista) False

agregarArte artePollo extra = concat [artePollo,extra]


--Pollos Ninja 2: Dados dos entrenadores y un pollo, averiguar cuál de los dos entrenadores lo entrena mejor, es decir, 
--lo deja con más artes marciales aprendidas.


entrenaMejor entrenador1 entrenador2 pollo | cantidadArtesEntrenado entrenador1 pollo> cantidadArtesEntrenado entrenador2 pollo = entrenador1
                                           | otherwise = entrenador2

cantidadArtesEntrenado entrenador pollo = (length(listaArteMarcial (entrenador pollo))) 


--Pollos Ninjas Espaciales
--Pero estos pollos también son espaciales, ya que viven en muchos planetas donde son entrenados por distintos expertos en artes marciales.
--De los planetas se conoce su entrenador asignado y los pollos que habitan allí. Para esto tenemos:

marte = UnPlaneta arguiniano [kod,roc]
venus = UnPlaneta miyagui [pau,gin]
urano = UnPlaneta marcelito [kan,pan,jac,jet]
neptuno = UnPlaneta (marioBros "karate") [chun]
pluton = UnPlaneta (brujaTapita carpincho) [liu,ryu]


--elMejorPollo un pollo es el mejor pollo de un planeta si es el que más artes marciales sabe

cantArtes::Pollo->Int
cantArtes pollo = length (listaArteMarcial pollo)

elMejorPollo::Planeta->Pollo
elMejorPollo planeta = foldl1 (maxSegun cantArtes) (pollos planeta)

maxSegun::Ord b => (a->b)->a->a->a
maxSegun param x y 
   | param x > param y = x
   | otherwise = y

--esDebil Un planeta es débil si ninguno de sus pollos adultos sabe más de 2 artes marciales o si al menos dos de sus pollos 
--no saben ningún arte marcial.

esDebil::Planeta->Bool 
esDebil planeta = (noSabenArtes (pollos planeta) > 2) || not (minArtesPolloAdulto 2 planeta)

noSabenArtes::[Pollo]->Int
noSabenArtes lpollos = length (filter(==0) (cantidadArtesPollos lpollos))

minArtesPolloAdulto::Int->Planeta->Bool
minArtesPolloAdulto num planeta=  (alMenos num) (cantidadArtesPollos (pollosAdultos (pollos planeta)))

cantidadArtesPollos::[Pollo]->[Int]
cantidadArtesPollos lpollos = map length ((map listaArteMarcial) lpollos)

--PollosAdultos
pollosAdultos::[Pollo]->[Pollo]
pollosAdultos = filter esMayor

alMenos::Int->[Int]->Bool
alMenos num cant = any (<=num) cant

--entrenar: Recibe un planeta y hace que su entrenador haga lo correspondiente 
--con todos los pollos del planeta. Devuelve el planeta con todos sus pollos entrenados.
entrenar::Planeta->Planeta
entrenar planeta = entrenaPlaneta planeta planeta 

--Funcion auxiliar, entrena pollos de planeta1 con entrenador de planeta2
entrenaPlaneta::Planeta->Planeta->Planeta
entrenaPlaneta planeta2 planeta1= planeta1 {pollos = map (entrenador planeta2) (pollos planeta1)} 

--entrenamientoKaio dado dos planetas los pollos del primer planeta son entrenados por su entrenador asignado y después por 
--el entrenador del segundo planeta, devolviendo el planeta con los pollos entrenados.
entrenamientoKaio::Planeta->Planeta->Planeta
entrenamientoKaio planeta1 planeta2 = entrenaPlaneta planeta2 (entrenar planeta1)


--hacerViajeEspiritual hace que un pollo se transforme en The Chicken One (el Pollo Elegido). Hacer un viaje espiritual es entrenar a 
--un pollo con todos los entrenadores de la lista que se recibe como argumento. Realizar los cambios necesarios para que las funciones 
--de la primera parte sigan funcionando.
convertirTheChickenOne :: Pollo -> Pollo
convertirTheChickenOne pollo = pollo {theChickenOne=True}

hacerViajeEspiritual::[Pollo->Pollo]->Pollo->Pollo
hacerViajeEspiritual entrenadores pollo = foldl (\a b ->b a) (convertirTheChickenOne (pollo)) entrenadores 

--planetaDebilEntrenado saber si un planeta queda débil incluso después de hacer que todos sus pollos hagan un viaje espiritual 
--con ciertos entrenadores

planetaDebilEntrenado planeta entrenadores = esDebil UnPlaneta{ entrenador = entrenador planeta, pollos = (map (hacerViajeEspiritual entrenadores) (pollos planeta))}


--Pollos Ninjas Espaciales Mutantes
--Muchos de nuestros pollos tienen además características especiales: son pollos mutantes.

--chickenNorris Es un pollo mutante que pesa 100 kilos, tiene 9000000 días y sabe TODOS los niveles de karate. 
--"karate1", "karate2", "karate3", etc. (Sabe infinitas artes marciales) Mostrar ejemplos de invocación para entrenar
-- a chickenNorris con diferentes entrenadores. ¿Con cuáles se lo puede entrenar y con cuáles no? Justificar.
-- Mostrar ejemplos de invocación y respuesta con diferentes entrenadores.

infinitosKarate=["karate"++ show num| num<- [1..]]
chi = UnPollo "ChickenNorris" 900000 100000 infinitosKarate


--arguiniano chickenNorris funciona bien
--miyagui chickenNorris esta funciona mal porque usa elem, y recorre toda la lista de artes marciales y no terminara nunca
--marcelito chickenNorris podriamos decir que el pollo perderia las calificaciones para ser chickenNorris al no saber ningun arte marcial para solo saber judo
--brujaTapita chickenNorris como solo engorda no afecta en la lista artes marciales, asi que logicamente andaria bien
--marioBros chickenNorris,como utiliza elem para no repetir artes marciales  


--Graduar al mejor pollo de un planeta, mutándolo a entrenador: hacer una función que dado un planeta, permita obtener un nuevo entrenador, en base al 
--pollo que más artes marciales sabe de ese planeta, con un comportamiento que consista es enseñar todas sus artes marciales.

graduarPollo::Planeta->Pollo->Pollo
graduarPollo planeta (UnPollo nom dias peso lista elegido) = 
    UnPollo nom dias peso  (nub(agregarArte lista ((listaArteMarcial (elMejorPollo planeta))))) elegido
    --(lista++(listaArteMarcial (elMejorPollo planeta))) elegido

--Definir a Marceñano como la mutación de dos entrenadores: marcelito y arguiñano en un nuevo entrenador que combina sucesivamente                                                                                                                                                                                                                           el entrenamiento de ambo

marcenano::Pollo->Pollo
marcenano = marcenano.marcelito.arguiniano
© 2020 GitHub, Inc.
