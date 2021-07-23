module Library where
import PdePreludat
 
type Nombre = String
type Edad = Number
type Felicidonios = Number
type Habilidades = [String]
type Suenio = Persona -> Persona
type Fuente = Persona -> Persona

data Persona = Persona {
  nombre       :: Nombre,
  edad         :: Edad,
  habilidades  :: Habilidades,
  felicidonios :: Felicidonios,
  suenios      :: [Suenio]
}


instance Show Persona where
 show  persona = "Nombre: " ++ nombre  persona ++ "\n" ++ "Edad: " ++ (show.edad  ) persona  ++ "\n" ++ "Habilidades: " ++ (show.habilidades) persona ++ "\n" ++ "Felicidonios: " ++ (show.felicidonios) persona

-- instance Show Persona where
--   show  persona = "Nombre: " ++ nombre persona ++ "\n" ++ "Edad: " ++ (show.edad) persona ++ "\n" ++ "Suenios: " ++ (show.suenios) persona ++ "\n" ++ "Habilidades: " ++ (show.habilidades) persona



fede :: Persona
fede = Persona {
  nombre = "Fede",
  edad = 24,
  felicidonios = 50,
  habilidades = ["Cantar"," Dibujar", " Programar"],
  suenios = [recibirseDe "Ingienieria",recibirseDe "Pepito",viaja ["Montevideo"],queTodoSigaIgual]
}


muyFeliz :: Persona -> Bool
muyFeliz = (>100) . felicidonios

moderadamenteFeliz :: Persona -> Bool
moderadamenteFeliz persona = felicidonios persona <= 100 && felicidonios persona > 50

-- Punto 1
cantidadDeSuenios :: Persona -> Number
cantidadDeSuenios  =  length . suenios

coeficienteDeSatisfaccion :: Persona -> Number
coeficienteDeSatisfaccion persona 
  | muyFeliz persona                = felicidonios persona * edad persona 
  | moderadamenteFeliz persona      = cantidadDeSuenios persona * felicidonios persona 
  | otherwise                       = felicidonios persona `div` 2
  
gradoDeAmbicionDeUnaPersona :: Persona -> Number
gradoDeAmbicionDeUnaPersona persona 
 | muyFeliz persona                  =  felicidonios persona * cantidadDeSuenios persona
 | moderadamenteFeliz persona        =  edad persona * cantidadDeSuenios persona 
 | otherwise                         =  ((*2).cantidadDeSuenios) persona


-- Punto 2
nombreLargo :: Persona -> Bool
nombreLargo  = ( >10) . length . nombre  

personaSuertuda :: Persona -> Bool
personaSuertuda  =  even . ( *3) . coeficienteDeSatisfaccion

nombreLindo :: Persona -> Bool
nombreLindo  =  ( == 'a').last.nombre


-- Punto 3
agregarHabilidad :: String -> Suenio
agregarHabilidad habilidad persona = persona{
  habilidades = ((++ [habilidad]). habilidades) persona 
}

sumarFelicidonios :: Number -> Suenio
sumarFelicidonios felicidoniosASumar persona = persona{
  felicidonios = felicidonios persona + felicidoniosASumar
}

sumarEdad :: Suenio
sumarEdad persona = persona{
  edad = ((+1). edad )persona
}



recibirseDe :: String -> Suenio
recibirseDe carrera =  agregarHabilidad carrera . sumarFelicidonios (((*1000) . length) carrera) 

viaja :: [String] -> Suenio
viaja ciudades = sumarEdad . sumarFelicidonios (((*100) . length) ciudades)

enamorarseDeOtraPersona :: Persona -> Suenio
enamorarseDeOtraPersona personaQueAma = sumarFelicidonios (felicidonios personaQueAma)

queTodoSigaIgual :: Suenio
queTodoSigaIgual = id

comboPerfecto :: Suenio
comboPerfecto = (sumarFelicidonios 100 ) . (viaja ["Berazategui","Paris"]) . (recibirseDe "medicina")

--Punto 4
{-Punto a (integrante 1): Fuente minimalista
La fuente minimalista le cumple el primer sueño a la persona, y lo quita de la lista de sueños de esa persona.
-}

cumpleSuenio :: Suenio
cumpleSuenio persona = ((head.suenios) persona) persona

quitarUltimoSuenio :: Suenio
quitarUltimoSuenio  persona = persona {
  suenios = ((drop 1). suenios) persona
}

fuenteMinimalista :: Suenio
fuenteMinimalista persona = (quitarUltimoSuenio.cumpleSuenio) persona

{-Punto b (integrante 2): Fuente copada
La fuente copada le cumple todos los sueños a la persona. La persona debe quedar sin sueños.
-}

fuenteCopada :: Suenio
fuenteCopada persona = foldr($ fuenteMinimalista) persona (suenios persona)
{-fuenteCopada persona 
  | ((==0).length.suenios) persona  = persona
  | otherwise                       = (fuenteCopada.fuenteMinimalista) persona-}

--fuenteCopada persona = foldr(fuenteCopada.fuenteMinimalista) persona (suenios persona)
{-Punto c (integrante 3): Fuente a pedido
La fuente a pedido le cumple el enésimo sueǹo a una persona, pero no lo quita de la lista de sueños.
-}

fuenteAPedido :: Number -> Suenio
fuenteAPedido suenioPedido persona = (((!! (suenioPedido-1)).suenios) persona) persona

{-Punto d) (todos los integrantes)
Modelar la fuente sorda: como no entiende bien qué sueño tiene que cumplir no le cumple ninguno. Incluir el o los casos de prueba que sean necesarios.
-}
fuenteSorda :: Suenio
fuenteSorda persona = persona

{-Punto 5
Dada una lista de fuentes y una persona, saber cuál es la fuente "ganadora" en base a un criterio.
Por ejemplo:
el que más felicidonios le de a esa persona cuando lo cumpla (integrante 1)
el que menos felicidonios le de a esa persona cuando lo cumpla (integrante 2)
el que más habilidades le deje a esa persona cuando lo cumpla (integrante 3)

Cada integrante debe contar cómo invocar a esa función desde la consola para resolver ese requerimiento.
-}

 --tiene Persona y Fuente

--en el punto 5 no tienen que hacer 3 funciones. Tienen que hacer 1 y pasarle el criterio como parámetro.

type Criterio = Persona -> Persona -> Bool

masFelicidonios :: Criterio
masFelicidonios persona1 persona2 =  (felicidonios persona1) >= (felicidonios persona2)

menosFelicidonios :: Criterio
menosFelicidonios persona1 persona2 = (felicidonios persona1) <= (felicidonios persona2)

masHabilidades :: Criterio
masHabilidades persona1 persona2 = ((length.habilidades) persona1) >= ((length.habilidades) persona2)

fuenteGanadora :: Criterio -> [Fuente] -> Persona -> Fuente
fuenteGanadora criterio [] persona = error "Lista de fuentes vacía"
fuenteGanadora criterio [fuente] persona = fuente
fuenteGanadora criterio (primerFuente:segundaFuente:restoDeFuentes) persona
  | criterio (primerFuente persona) (segundaFuente persona) = fuenteGanadora criterio (primerFuente:restoDeFuentes) persona
  | otherwise = fuenteGanadora criterio (segundaFuente: restoDeFuentes) persona




--Ejercicio 6

--Saber qué sueños son valiosos para una persona, son aquellos que al cumplirlos la persona queda con más de 100 felicidonios
suenioValioso :: Persona -> [Suenio]
suenioValioso persona = (filter ((>100).felicidonios.($ persona)) .suenios) persona

--Saber si algún sueño de una persona es raro, que es el que lo deja  con la misma cantidad de felicidonios tras cumplirlo
suenioRaro :: Persona -> Bool
suenioRaro persona = (any (((==).felicidonios)persona).map(felicidonios.($ persona)).suenios) persona

--Dada una lista de personas, poder conocer la felicidad total de ese
totalFelicidoniosDelGrupo :: [Persona] -> Number
totalFelicidoniosDelGrupo  = foldr ((+).felicidonios.fuenteCopada) 0 




--Ejercicio 7
soniador :: Persona
soniador = Persona {
  nombre = "nube",
  edad = 58,
  felicidonios = 50,
  habilidades = ["Recursar","Memes", "Stickers de wpp"],
  suenios = repeat queTodoSigaIgual
}
--Fuente sorda satisface a la persona soniador