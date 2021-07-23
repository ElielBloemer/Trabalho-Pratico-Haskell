module Spec where
import PdePreludat
import Library
import Test.Hspec

personaBase :: Persona
personaBase = Persona{
  nombre="",
  edad=0,
  felicidonios=0,
  habilidades=[],
  suenios=[]
}

type SuenioPedido = Number

personaDeDosSuenios :: Persona
personaDeDosSuenios = Persona{
  nombre="",
  edad=0,
  felicidonios=0,
  habilidades=[],
  suenios=[recibirseDe "Aprobador de TPs", viaja ["en colectivo xd"]]
}

correrTests :: IO ()
correrTests = hspec $ do
  
  describe "Test del ejercicio 1a - Coeficiente de satisfacción" $ do
    
    it "Test con una persona muy feliz" $ do
      coeficienteDeSatisfaccion Persona{
        nombre="",
        edad=25,
        felicidonios=101,
        habilidades=[],
        suenios=[]
      } `shouldBe` 25 * 101

    it "Test con una persona moderadamente feliz" $ do  
      coeficienteDeSatisfaccion Persona{
        nombre="",
        edad=0,
        felicidonios=100,
        habilidades=[],
        suenios=[recibirseDe "Ing", viaja ["pepitolandia"]]
      } `shouldBe` 100 * 2

    it "Test con una persona poco feliz" $ do  
      coeficienteDeSatisfaccion Persona{
        nombre="",
        edad=0,
        felicidonios=50,
        habilidades=[],
        suenios=[]
      } `shouldBe` 25
  
    
  describe "Test del ejercicio 1b - Grado de ambición de una persona" $ do
    it "Test con una persona muy feliz" $ do
      gradoDeAmbicionDeUnaPersona Persona{
        nombre="",
        edad=0,
        felicidonios=101,
        habilidades=[],
        suenios=[recibirseDe "cartonero", recibirseDe "contador"]
      } `shouldBe` 101 * 2

    it "Test con una persona moderadamente feliz" $ do
      gradoDeAmbicionDeUnaPersona Persona{
        nombre="",
        edad=26,
        felicidonios=100,
        habilidades=[],
        suenios=[recibirseDe "cartonero", recibirseDe "contador"]
      } `shouldBe` 26 * 2
      
    it "Test con una persona poco feliz" $ do
      gradoDeAmbicionDeUnaPersona Persona{
        nombre="",
        edad=0,
        felicidonios=50,
        habilidades=[],
        suenios=[recibirseDe "cartonero"]
      } `shouldBe` 2 * 1
      

  describe "Test del ejercicio 2a - Nombre largo" $ do

    it "Test con una persona con nombre largo" $ do
       Persona {
         nombre="Evangelina",
         edad = 0,
         felicidonios = 0,
         habilidades = [],
         suenios = []
         }  `shouldNotSatisfy` nombreLargo
         

    it "Test con una persona con nombre corto" $ do
       Persona {
         nombre ="Maximiliano",
         edad = 0,
         felicidonios = 0,
         habilidades = [],
         suenios = []
         } `shouldSatisfy` nombreLargo


  describe "Test del ejercicio 2b - Persona suertuda" $ do

    it "Test con una persona NO suertuda" $ do  
       Persona {
         nombre="",
         edad = 0,
         felicidonios = 14,
         habilidades = [],
         suenios = []} 
         `shouldNotSatisfy` personaSuertuda

    it "Test con una persona suertuda" $ do  
       Persona {
         nombre="",
         edad = 0,
         felicidonios = 12,
         habilidades = [],
         suenios = []} 
         `shouldSatisfy` personaSuertuda  

  describe "Test del ejercicio 2C - Nombre lindo" $ do

    it "Test con una persona de nombre comun" $ do
      Persona{
        nombre ="Ariel",
        edad=0,
        felicidonios=0,
        habilidades = [],
        suenios = []}
        `shouldNotSatisfy` nombreLindo

    it "Test con una persona de nombre lindo" $ do
      Persona{
        nombre ="Melina",
        edad=0,
        felicidonios=0,
        habilidades = [],
        suenios = []}
        `shouldSatisfy` nombreLindo

  describe "Test del ejercicio 3  - Los sueños se cumplen" $ do
    it "Test con una persona que se recibió" $ do
      ((habilidades).(recibirseDe "Fullstack")) Persona {
        nombre="",
        edad=0,
        felicidonios=0,
        habilidades=[],
        suenios=[]
      } `shouldBe` ["Fullstack"]

      ((felicidonios).(recibirseDe "Fullstack")) Persona {
        nombre="",
        edad=0,
        felicidonios=0,
        habilidades=[],
        suenios=[]
      } `shouldBe` 9000

    it "Test con una persona que viajó" $ do
      ((felicidonios).viaja ["Madrid","Roma"]) personaBase `shouldBe` 200

      ((edad).viaja["Verona","Como","Milan"]) personaBase `shouldBe` 1

    it "Test con una persona que se enamoró" $ do
      ((felicidonios).(enamorarseDeOtraPersona Persona{
        nombre="",
        edad=0,
        felicidonios=2400,
        habilidades=[],
        suenios=[]
      })) Persona{
        nombre="",
        edad=0,
        felicidonios=530,
        habilidades=[],
        suenios=[]
      } `shouldBe` 2400+530
    
    it "Test de una persona conformista" $ do
      (nombre.queTodoSigaIgual) personaBase `shouldBe` ""
      (edad.queTodoSigaIgual) personaBase `shouldBe` 0
      (felicidonios.queTodoSigaIgual) personaBase `shouldBe` 0
      (habilidades.queTodoSigaIgual) personaBase `shouldBe` []
      (length.suenios.queTodoSigaIgual) personaBase `shouldBe` 0

    it "Test con alguien que cumplió el combo perfecto" $ do 
      (felicidonios.comboPerfecto) personaBase `shouldBe` 8300
      (edad.comboPerfecto) personaBase `shouldBe` 1
      (habilidades.comboPerfecto) personaBase `shouldBe` ["medicina"]
{-
      Casos de prueba
      - A implementar por el integrante 1, debe verificar que una vez que la fuente le cumplió el deseo...
      -se cumpla el primer sueño de una persona (y no otro, ver cómo queda la persona).
      - queden menos sueños para la persona
-}

{-A implementar por el integrante 3, debe verificar que una vez que la fuente le cumplió el deseo...
se cumplan el enésimo sueño y no otro (cómo queda la persona)
-}
  describe "Test del ejercicio 4 - Fuentes de Deseos " $ do
    it "Fuente Minimalista: Persona cumple el primer suenio" $ do
      (length.suenios.fuenteMinimalista) personaDeDosSuenios `shouldBe` ((+ (-1)).length.suenios) personaDeDosSuenios
      (felicidonios.cumpleSuenio) personaBase{suenios=[recibirseDe "recursante",recibirseDe "Recursante, pero con más letritas xd"]} `shouldBe` (felicidonios.(recibirseDe "recursante")) personaBase
    it "Fuente Copada: Persona cumple todos los suenios" $ do
      (length.suenios.fuenteCopada) personaDeDosSuenios `shouldBe` 0
      (felicidonios.fuenteCopada) personaDeDosSuenios `shouldBe` ((felicidonios.(recibirseDe "Aprobador de TPs")) personaDeDosSuenios) + ((felicidonios.(viaja ["en colectivo xd"])) personaDeDosSuenios)
    it "Fuente a Pedido: Cumple un suenio a pedido " $ do
      (felicidonios.fuenteAPedido 2) personaBase{suenios=[recibirseDe "recursante",recibirseDe "Recursante, pero con más letritas xd"]} `shouldBe` (felicidonios.(recibirseDe "Recursante, pero con más letritas xd")) personaBase
    it "Fuente Sorda: NO cumple ningun suenio " $ do 
      (nombre.fuenteSorda)personaBase `shouldBe` nombre personaBase
      (edad.fuenteSorda)personaBase `shouldBe` edad personaBase
      (felicidonios.fuenteSorda) personaBase `shouldBe` felicidonios personaBase
      (habilidades.fuenteSorda) personaBase `shouldBe` habilidades personaBase
      (length.suenios.fuenteSorda) personaBase `shouldBe` (length.suenios) personaBase

--el que más felicidonios le de a esa persona cuando lo cumpla (integrante 1)

  describe "Teste del ejercicio 5 - Fuente Ganadora" $ do 
    it "Test de fuente ganadora que suma MÁS felicidonios"$ do
      (felicidonios.(fuenteGanadora masFelicidonios [fuenteSorda,fuenteCopada,fuenteAPedido 1] personaDeDosSuenios)) personaDeDosSuenios `shouldBe ` (felicidonios.fuenteCopada) personaDeDosSuenios
    it "Test de fuente ganadora que suma MENOS felicidonios"$ do 
      (felicidonios.(fuenteGanadora menosFelicidonios [fuenteSorda,fuenteCopada,fuenteAPedido 1] personaDeDosSuenios)) personaDeDosSuenios `shouldBe` (felicidonios.fuenteSorda) personaDeDosSuenios 
    it "Test de fuente ganadora que suma SUMAS MAS HABILIDADES"$ do 
      (habilidades.(fuenteGanadora masHabilidades [fuenteSorda,fuenteCopada,fuenteAPedido 1] personaDeDosSuenios)) personaDeDosSuenios `shouldBe` (habilidades.fuenteCopada) personaDeDosSuenios

  describe " Test del ejercicio 6 - Reportes" $ do
    it "Test del suenio valioso para una persona"$ do
      (length.suenioValioso) personaBase{suenios=[recibirseDe "recursante",recibirseDe "Recursante, pero con más letritas xd"]} `shouldBe` length [recibirseDe "recursante",recibirseDe "Recursante, pero con más letritas xd"]