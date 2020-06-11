import Test.Hspec
import Taller

auto1 :: Auto
auto1 = Auto "AB808RD" [1,1,1,1] 1000.10 10.00 (10, 10, 2010)

auto1b :: Auto
auto1b = Auto "AB808RD" [0,0,0,0] 1000.10 10.00 (10, 10, 2010)

auto1c :: Auto
auto1c = Auto "AB808RD" [0.0,0.0,0.0,0.0] 1000.1 90.0 (1,6,2010)

auto2 :: Auto
auto2 = Auto "EJS363" [0.2,0,0,0] 5000.10 10.00 (10, 10, 2020)

auto2b :: Auto
auto2b = Auto "EJS363" [0.2,0,0,0] 2000.00 10.00 (10, 10, 2020)

auto3 :: Auto 
auto3 = Auto "EJS364" [0,0,0,0] 5000.20 10.00 (10, 10, 2000)

auto3b :: Auto
auto3b = Auto "EJS364" [0,0,0,0] 2000.00 10.00 (10, 10, 2000)

auto4 :: Auto
auto4 = Auto "ABC123" [0,0,0,0] 5000.20 10.00 (10, 10, 2000)

auto5 :: Auto
auto5 = Auto "MER702" [2,5,3,3] 3000.20 50.00 (10, 10, 2018)

auto5b :: Auto
auto5b = Auto "MER702" [0,0,3,3] 3000.20 50.00 (10, 10, 2018)

auto5c :: Auto
auto5c = Auto "MER702" [0,0,3,3] 3000.20 90 (10, 10, 2018)

auto6 :: Auto
auto6 = Auto "ABC111" [0.1,0.2,1,2] 1000.00 10 (01, 10, 2020)

auto7 :: Auto
auto7 = Auto "ABA222" [0.1,0.2,2,3] 1000.00 10 (10,10,2010)

auto7b :: Auto
auto7b = Auto "ABA222" [0,0,0,0] 1000 90 (10,10,2020)

listaMecanicos1 :: [Mecanico]
listaMecanicos1 = [personalBravo, personalCharly, personalLima, personalZulu]

listaMecanicos2 :: [Mecanico]
listaMecanicos2 = [personalAlfa,personalTango]

listaMecanicos3 :: [Mecanico]
listaMecanicos3 = [personalAlfa, personalBravo, personalZulu]

 

main :: IO()
main = hspec $ do
   describe "Taller mecanico" $ do
      it "anio de una fecha" $ do
         anio (1,1,2020) `shouldBe` 2020
   describe "Costo de reparacion de un auto" $ do
      it "Un auto con patente vieja costara $12500 arreglarlo " $ do
         costoReparacion auto1 `shouldBe` 12500
      it "Un auto con patente nueva y que esta entre DJ y NB costara $20000 arreglarlo " $ do
         costoReparacion auto2 `shouldBe` 20000
      it "Un auto con patente nueva, que está entre DJ y NB, y termina en 4 costara $18000 arreglarlo " $ do
         costoReparacion auto3 `shouldBe` 18000
      it "Un auto con patente nueva y que no está entre DJ y NB costara $15000 arreglarlo " $ do
         costoReparacion auto4 `shouldBe` 15000
   describe "Auto peligroso" $ do
      it "Un auto con la primera llanta poca desgastada no es peligroso" $ do
         auto1 `shouldSatisfy` esPeligroso
      it "Un auto con la primera llanta muy desgastada es peligroso" $ do
         auto2 `shouldNotSatisfy` esPeligroso
   describe "Auto necesita revision" $ do
      it "El auto1 necesita revision porque no se revisa desde antes de 2015" $ do
         auto1 `shouldSatisfy` necesitaRevision   
      it "El auto2 no necesita revision porque se revisó después de 2015 " $ do
         auto2 `shouldNotSatisfy` necesitaRevision   
   describe "Personal tecnico encargado de las reparaciones" $ do
      it "Alfa deja como esta a un auto con rpm menor que 2000" $ do
         personalAlfa auto1 `shouldBe` auto1
      it "Alfa regula a 2000 vueltas si el auto regula a mas de 2000" $ do
         personalAlfa auto2 `shouldBe` auto2b
      it "Bravo deja las llantas sin desgaste" $ do
          (personalBravo auto1) `shouldBe` auto1b
      it "Charly deja las llantas sin desgaste y deja como esta a un auto con rpm menor que 2000" $ do
          (personalCharly auto1) `shouldBe` auto1b
      it "Charly deja las llantas sin desgaste y regula a 2000 vueltas si el auto regula a mas de 2000" $ do
          (personalCharly auto3) `shouldBe` auto3b
      it "Tango deja los autos como llegan" $ do
         (personalTango auto5) `shouldBe` auto5
      it "Lima deja las dos primeras llantas sin desgaste y las otras dos como estaban" $ do
         (personalLima auto5) `shouldBe` auto5b
      it "Zulu deja las dos primeras llantas sin desgaste y la temperatura a 90" $ do
         (personalZulu auto5) `shouldBe` auto5c
   describe "Ordenamiento TOC de autos" $ do
      it "Una lista con dos autos donde el primero tiene una cantidad de desgaste impar y el segundo par está ordenada" $ do
         [auto6, auto1] `shouldSatisfy` estanOrdenados 
      it "Una lista con dos autos donde ambos tienen una cantidad de desgaste impar no está ordenada" $ do
         [auto6, auto7] `shouldNotSatisfy` estanOrdenados 
      it "Una lista con dos autos donde ambos tienen una cantidad de desgaste par no está ordenada" $ do
         [auto1, auto2] `shouldNotSatisfy` estanOrdenados 
      it "Una lista con un solo auto que tiene una cantidad de desgaste impar está ordenada" $ do
         [auto6] `shouldSatisfy` estanOrdenados 
      it "Una lista con un solo auto que tiene una cantidad de desgaste par no está ordenada" $ do
         [auto1] `shouldNotSatisfy` estanOrdenados 
   describe "Orden de reparación" $ do
      it "Un auto que pasa por la lista de mecanicos y se le actualiza su fecha" $ do
       ordenDeReparacion auto7 (10,10,2020) `shouldBe` auto7b
   describe "Técnicos que dejan el auto en condiciones" $ do
      it "Todos los mecanicos que dejan el auto sin ser peligroso dejan el auto en condiciones" $ do 
         cantTecnicosLoDejanEnCond auto1 listaMecanicos1 `shouldBe` 4
      it "Ningun mecanico que deje el auto peligroso deja el auto en condiciones" $ do
        cantTecnicosLoDejanEnCond auto1 listaMecanicos2 `shouldBe` 0
      it "Los mecanicos de la lista que dejan el auto sin ser peligroso dejan el auto en condiciones" $ do
        cantTecnicosLoDejanEnCond auto1 listaMecanicos3 `shouldBe` 2
    describe "Precio total de la reparación de los autos"
      it "El costo total sumado de los autos de la lista es 37500"
         sumaDeCostosDeAutosFiltrados listaAutos1 `shouldBe` 37500
      it "El costo total sumado de los autos de la lista es 0"
         sumaDeCostosDeAutosFiltrados listaAutos2 `shouldBe` 0
      it "El costo total sumado de los autos de la lista es 48000"
         sumaDeCostosDeAutosFiltrados listaAutos3 `shouldBe` 48000
