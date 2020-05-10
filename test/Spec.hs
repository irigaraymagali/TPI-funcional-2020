import Test.Hspec
import Taller

auto1 :: Auto
auto1 = Auto "AB808RD" [1,1,1,1] 1000 10 2010 

auto1b :: Auto
auto1b = Auto "AB808RD" [0,0,0,0] 1000 10 2000

auto2 :: Auto
auto2 = Auto "EJS363" [0.2,0,0,] 5000 10 2020

auto2b :: Auto
auto2b = Auto "EJS363" [0.2,0,0,0] 2000 10 2020

auto3 :: Auto 
auto3 = Auto "EJS364" [0,0,0,0] 5000 10 2000

auto3b :: Auto
auto3b = Auto "EJS364" [0,0,0,0] 2000 10 2000

auto4 :: Auto
auto4 = Auto "ABC123" [0,0,0,0] 5000 10 2000

auto5 :: Auto
auto5 = "MER702" [2,5,3,3] 3000 50 2018

auto5b :: Auto
auto5b :: "MER702" [0,0,3,3] 3000 50 2018

auto5c :: Auto
auto5c :: "MER702" [0,0,3,3] 3000 90 2018

main :: IO()
main = hspec $ do
   describe "Taller mecanico" $ do
      it "anio de una fecha" $ do
         anio (1,1,2020) `shouldBe` 2020
   describe "Costo de reparacion de un auto" $ do
      it "Un auto con patente AB808RD costara $12500 arreglarlo " $ do
         costoReparacion auto1 `shouldBe` 12500
      it "Un auto con patente EJS363 costara $20000 arreglarlo " $ do
         costoReparacion auto2 `shouldBe` 20000
      it "Un auto con patente EJS364 costara $18000 arreglarlo " $ do
         costoReparacion auto3 `shouldBe` 18000
      it "Un auto con patente ABC123 costara $15000 arreglarlo " $ do
         costoReparacion auto4 `shouldBe` 15000
   describe "Auto peligroso" $ do
      it "El auto1 es peligroso " $ do
         auto1 `shouldSatisfy `esPeligroso
      it "El auto2 no es peligroso " $ do
         auto2 `shouldNotSatisfy `esPeligroso
   describe "Auto necesita revision" $ do
      it "El auto1 necesita revision " $ do
         auto1 `shouldSatisfy `necesitaRevision   
      it "El auto2 no necesita revision " $ do
         auto2 `shouldNotSatisfy `necesitaRevision   
   describe "Personal tecnico encargado de las reparaciones" $ do
      it "Alfa si el auto regula a 1000 vueltas lo deja como esta" $ do
          personalAlfa auto1 `shouldBe` auto1
      it "Alfa si el auto regula a 5000 vueltas lo deja a 2000 vueltas" $ do
          personalAlfa auto2 `shouldBe` auto2b
      it "Bravo deja las llantas sin desgaste" $ do
          personalBravo auto1 `shouldBe` auto1b
      it "Charly deja las llantas sin desgaste y si el auto regula a 1000 vueltas lo deja como esta" $ do
          personalCharly auto1 `shouldBe` auto1b
      it "Charly deja las llantas sin desgaste y si el auto regula a 5000 vueltas lo deja a 2000 vueltas" $ do
          personalCharly auto3 `shouldBe` auto3b
      it "Tango deja los autos tal cual llegan"
         personalTango auto5 `shouldBe` auto5
      it "Lima deja las dos primeras llantas sin desgaste y las otras dos como estaban"
         personalLima auto5 `shouldBe` auto5b
      it "Zulu deja las dos primeras llantas arregladas y la temperatura a 90"
         personalZulu auto5 `shouldBe` auto5c