import Test.Hspec
import Taller

main :: IO()
main = hspec $ do
   describe "Taller mecanico" $ do
      it "anio de una fecha" $ do
         anio (1,1,2020) `shouldBe` 2020
   describe "Costo de reparacion de un auto" $ do
      it "Un auto con patente AB808RD costara $12500 arreglarlo " $ do
         costoReparacion "AB808RD" desgasteLlantas rpm temperaturaAgua ultimoArreglo `shouldBe` 12500
      it "Un auto con patente EJS363 costara $20000 arreglarlo " $ do
         costoReparacion "EJS363" desgasteLlantas rpm temperaturaAgua ultimoArreglo `shouldBe` 20000
      it "Un auto con patente EJS364 costara $18000 arreglarlo " $ do
         costoReparacion "EJS364" desgasteLlantas rpm temperaturaAgua ultimoArreglo `shouldBe` 18000
      it "Un auto con patente ABC123 costara $15000 arreglarlo " $ do
         costoReparacion "ABC123" desgasteLlantas rpm temperaturaAgua ultimoArreglo `shouldBe` 15000
   describe "Auto peligroso" $ do
      it "Un auto con la primera llanta muy desgastada es peligroso " $ do
         patente [1,_,_,_] rpm temperaturaAgua ultimoArreglo `shouldSatisfy esPeligroso
      it "Un auto con la primera llanta poco desgastada no es peligroso " $ do
         patente [0.2,_,_,_] rpm temperaturaAgua ultimoArreglo `shouldNotSatisfy esPeligroso
   describe "Auto necesita revision" $ do
      it "Un auto con el ultimo arreglo en 2010 necesita revision " $ do
         patente desgasteLlantas rpm temperaturaAgua 2010 `shouldSatisfy necesitaRevision   
      it "Un auto con el ultimo arreglo en 2020 no necesita revision " $ do
         patente desgasteLlantas rpm temperaturaAgua 2020 `shouldNotSatisfy necesitaRevision   
      