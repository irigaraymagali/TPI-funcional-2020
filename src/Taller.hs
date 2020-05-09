module Taller where

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Float,
 temperaturaAgua :: Float,
 ultimoArreglo :: Fecha
} deriving Show

--PUNTO 1
costoReparacion :: Auto -> Int
costoReparacion Auto  | length (patente Auto) > 7 = 12500
                      | length (patente Auto) == 6  && patenteEntre  = calculoPatental
                      | otherwise = 15000

patenteEntre :: String -> Bool
patenteEntre patente = patente > "DJ" &&  patente < "NB"          

calculoPatental :: Auto -> Int                     
calculoPatental Auto | last (patente Auto) == 4 = 3000* length (patente Auto)
                     | otherwise = 20000
            
--PUNTO 2, parte 1
esPeligroso = (>0.5).(head desgasteLlantas)

--PUNTO 2, parte 2
necesitaRevision :: Auto -> Bool
necesitaRevision = (<=2015).(anio).(ultimoArreglo)

--PUNTO 3, parte 1

personalAlfa :: Auto -> Auto
personalAlfa Auto | (rpm Auto) < 2000 = Auto
                  | otherwise = patente desgasteLlantas 2000 temperaturaAgua ultimoArreglo

personalBravo :: Auto -> Auto
personalBravo Auto = patente [0,0,0,0] rpm temperaturaAgua ultimoArreglo

personalCharly :: Auto -> Auto
personalCharly Auto = personalAlfa && personalBravo


--PUNTO 3, parte 2
personalTango :: Auto -> Auto
personalTango Auto = Auto

personalLima :: Auto -> Auto
personalLima 