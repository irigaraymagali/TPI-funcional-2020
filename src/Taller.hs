module Taller where

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
type Mecanico = Auto -> Auto
 
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
costoReparacion auto  | length (patente auto) > 7 = 12500
                      | length (patente auto) == 6  && patenteEntre  = calculoPatental
                      | otherwise = 15000

patenteEntre :: String -> Bool
patenteEntre patente = patente > "DJ" &&  patente < "NB"          

calculoPatental :: Auto -> Int                     
calculoPatental auto | last (patente auto) == 4 = 3000* length (patente auto)
                     | otherwise = 20000
            
--PUNTO 2, parte 1
esPeligroso :: Auto -> Bool
esPeligroso = (>0.5).head.desgasteLlantas

--PUNTO 2, parte 2
necesitaRevision :: Auto -> Bool
necesitaRevision = (<=2015).(anio).(ultimoArreglo)

--PUNTO 3, parte 1
personalAlfa :: Mecanico
personalAlfa auto | (rpm auto) < 2000 = auto
                  | otherwise = auto {rpm = 2000 }
                      
personalBravo :: Mecanico
personalBravo auto = auto {desgasteLlantas = [0,0,0,0]}

personalCharly :: Mecanico
personalCharly = personalAlfa.personalBravo

--PUNTO 3, parte 2
personalTango :: Mecanico
personalTango auto = auto

arregloLlantas :: [Desgaste] -> [Desgaste]
arregloLlantas = [0,0,_,_]

personalLima :: Mecanico
personalLima auto = auto {desgasteLlantas = arregloLlantas}

trabajoZulu :: Mecanico
trabajoZulu auto = auto {temperaturaAgua = 90}

personalZulu :: Mecanico
personalZulu = personalLima.trabajoZulu

