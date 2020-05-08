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
costoReparacion (patente, _,_,_,_) | length patente > 7 = 12500
                     | length patente == 6 = calculoPatental
                     | (dosPatente patente) > "DJ" && dosPatente patente) < "NB" = calculoPatental
                     |otherwise = 15000
                     
dosPatente patente = take 2 patente
patente Auto = patente

calculoPatental :: Auto -> Int                     
calculoPatental (patente,_,_,_,_) | last patente == 4 = 3000* length patente
                     | otherwise = 20000
            

--PUNTO 2, parte 1
espeligroso:: Auto -> Bool
espeligroso (_,desgasteLlantas,_,_,_) = (>0.5).(head desgasteLlantas)

--PUNTO 2, parte 2

--PUNTO 3, parte 1
alfa (_,_,rpm,_,_)  | rpm < 2000 = rpm 
                    | otherwise = rpm == 2000
bravo (_,desgasteLlantas,_,_,_)  = desgasteLlantas == [0,0,0,0]

--PUNTO 3, parte 2