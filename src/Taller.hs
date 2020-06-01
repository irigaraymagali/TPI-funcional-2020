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
costoReparacion auto  | patenteVieja auto = 12500
                      | patenteNueva auto  && (patenteEntreDJyNB.patente) auto = calculoPatental auto
                      | otherwise = 15000

patenteVieja :: Auto -> Bool
patenteVieja auto = (length.patente) auto == 7

patenteNueva :: Auto -> Bool
patenteNueva auto = (length.patente) auto == 6

patenteEntreDJyNB :: Patente -> Bool
patenteEntreDJyNB patente = patente > "DJ" &&  patente < "NB"          

calculoPatental :: Auto -> Int                     
calculoPatental auto | (last.patente) auto == '4' = 3000* (length.patente) auto
                     | otherwise = 20000
            
--PUNTO 2, parte 1
esPeligroso :: Auto -> Bool
esPeligroso = (>0.5).head.desgasteLlantas

--PUNTO 2, parte 2
necesitaRevision :: Auto -> Bool
necesitaRevision = (<=2015).anio.ultimoArreglo

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

arregloDosLlantas :: [Desgaste] -> [Desgaste]
arregloDosLlantas [_,_,c,d]= [0,0,c,d]

personalLima :: Mecanico
personalLima auto = auto {desgasteLlantas = (arregloDosLlantas.desgasteLlantas)auto} 

estabilidadDeTemperatura :: Mecanico
estabilidadDeTemperatura auto = auto {temperaturaAgua = 90} 

personalZulu :: Mecanico
personalZulu = personalLima.estabilidadDeTemperatura

--PUNTO 4
estaOrdenadoCriterio _ [] = True
estaOrdenadoCriterio condicion (x:xs) |condicion x = estaOrdenadoCriterio (not.condicion) xs

autosOrdenados = estaOrdenadoCriterio odd

--PUNTO 5
mecanicos = [personalAlfa,personalBravo,personalCharly,personalTango,personalLima,personalZulu]

arreglosTecnicos auto = foldl auto  mecanicos
cambioFecha auto fecha = auto {ultimoArreglo = fecha}
