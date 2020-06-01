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

--PUNTO 6, parte 1
--tecnicosLoDejanEnCond :: [Auto] -> [Auto] 
tecnicosLoDejanEnCond = filter autoEnCondiciiones 

autoEnCondiciiones :: Auto -> Bool
autoEnCondiciiones = not.esPeligroso

--PUNTO 6, parte 2

--PUNTO 7, parte 1
--Considerando una lista de técnicos  infinita, ¿podríamos obtener el primer técnico que deja el auto
--en condiciones? Muestre un ejemplo y justifique. 
--Si, se puede hacer ya que se utiliza lazy evaluation lo cual hace que antes de tener que obtener la lista 
--completa (que seria imposible porque es infinita) opera con la funcion head, lo cual nos devuelve el primer elemento.

--primeroEnDejarloEnCond :: [Auto] -> Auto
primeroEnDejarloEnCond = head.tecnicosLoDejanEnCond

--PUNTO 7, parte 2


 --Tecnicos y autos infinitos
tecnicosInfinitos = personalZulu:tecnicosInfinitos
 
autosInfinitos :: [Auto]
autosInfinitos = autosInfinitos' 0
 
autosInfinitos' :: Float -> [Auto]
autosInfinitos' n = Auto {
 patente = "AAA000",
 desgasteLlantas = [n, 0, 0, 0.3],
 rpm = 1500 + n,
 temperaturaAgua = 90,
 ultimoArreglo = (20, 1, 2013)
} : autosInfinitos' (n + 1)

