module Library where
import PdePreludat

-- Punto 1) 

type Altura = Number
type Peso = Number

-- Enfoque TOP-DOWN


-- Expresivo: lo puedo leer en 2 semanas y recordar por qué lo escribí así. Es subjetiva.
-- Declarativo: veo más qué y menos cómo
-- Flexibilidad: por el cambio climático las bases pesan 2.5 kg por cm 

-- pesoPino altura = min altura 3 * 300 + max (altura - 3) 0 *200
-- (menos )
pesoPino :: Altura -> Peso
pesoPino altura = pesoBase altura + pesoCopa altura

pesoXCMDeLaBase:: Number
pesoXCMDeLaBase = 3
pesoBase :: Altura -> Peso
pesoBase alturaEnM = pesoXCMDeLaBase * alturaBase alturaEnM * 100

alturaBase :: Altura -> Altura
alturaBase altura = min 3 altura

pesoCopa :: Altura -> Peso
pesoCopa alturaEnM = 2 * alturaCopa alturaEnM * 100

alturaCopa :: Altura -> Altura
alturaCopa altura = altura - alturaBase altura

-- Punto 2) 


-- int pesoPino(int altura){ return pesoBase(altura) + }

a:: Number
a = 5

data Gimnasta = UnGimnasta {
    peso :: Number,
    tonificacion :: Number,
    apodo :: String
} deriving (Show, Eq)


-- Dos formas de construir un gimnasta:
nazareno :: Gimnasta
nazareno = UnGimnasta 150 10 "Naza"

facu :: Gimnasta
facu = UnGimnasta { peso=100, tonificacion=11, apodo="Facu"}

------------------------------------


-- No hablamos de Bruno (tuplas)

type Ayudante = (String,Number, Number)

nombreAyudante :: Ayudante -> String
nombreAyudante (nombre,_,_) = nombre

edadAyudante :: Ayudante -> Number
edadAyudante (_,edad,_) = edad

data AyuData = UnAyudante {
    nombre2 :: String,
    edad :: Number,
    peso2 :: Number
}


------------------------------------------------------------------

data Fecha = UnaFecha {
   dia :: Number,
   mes :: Number,
   anio :: Number
} deriving (Show)

data Cumpleaniero = UnCumpleaniero {
    nombre :: String,
    fechaNac :: Fecha,
    nombresAmigos :: [String]
} deriving (Show)

-- Punto 1) esMayor que recibe dos personas y me dice si la primera es mayor que la segunda.

esMayor :: Cumpleaniero -> Cumpleaniero -> Bool
esMayor uno otro = esMayorFecha (fechaNac otro) (fechaNac uno)

esMayorFecha :: Fecha -> Fecha -> Bool
esMayorFecha una otra = anio una > anio otra

juan :: Cumpleaniero
juan = (UnCumpleaniero "juan" (UnaFecha 1 1 1900) []) 
ana :: Cumpleaniero
ana = (UnCumpleaniero "ana" (UnaFecha 1 1 1800) ["juan"]) 

-- Punto 2) ponerApodo, que recibe una persona y un apodo, y reemplaza su nombre por su apodo.

ponerApodo :: Cumpleaniero -> String -> Cumpleaniero
ponerApodo alguien apodo = UnCumpleaniero apodo (fechaNac alguien) (nombresAmigos alguien)

-- Así se crea un cumpleañero copiando todo y sólo poniendo apodo en el nombre
-- ponerApodo alguien apodo = alguien { nombre = apodo }
-- EXACATAMENTE lo mismo que arriba.

miMin :: Number -> Number -> Number
miMin nro1 nro2 | nro1 > nro2 = nro1
                | otherwise = nro2