module Library where
import PdePreludat

data Nave = UnaNave{
    durabilidad :: Number,
    ataque :: Number,
    escudo :: Number,
    poderEspecial :: Poder
} deriving (Show, Eq)

tieFighter :: Nave
tieFighter = UnaNave 200 100 50 movimientoTurbo

xWing :: Nave
xWing = UnaNave 300 150 100 reparacionEmergencia 

naveDarthVader :: Nave
naveDarthVader = UnaNave 500 300 200 superTurbo

millenniumFalcon :: Nave 
millenniumFalcon = UnaNave 1000 500 50 (reparacionEmergencia . modificarEscudo 100)

balduneta :: Nave
balduneta = UnaNave 300 1000 300 (superTurbo . modificarEscudo 150)


type Poder = Nave -> Nave

movimientoTurbo :: Poder
movimientoTurbo = modificarAtaque (25)

reparacionEmergencia :: Poder
reparacionEmergencia = modificarDurabilidad 50 . modificarAtaque (-30)

superTurbo :: Poder
superTurbo = modificarDurabilidad (-45) . movimientoTurbo . movimientoTurbo . movimientoTurbo

modificarAtaque :: Number -> Nave -> Nave
modificarAtaque cantidad nave = nave {ataque = ataque nave + cantidad}

modificarDurabilidad :: Number -> Nave -> Nave
modificarDurabilidad cantidad nave = nave {durabilidad = durabilidad nave + cantidad}

modificarEscudo :: Number -> Nave -> Nave
modificarEscudo cantidad nave = nave {escudo = escudo nave + cantidad}

type Flota = [Nave]

durabilidadFlota :: Flota -> Number
durabilidadFlota flota = sum (durabilidadNavesFlota flota)

durabilidadNavesFlota :: Flota -> [Number]
durabilidadNavesFlota = map durabilidad  

atacar :: Nave -> Nave -> Nave
atacar naveAtaque naveAtacada = modificarDurabilidad (- calcularDanio naveAtaque naveAtacada) naveAtacada

calcularDanio :: Nave -> Nave -> Number
calcularDanio naveAtaque naveAtacada = ataque (activarPoderEspelcial naveAtaque) - escudo (activarPoderEspelcial naveAtacada)

activarPoderEspelcial :: Nave -> Nave
activarPoderEspelcial nave = (poderEspecial nave) nave

estaFueraDeCombate :: Nave -> Bool
estaFueraDeCombate = (== 0) . durabilidad 

type Estrategia = Nave -> Bool

debil :: Estrategia 
debil = (< 200) . escudo

peligrosidad :: Number -> Estrategia
peligrosidad valor = (> valor) . ataque

quedariaFueraDeCombate :: Nave -> Estrategia
quedariaFueraDeCombate atacante = estaFueraDeCombate . atacar atacante





