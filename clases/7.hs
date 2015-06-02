import Data.List (nub)
import Data.Maybe (isJust)

data Direccion = Norte | Este | Sur | Oeste deriving Show
type Tortuga = (Pos, Direccion)
type Pos = (Int,Int)

instance Enum Direccion where
    fromEnum Norte = 0
    fromEnum Este  = 1
    fromEnum Sur   = 2
    fromEnum Oeste = 3
    toEnum 0 = Norte
    toEnum 1 = Este
    toEnum 2 = Sur
    toEnum 3 = Oeste
    toEnum n = toEnum $ abs $ n `mod` 4

arranca :: Tortuga
arranca = ((0,0),Norte)

girarDerecha :: Tortuga -> Tortuga
girarDerecha = fmap succ

girarIzquierda :: Tortuga -> Tortuga
girarIzquierda = fmap pred

avanzar :: Int -> Tortuga -> Tortuga
avanzar i ((x,y),d) = case d of
                          Norte -> ((x+i,y),d)
                          Sur   -> ((x-i,y),d)
                          Este  -> ((x,y+i),d)
                          Oeste -> ((x,y-i),d)

showTortuga :: Tortuga -> String
showTortuga (xy,d) = "Una tortuga yendo al " ++ show d
                        ++ " en las coords " ++ show xy

-------------------------------------

type Punto = (Float,Float)
data Figura = Rectangulo Punto Punto | Circulo Punto Float 
            | Triangulo Punto Punto Punto deriving (Eq,Show)

circPi = Circulo (0,0) pi

tRectangulo :: Float -> Float -> Figura
tRectangulo x y = Triangulo (0,0) (x,0) (0,y)

area :: Figura -> Float
area (Rectangulo (x0,y0) (x1,y1)) = (x0-x1) * (y1-y1)
area (Circulo _ r) = pi * r**2
area (Triangulo p r q) = sqrt $ s * (s-a) * (s-b) * (s-c)
    where a = norma p r
          b = norma r q
          c = norma q p
          s = 0.5 * (a+b+c)

norma :: Punto -> Punto -> Float
norma (x0,y0) (x1,y1) = sqrt $ (x0-x1)**2 + (y0-y1)**2

---------------------------------------

divisores :: Integer -> [Integer]
divisores x = nub $ 1:x:[n | n <- [2..x `div` 2] , x `mod` n == 0]

esPrimo :: Integer -> Bool
esPrimo n = not $ any ((==0).mod n) [2..floor $ sqrt $ fromIntegral n]

primosEntre :: Integer -> Integer -> [Integer]
primosEntre n m = takeWhile (<=m) $ filter esPrimo $ [n..]

primosHasta :: Integer -> [Integer]
primosHasta = primosEntre 1

hayPrimosEntre :: Integer -> Integer -> Bool
hayPrimosEntre = fmap (not.null) . primosEntre

bertrand :: Integer -> Bool
bertrand n = bGeneral n n 1 2

bGeneral :: Integer -> Integer -> Integer -> Integer -> Bool
bGeneral d h a b = all (\n -> hayPrimosEntre (a*n) (b*n)) [d..h]

goldbach :: Integer -> Maybe (Integer,Integer)
goldbach n = if not $ null tuples then Just $ head tuples else Nothing
    where tuples = [(p,q) | p <- primosHasta $ n `div` 2, let q=n-p, primo q]

isGoldbach :: Integer -> Bool
isGoldbach = isJust . goldbach

