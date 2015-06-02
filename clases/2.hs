import Data.List (nub)
import Data.Tuple (swap)

crearPar :: a -> b -> (a,b)
crearPar = (,)

invertir :: (a,b) -> (b,a)
invertir = swap

distancia :: (Float,Float) -> (Float,Float) -> Float
distancia (x0,y0) (x1,y1) = sqrt $ (x0-x1)**2 + (y0-y1)**2

raices :: (Floating a, Eq a) => a -> a ->  a -> [a]
raices a b c = nub $ [(-b + s * sqrt (b**2 - 4 * a * c)) / (2/a) | s <- [-1,1]]

