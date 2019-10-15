import Data.Matrix

--Calcula el maximo entre 3 elementos
max3 :: Ord a => a -> a -> a -> a  
max3 a b c = if a >= b then (if a >= c then a else (if b >= c then b else c)) else (if b >= c then b else c)

--Calcula el maximo entre 3 tuplas de 3 elementos y devuelve una tupla de 2 elementos
max3ij :: Ord a1 => (a1, a2, b) -> (a1, a2, b) -> (a1, a2, b) -> (a2, b)
max3ij (av, ai, aj) (bv , bi, bj) (cv, ci, cj) = if av >= bv then (if av >= cv then (ai,aj) else (if bv >= cv then (bi, bj) else (ci,cj))) else (if bv >= cv then (bi,bj) else (ci,cj))

-- Chekea que la subsecuencia entre i y j sea una SCC en base a si fue extendida en la casilla (i,j)
cond2 :: Int -> Int -> Matrix Int -> Bool
cond2 i j mx |j <= 0	= True
	     |otherwise = getElem i j mx > (max3 (getElem i (j-1) mx) (getElem (i+1) j mx) (getElem (i+1) (j-1) mx)) 

--Checkea que los elementos en las posiciones i y j sean maryores a la suma de los elementos entre ellas y que la subsecuencia sin los elementos i y j es una SCC
cond :: (Ord a, Num a) => Int -> Int -> [a] -> Matrix Int -> Bool
cond i j orgn mx |j-i == 1 = True
		 |otherwise= let
				f = [ x | (x,a) <- zip orgn [1..], a > i && a < j ]
		   	in
			(orgn!!(i-1) >= sum f) && (orgn!!(j-1) >= sum f) && cond2 (i+1) (j-1) mx
                 

--Corta una lista apartir de la posicion a hasta b
trim :: (Num a1, Enum a1, Ord a1) => a1 -> a1 -> [a2] -> [a2]
trim a b orgn = [ x | (x,i) <- zip orgn [1..], i >= a && i <= b ]

-- Calcula la matriz que permite hacer el calculo del algoritmo
-- bdp :: Int          Coordenada I
-- -> Int              Coordenada J
-- -> Int              Inicio, generalmente es 2
-- -> Int              Fin, Uno mas que el largo de la lista
-- -> [Int]            Lista de input
-- -> Matrix Int       Matriz en la q va la respuesta
bdp :: (Ord a, Num a) => Int -> Int -> Int -> Int -> [a] -> Matrix Int -> Matrix Int
bdp  i j b e orgn mx | (i == 1 && j == e)    = mx
bdp  i j b e orgn mx | i == e || j == e      = bdp 1 b (b+1) e orgn mx
bdp  i j b e orgn mx | otherwise             =
        if cond i j orgn mx
                then bdp (i+1) (j+1) b e orgn (setElem (j-i+1) (i,j) mx)
                else bdp (i+1) (j+1) b e orgn (setElem (max3(getElem i (j-1) mx) (getElem (i+1) j mx) (getElem (i+1) (j-1) mx)) (i,j) mx)
        

-- Recorre la matriz generada por bdp para obtener la subsequencia 
-- adp :: Int       Coordenada I, tendria que ser 1
-- -> Int           Coordenada J, tendria que ser el largo de la lista
-- -> [Int]         Lista del input
-- -> Matrix Int    Matriz precalculada
-- -> Int           Longitud de la subsequencia, elemento de la matriz en la posicion I = 1 y J = Largo de la lista
adp :: Ord t => Int -> Int -> [a] -> Matrix t -> t -> [a]
adp i j orgn mx last | last > (max3(getElem i (j-1) mx) (getElem (i+1) j mx) (getElem (i+1) (j-1) mx)) = trim i j orgn
adp i j orgn mx last | otherwise = adp (fst maximum) (snd maximum) orgn mx last
        		where 	maximum = (max3ij ((getElem i (j-1) mx),i,j-1) ((getElem (i+1) j mx),i+1,j) ((getElem (i+1) (j-1) mx), i+1, j-1))
                        
-- Coordina bdp y adp
dp :: (Ord a, Num a) => [a] -> [a]
dp orgn =   let
    l = length orgn
    mx = bdp 1 2 2 (l+1) orgn (identity l)
    ms = getElem 1 l mx
    in
        adp 1 l orgn mx ms
