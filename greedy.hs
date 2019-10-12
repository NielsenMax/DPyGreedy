-- El Int es para la arista que da origen al nodo, en el caso de la raiz es 0
data Tree a = N Int a [Tree a] deriving (Show)

-- Funcion que toma una listas de arboles y devuelve el arbol cuya arista que le da origen es la de menor peso
tmin :: [Tree a] -> Tree a
tmin [x] = x
tmin (x:xs) = let
                minim (x@(N xe xa xs)) (y@(N ye ya ys)) | xe > ye  = y
                                                        | xe <= ye = x
            in
                minim x (tmin xs) 

-- Funcion que ejecuta el algoritmo Greedy
greedy :: Tree a -> [a]
greedy (N e a []) = [a]
greedy (N e a xs) = a : greedy (tmin xs)

-- Arbol para testear
-- t = N 0 'a' [(N 0 'b' [(N 1 'c' []),(N 2 'd' [])]),(N 3 'e' [(N 1 'f' []),(N 1 'g' [])]),(N 7 'h' [(N 2 'i' []),(N 0 'j' [])])] 