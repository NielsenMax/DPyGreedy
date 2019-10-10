import Data.Matrix

-- getElem
-- :: Int	Row      j
-- -> Int	Column   i 
-- -> Matrix a	 Matrix
-- -> a	

max3 a b c = if a >= b then (if a >= c then a else (if b >= c then b else c)) else (if b >= c then b else c)

cond a b orgn  =  (orgn!!(a-1) >= sum f) && (orgn!!(b-1) >= sum f)
                where f = [ x | (x,i) <- zip orgn [1..], i > a && i < b ]

--cond a b c = True

pdp  i j b e orgn mx | (i == 1 && j == e)    = mx
pdp  i j b e orgn mx | i == e || j == e      = pdp 1 b (b+1) e orgn mx
pdp  i j b e orgn mx | otherwise             =
    if cond i j orgn 
        then pdp (i+1) (j+1) b e orgn (setElem ((max3 (getElem (j-1) i mx) (getElem j (i+1) mx) (getElem (j-1) (i+1) mx) )+1) (j,i) mx)
        else pdp (i+1) (j+1) b e orgn (setElem (max3 (getElem (j-1) i mx) (getElem j (i+1) mx) (getElem (j-1) (i+1) mx) ) (j,i) mx)

