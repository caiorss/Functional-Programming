
import Graphics.Gnuplot.Simple

type Matrix = [[Double]]

--  Kronecker delta 
-- 
--- http://en.wikipedia.org/wiki/Kronecker_delta
--
δ :: (Int, Int) -> Double
δ (i, j) | i==j = 1.0
         | otherwise = 0.0


eye :: Int -> [[Double]]
eye   n  = [ [ δ(i, j) | i <- [0..(n-1)]] | j <- [0..(n-1)]]

ones :: Int -> [[Double]]
ones  n  = [ [ 1.0   | i <- [0..(n-1)]] | j <- [0..(n-1)]]

zeros :: Int -> [[Double]]
zeros n  = [ [ 0.0   | i <- [0..(n-1)]] | j <- [0..(n-1)]]

-- Function to Display Matrix 
disp matrix =   putStrLn   $ concat $  map (\row ->  show row  ++ "\n" )  matrix

enumerate seq = zip [0..(length seq)] seq

eleM matrix (i, j) =  (matrix !! i) !! j   -- eleMent i, j
colM matrix i = map (!!i) matrix        -- Matrix row    i
rowM matrix i = matrix !! i             -- Matrix column i

addv va vb = zipWith (+) va vb
subv vb va = zipWith (-) vb va
mulv vb va = zipWith (*) va vb
mulvx x v  = map (*x) v

normv vector = sqrt $ sum $ map (\e -> e^^2) vector

nrowsM matrix = length matrix
ncolsM matrix = length (matrix !! 0)

-- Subtract Matrix of equal sizes
subm mb ma = map (\(b, a) -> zipWith (-) b a) (zip mb ma)

-- Add matrix eleMent by eleMent
addm mb ma = map (\(b, a)  -> zipWith (+) b a) (zip mb ma)


-- Multiply matrix a and b
mullm ma mb =  [ [ c i j   | i <- [0..(nrows -1)]] | j <- [0..(ncols -1)]]
    where
    c i j = sum $ zipWith (*) (rowM ma i) (colM mb j)
    nrows = length ma
    ncols = length (mb !! 0)


mullmx x ma = (map $ map (*x)) ma


mullmcol matrix col = map (\row -> sum $ zipWith (*) row col)  matrix

-- Matrix Multiplication Operator
(@@) ma mb = mullm ma mb

powm matrix n = foldr (mullm)  (eye (length matrix)) $ take n $ repeat matrix

-- Transpose Matrix
transpM matrix =  map (colM matrix) [0..((ncolsM matrix) -1)] 

matrixIterator :: ((Int, Int) -> Double) -> [[Double]] -> [[Double]]
matrixIterator element_ij matrix = newmatrix 
    where
    lim_i = (nrowsM matrix) - 1
    lim_j = (nrowsM matrix) - 1
    matrix_ij = eleM matrix

    newmatrix = [ [ element_ij (j, i) * matrix_ij(j, i) | i <- [0..lim_i]] | j <- [0..lim_j]]

-- Create Diagonal matrix from a Matrix
-- Cij = Aij * δij 
-- Cij = 0 if i == j, = Aii if i == j
--
diagM matrix = matrixIterator δ matrix

-- Triangular Inferior Matrix
-- 
matrixL matrix = matrixIterator e_ij matrix 
    where
    e_ij (i, j)  | i <=j     = 0
                 | otherwise = 1

-- Create a Triangular superior matrix from a given matrix
--
matrixU matrix = matrixIterator e_ij matrix
    where
    e_ij (i, j)  | i >=j     = 0
                 | otherwise = 1


matrixLU matrix = matrixIterator e_ij matrix
    where
    e_ij (i, j) | i /= j    = 1
                | otherwise = 0   

diagonalM matrix = map (\i -> (eleM matrix)(i, i)) [0..((nrowsM matrix)-1)] 



inverse3x3 :: [[Double]] -> [[Double]]
inverse3x3 matrix = inversedm
    where
    em = curry $ eleM matrix
    a = em 0 0  ; b = em 0 1 ; c = em 0 2  
    d = em 1 0  ; e = em 1 1 ; f = em 1 2  
    g = em 2 0  ; h = em 2 1 ; i = em 2 2
    
    aA = e*i - f*h
    bB = -(d*i -f*g)
    cC = d*h - e*g
    dD = -b*i + c*h
    eE = a*i - c*g
    fF = -a*h + b*g
    gG =  b*f - c*e
    hH = -a*f + c*d
    iI = a*e - b*d
    invdet = 1/(a*aA + b*bB + c*cC)
    inversedm = mullmx invdet  [[aA, dD, gG], [bB, eE, hH], [cC, fF, iI]]
                            
pairs = tail 

jacobi_step matrix vector_b vector_x = zipWith (/) (zipWith (-) vector_b  (mullmcol lum vector_x)) diag
    where
    lum  = matrixLU  matrix
    diag = diagonalM matrix

--mat = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Double]]



--b = [-1, 2, 3] :: [Double]
--matrixA  = [[5, -2, 3], [-3, 9, 1], [2, -1, -7]] :: [[Double]]

jacobi matrixA vector_b vector_x0 = vector_x
    where
    generator = iterate ( jacobi_step matrixA vector_b) vector_x0

    errorv vx = normv $ (matrixA `mullmcol` vx) `subv` vector_b 
    until_error vx = (errorv vx) >= 1e-5
    vector_x = last $ take 200 $ takeWhile until_error  generator



--ss_setep :: [[Double]] -> [Double] -> Double ->  (Double -> [Double]) -> (Double, [Double]) -> (Double, [Double])
ss_step a b dt u (t, x) = (tnext, xnext) 
    where
    -- (I + dt*A)
    da = (mullmx dt a)  `addm` eye (length a)
    
    -- dt*B
    db = mulvx dt b 
    
    xnext = (mullmcol da x) `addv` ( (u t) `mulvx` b) 
    tnext = t + dt



a = [[-20, -40, -60], [1,  0,  0], [0,  1, 0]] :: Matrix
b = [1, 0, 0] :: [Double]
c = [0, 0, 1] :: [Double]
d = [0, 0, 0] :: [Double]

u :: Double -> Double
u t = 1.0
--u t =  0.0

x0 = [0.1, 0.1, 0.1]


ssys = ss_step a b 0.01 u

simu = take 1000 $ iterate ssys (0, x0) 

t_vector = map fst simu
x_vector = map snd simu
y_vector = map (!!2) x_vector

plot_solution = plotList [] (zip t_vector y_vector) 

 
{-
Testing:

    a = [[1, 3, 5],   [2, 4, 6]]  :: [[Double]]
    b = [[3, 6], [1, 4], [5, 2]]  :: [[Double]]        

    λ > 
    λ > [[3.0, 4.0], [5.0, 6.0]] @@ [[-3.0, 2], [5/2, -3/2]]
    [[1.0,0.0],[0.0,1.0]]
    λ > 
    
    mat = [[3.0, 4.0], [5.0, 6.0]] :: [[Double]]
-}
