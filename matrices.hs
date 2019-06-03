import Data.List

--Tarea
a=[[2.0,0.0,1.0],[3.0,0.0,0.0],[5.0,1.0,1.0]]
b=[[1.0,0.0,1.0],[1.0,2.0,1.0],[1.0,1.0,0.0]]
c=[[1.0,2.0],[2.0,-1.0]]
d=[[-1.0,-1.0],[5.0,6.0]]
x=[[1.0,2.0,3.0],[3.0,2.0,1.0],[1.0,0.0,1.0]]
y=[[1.0,2.0,3.0,4.0],[4.0,3.0,2.0,1.0],[1.0,1.0,0.0,1.0],[1.0,1.0,1.0,1.0]]
ec=[[4,5,3],[6,-10,1]]
w=[[1.0,3.0,2.0],[1.0,0.0,0.0],[1.0,2.0,2.0]]
e=[[1.0,0.0,5.0],[7.0,5.0,0.0],[2.0,1.0,1.0]]
--suma de matrices
summat::[[Double]]->[[Double]]->[[Double]]
summat [] matB = []
summat matA []= []
summat matA matB = (sumvec (head matA) (head matB)):(summat (tail matA) (tail matB))

sumvec::[Double]->[Double]->[Double]
sumvec [] vecB = []
sumvec vecA [] = []
sumvec (x:xs) (xb:xsb) = (x+xb):(sumvec xs xsb)


--resta de matrices
resmat::[[Double]]->[[Double]]->[[Double]]
resmat [] matB = []
resmat matA []= []
resmat matA matB = (resvec (head matA) (head matB)):(resmat (tail matA) (tail matB))

resvec::[Double]->[Double]->[Double]
resvec [] vecB = []
resvec vecA [] = []
resvec (x:xs) (xb:xsb) = (x-xb):(resvec xs xsb)


--multiplicación de matrices
multMat::[[Double]]->[[Double]]->[[Double]]
multMat [] matB = []
multMat matA []= []
multMat matA matB = multAux matA (traspuesta matB):multMat (tail matA) matB

multAux::[[Double]]->[[Double]]->[Double]
multAux [] matB = []
multAux matA []= []
multAux matA matB = multvec (head matA) (head matB):multAux matA (tail matB)

multvec::[Double]->[Double]->Double
multvec [] vecB = 0.0
multvec vecA [] = 0.0
multvec (x:xs) (xb:xsb) = (x*xb)+(multvec xs xsb)

traspuesta::[[a]] -> [[a]]
traspuesta ([]:_) = []
traspuesta xss = primeros xss : traspuesta (restos xss)

primeros xss = [head xs | xs <- xss] 
 
restos xss   = [tail xs | xs <- xss]


--determinante
determinante :: [[Double]]->Double
determinante matA = if (length matA)==2 then sum(determinanteAux(matrizD(concatenaCero matA) (length matA) 0)) 
                                             - sum(determinanteAux(matrizIZ(concatenaCero matA) (length matA) ))
                    else sum(determinanteAux(matrizD (duplicaDatos matA (length matA)) (length matA) 0)) 
                         - sum(determinanteAux(matrizIZ (duplicaDatos matA (length matA)) (length matA)))

concatenaCero matA=[x++[0] |x<-matA]

determinanteAux [] = []
determinanteAux matA =producto (head matA):determinanteAux (tail matA)

producto lista= product[x | x<-lista]

duplicaDatos :: [[a]] -> Int -> [[a]]
duplicaDatos [] y = []
duplicaDatos (x:xs) y =  [x ++ take (y-1) x] ++ duplicaDatos (xs) y

matrizDiagonalIZD [] contador = []
matrizDiagonalIZD (x:xs) contador  = x !!contador : matrizDiagonalIZD xs (contador+1)

matrizD [] longitud contador = []
matrizD (x:xs) longitud contador = if contador /= longitud then 
                                           [matrizDiagonalIZD (x:xs) contador] ++ matrizD (x:xs) longitud (contador+1) 
                                        else [] 

resta [] contador = []
resta (x:xs) contador  = x !!contador : resta xs (contador-1)

matrizDiagonalDIZ [] longitud contador = []
matrizDiagonalDIZ (x:xs)  longitud contador = if longitud /= 0 then 
                                                    [resta (x:xs) contador] ++ matrizDiagonalDIZ (x:xs) (longitud-1) (contador-1) 
                                                        else [] 
                                        
matrizIZ (x:xs) longitud = matrizDiagonalDIZ (x:xs) longitud ((length x)-1)


--matriz adjunta
adjunta [] = []
adjunta matA = adjuntaDos (head matA):adjunta (tail matA)

adjuntaDos matA = [x*(-1) |x <-matA]

--división de matrices
divMat::[[Double]]->[[Double]]->[[Double]]
divMat matA matB= multMat matA (inversa matB)

division [] a = []
division (x:xs) a  = [map (/a) x] : division xs a

--matriz inversa
inversa::[[Double]]->[[Double]]
inversa matA = inversaAux (traspuesta (adjunta matA)) (determinante matA)

inversaAux::[[Double]]->Double->[[Double]]
inversaAux [] determinante= []
inversaAux matA determinante = inversaDiv (head matA) determinante : inversaAux (tail matA) determinante

inversaDiv matA determinante = [x/determinante | x<-matA]

--------------------------------------------------------------------------------------------------------------------------------------
--potencia a 2
potencia xs 1 = xs
potencia xs contador =  potencia (multMat (xs) (xs)) (contador-1) 








