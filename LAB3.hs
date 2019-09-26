
--NOME : PAULO CESAR GARCIA JUNIOR   -  11811BCC003 
 
 --LISTA LAB 3 HASKELL 

-- 3) Usando lista por compreensão, gere a lista listaQuad contendo os quadrados dos
--números de 1 a 100.

lista = [ x^2 | x <- [1 .. 100]]


-- 4) Usando lista por compreensão, gere a lista listaQuadPares contendo os quadrados
-- dos números pares de 1 a 100. 

listaQuadPares = [x^2 | x <- [1 .. 100],even x]

--5) Escreva a função quadrados que recebe dois inteiros e retorna os quadrados dos
--números entre eles. E.g.:

quadrados x y = [x^2| x <- [x..y]]

--6) Usando lista por compreensão, escreva a função seleciona_ímpares que recebe
--um lista de inteiros e retorna uma nova lista com todos os números ímpares presentes na
--lista de entrada.

seleciona_ímpares = [x | x <- [1 .. 100], odd x ]

--7) Escreva a função tabuada que recebe um valor inteiro e retorna a lista de seus dez
--primeiros múltiplos. E.g.:

type Data = (Int)
tabuada x = [y*x | y <- [1 .. 10]]

--8) Escreva a função bissextos a seguir que recebe uma lista de inteiros e retorna uma
--lista com os valores que representam anos bissextos. Dica: use a função bissexto (Lab.2).
  
 type Data = (Int,Int,Int)
bissextos :: Int-> Bool
bissextos x | (mod x 400 == 0) = True
| (mod x 4 == 0) && (mod x 100 /= 0) = True
| otherwise = False
 
bissextos::[Int]->[Int]
bissextos x = [x | listaanos <- x, bissexto x]

--9) Usando lista por compreensão, escreva a função sublistas que recebe uma lista
--formada por sublistas de um mesmo tipo e retorna uma lista com todos os elementos da
-- lista de entrada na mesma ordem, mas no nível da lista principal, sem sublistas.

sublistas::[[a]]->[a]
sublistas x = [y|y<-concat x]

--10) Sejam os tipos Data, Emprestimo, Emprestimos e a variável bdEmprestimo do
--exemplo da Biblioteca. Escreva a função atrasados que recebe um parâmetro do tipo
--Emprestimos e a Data atual, e retorna uma lista com todos os empréstimos atrasados.
--Dica: use a função definida no Lab 2,

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos
bdEmprestimo =  [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),	("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"), ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)bissexto::Int->Bool
bissexto x = if (mod x 4 == 0 && mod x 100 /= 0 || mod x 400 == 0) then True else False

valida::Data->Bool
valida (d,m,a)
	| d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 ||
	 m == 8 || m == 10 || m == 12) = True
	| d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
	| d <= 29 && d >= 1 && m == 2 && (bissexto a == True) = True
	| d <= 28 && d >= 1 && m == 2 = True
	| otherwise	= False

checaEmprestimo::Emprestimo->Data->Bool
checaEmprestimo (_,_,(d1,m1,a1),(d2,m2,a2),_) (d3,m3,a3)
	|(d3 > d1) && (m3 == m1) && (a3 == a1) && (valida (d3,m3,a3) == True) && 
	(valida (d1,m1,a1) == True) && (d2 > d3) && (m2 == m3) && (a2 == a3) && (valida (d3,m3,a3) == True) && 
	(valida (d2,m2,a2) == True) = True
	| (m3 > m1) && (a3 == a1) && (valida (d1,m1,a1) == True) && 
	(valida (d3,m3,a3) == True) && (m2 > m3) && (a2 == a3) && (valida (d2,m2,a2) == True) && 
	(valida (d3,m3,a3) == True) = True
	| (a3 > a1) && (valida (d1,m1,a1) == True) && (valida (d3,m3,a3) == True) &&
	(a3 > a2) && (valida (d2,m2,a2) == True) = True
	| otherwise = False

atrasados::Emprestimos->Data->[Emprestimo]
atrasados x y = [z|z<-x, checaEmprestimo z y == False]
