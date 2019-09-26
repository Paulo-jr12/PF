--2) Dado um valor monetário em reais, escreva uma função conversao que retorna uma tupla-3
-- com o valor em Real, Dolar e Euro, sendo que 1 real = 3,96 dólares = 4,45 euros.

type dinheiro = (Float,Float,Float,Float)
conversao Float -> Dinheiro 
conversao x = (x,x*3.96,x*4.45)
 
--  3) Implemente a função bissexto que, dado um ano (inteiro), indique se ele é bissexto ou não

type Data = (Int,Int,Int)
bissexto :: Int -> Bool
bissexto x 
  | (mod x 400 == 0 ) = True
  | (mod x 4 == 0 && (mod x 100 \= 0) = True
  | otherwise = False 
  
-- 4) Defina o tipo Data dado a seguir. Escreva a função bissexto2 que recebe uma data e indique
-- se ela pertence a um ano bissexto ou não.

type Data = (Int, Int, Int)
bissexto2::Data->Bool
bissexto2 (dia,mes,ano) = if bissexto ano = True then True else False 

-- 5) Escreva a função valida que indica se uma data é válida ou não.


valida::Data->Bool
valida (d,m,a)  
 | d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || 
  m == 8 || m == 10 || m == 12) = True
 | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
 | d >= 1 && d <= 28 && m == 2 && not (bissexto a) = True
 | d >= 1 && d <= 29 && m == 2 && (bissexto a ) = True
 | otherwise = False 
 
-- 6) Escreva a função precede que recebe 2 datas e indica se a 1a data é anterior à 2a.

precede::Data -> Data -> Bool
precede (d1,m1,a1) (d2,m2,a2)
 | (d2 > d1 ) && (m2 == m1) && (a2 == a1) && (valida(d2,m2,a2) == True) && 
 (valida(d1,m1,a1) == True ) = True 
 | (m2 > m1) && (a2 == a1) && (valida(d2,m2,a2) == True) && 
 (valida(d1,m1,a1) == True ) = True
 | (a2 < a1) && (valida(d2,m2,a2) == True) && (valida(d1,m1,a1) == True ) = True 
 | otherwise = False

--7) Implemente as estruturas de dados (tuplas) para um sistema de gerenciamento de bibliotecas e
--depois as defina como tipos. O sistema tem 3 estruturas básicas:
--Livro: composto por código do livro, título do livro, autor, editora e ano de publicação.
--Aluno: composto por código do aluno, nome, e-mail e telefone.
--Empréstimo: composto por código do livro, código do aluno, data de empréstimo, data de
--devolução e situação. Obs: utilize a estrutura/tipo auxiliar data do exercício 4.
 
type Livro = (Int,String,String,String,String,Int)
type Aluno = (Int,String,String,Int)
type Empréstimo = (Int,Int,int,Int,String ) 

-- 8) Seja o tipo Emprestimo e o exemplo dado a seguir, composto por código do livro, código do
-- aluno, data de empréstimo, data de devolução e situação. Escreva uma função que verifica se um
-- empréstimo está em dia, dado um empréstimo e a data de hoje.

type Emprestimo = (String, String, Data, Data, String)
e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")
 
verificaEmprestimo::Emprestimo -> Data -> Bool 
verificaEmprestimo (_,_,(d1,m1,a1),(d2,m2,a2),_)(d3,m3,a3)
          | (d3 > d1) && (m3 == m3) && (a3 == a3) && (valida (d3,m3,a3) == True) &&
		  (valida (d1,m1,a1) == True) && (d2 > d3) && (m2 == m3) && (a2 == a3) && (valida (d3,m3,a3) == True) &&
		  (valida (d2,m2,a2) == True) = True 
		  | (m3 > m1) && (a3 == a1)&& (valida (d1,m1,a1) == True) &&
		  (valida (d3,m3,a3) == True) && (m2 > m3) && (a2 == a3) && (valida (d2,m2,a2) == True) &&
		  (valida (d3,m3,a3) == True) = True
		  | (a3 > a1) && (valida (d1,m1,a1) == True ) && (valida (d3,m3,a3) == True) &&
		  (a3 > a2) && (valida (d2,m2,a2) == True) = True
		  | otherwise = False 
 
 