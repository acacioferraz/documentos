module Relatorio where
import System.IO
{-relatorio :: Int -> IO()
relatorio n = putStr (cabecalho ++ corpo ++ rodape)
-}

--Universidade Estadual do Sudoesta da Bahia
--Curso Ciência da Computação
--Programação Declarativa
--Trabalho da 2º Unidade - Relatorio de Vendas em Haskell
--Data Inicio: 28/10/2020 - Data Entrega: 13/11/2020

--Alunos: Acácio Ferraz, Adrian, Ademilson, Alberto, Alef

tamanhoLinha :: Int
tamanhoLinha = 60

cabecalho :: String
cabecalho = imprimir tamanhoLinha " " ++ "\n" ++ 
			imprimirTitulo ++ "\n" ++
			imprimir tamanhoLinha " " ++ "\n" ++
			titulo2 ++ "\n"

imprimirTitulo :: String
imprimirTitulo = imprimir quantidade " " ++ titulo ++ imprimir quantidade " " 
	where
		quantidade :: Int
		quantidade = div (tamanhoLinha - (length titulo)) 2

titulo :: String
titulo = "Relatorio de Vendas"

titulo2 :: String
titulo2 = "Mes         Quantidade         R$ "

imprimir :: Int -> String -> String
imprimir 0 c = " "
imprimir n c = c ++ imprimir(n-1) c

corpo :: String 
corpo = " "

--rodape :: Int -> String
--rodape n = 30 ++ "\n" ++ "Mes "++ "         "  ++ "Quantidade" ++ "\n" ++ espacamento (25 - length(imprimirLinhas n)) ++ imprimirLinhas n

vendasMes :: Int -> Int
vendasMes mes 
			|mes == 1 = 20
			|mes == 2 = 32
			|mes == 3 = 21
			|mes == 4 = 60
			|mes == 5 = 25
			|mes == 6 = 12
			|mes == 7 = 52
			|mes == 8 = 28
			|mes == 9 = 29
			|mes == 10 = 40
			|mes == 11 = 50
			|mes == 12 = 33
			|otherwise = 0			

-- soma de todas as vendas no periodo de um ano
soma :: Int -> Int
soma n 
	|n == 0	= vendasMes 0
	|n > 0 = soma(n-1) + vendasMes(n)
	|otherwise = 0

total :: Int 
total = soma 12

vendazero :: Int -> Int
vendazero 0 = 0
vendazero n 
	| vendasMes(n) == 0 = 1 + vendazero(n-1)
	| otherwise = vendazero(n-1)

-- quantidade maior de vendas no periodo de um mes
maior :: Int -> Int
maior n
	| n == 1 = vendasMes 1
	|otherwise = max(vendasMes n) (maior(n-1))

--media de vendas no periodo de um ano
media :: Int -> Int
media n = div(soma n) n

mediageral :: Int
mediageral = media 12

-- quantidade menor de vendas no periodo de um mes
menor :: Int -> Int
menor n 
	| n == 1 = vendasMes 1
	| otherwise = min(vendasMes n) (menor(n-1)) 

imprimemes :: Int -> String
imprimemes n = impmes n 
	where
		impmes n
			| n == 12 = show (mes n) ++ "\n"
			| otherwise = show (mes n) ++ "\n" ++ impmes (n+1)

--emprime :: Int
--emprime = imprimemes 12

-- mes referente ao numero
mes :: Int -> String
mes n 
	| n==1 = "Janeiro"
	| n==2 = "Fevereiro"
	| n==3 = "Marco"
	| n==4 = "Abril"
	| n==5 = "Maio"
	| n==6 = "Junho"
	| n==7 = "Julho"
	| n==8 = "Agosto"
	| n==9 = "Setembro"
	| n==10 = "Outubro"
	| n==11 = "Novembro"
	| n==12 = "Dezembro"
	|otherwise = ""

valor :: Int
valor = 12

imprimirLinha :: Int -> String
imprimirLinha n = mes n ++ "     " 
				++ show (vendasMes n) 
				++ "        R$ " 
				++ show (valor*(vendasMes n)) ++ "\n"

imprimirLinhas :: Int -> String
imprimirLinhas n 
	|n == 1 = imprimirLinha 1
	|otherwise =  imprimirLinhas(n-1) ++ imprimirLinha(n)


rodape :: String
rodape = "Total        " ++ show(total) ++ "     R$ " ++ show (total*12) ++ "\n" 
		++ "Maior        " ++ show(maior 12) ++ "\n"
		++ "Menor        " ++ show(menor 12) ++ "\n"
		++ "Venda Zerada " ++ show (vendazero 12) ++ "\n"

