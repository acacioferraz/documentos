module Relatorio where
import System.IO

--Universidade Estadual do Sudoesta da Bahia
--Curso Ciência da Computação
--Programação Declarativa
--Trabalho da 2º Unidade - Relatorio de Vendas em Haskell
--Data Inicio: 28/10/2020 - Data Entrega: 13/11/2020

--Alunos: Acácio Ferraz, Adrian, Ademilson, Alberto, Alef

tamanhoLinha :: Int
tamanhoLinha = 60

--cabecalho do relatorio
cabecalho :: String
cabecalho = imprimir tamanhoLinha " " ++ "\n" ++ 
			imprimirTitulo ++ "\n" ++
			imprimir tamanhoLinha " " ++ "\n" ++
			titulo2 ++ "\n"

--imprime o titulo do arquivo
imprimirTitulo :: String
imprimirTitulo = imprimir quantidade " " ++ titulo ++ imprimir quantidade " " 
	where
		quantidade :: Int
		quantidade = div (tamanhoLinha - (length titulo)) 2

--string que contem o titulo
titulo :: String
titulo = "Relatorio de Vendas"

titulo2 :: String
titulo2 = "Mes                        Quantidade                   R$ "

--imprime uma sequencia de qualquer caracter
imprimir :: Int -> String -> String
imprimir 0 esp = " "
imprimir n esp = esp ++ imprimir(n-1) esp

--imprime o corpo do projeto com os meses valores e quantidade de vendas correspondentes a cada mes
corpo :: Int -> String 
corpo n = imprimirLinhas n

--funcao que retorna a quantidade de vendas correspondentes ao mes
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

-- função que calcula se houve alguns mes com zero de vendas
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

-- imprime o mes que corresponde ao numero 1 - 12
imprimemes :: Int -> String
imprimemes n = impmes n 
	where
		impmes n
			| n == 12 = show (mes n) ++ "\n"
			| otherwise = show (mes n) ++ "\n" ++ impmes (n+1)

-- numero referente ao mes
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
	| otherwise = ""

valor :: Int
valor = 12

-- calcula o tamanho da string no caso o nome do mes
tamanho :: String -> Int
tamanho "" = 0
tamanho (a:b) = 1 + tamanho b 

--imprime a linha tabulada com nome do mes, numero de vendas e total em R$
imprimirLinha :: Int -> String
imprimirLinha n = mes n ++ imprimir (30 - tamanho (mes n)) " " 
				++ show (vendasMes n) 
				++ imprimir 20 " " ++ "R$ " 
				++ show (valor*(vendasMes n)) ++ ",00" ++ "\n"

-- imprime na forma recursiva a quantidade de meses solicitada
imprimirLinhas :: Int -> String
imprimirLinhas n 
	|n == 1 = imprimirLinha 1
	|otherwise =  imprimirLinhas(n-1) ++ imprimirLinha(n)

-- imprime o rodape com valores do balanço anual tabulado

rod1 :: String
rod1 = "Total"

rod2 :: String
rod2 = "Maior"

rod3 :: String
rod3 = "Menor"

rod4 :: String
rod4 = "Venda Zerada"

rod5 :: String
rod5 = "Preço do Produto"

rodape :: String
rodape = "\n" ++ rod1 ++ imprimir (30 - tamanho (rod1)) " " ++ show total ++ imprimir 20 " " ++ "R$ " ++ show (total*12) ++ ",00"++ "\n" 
		++ rod2 ++ imprimir (30 - tamanho (rod2)) " "  ++ show(maior 12) ++ "\n"
		++ rod3 ++ imprimir (30 - tamanho (rod3)) " "  ++ show(menor 12) ++ "\n"
		++ rod4 ++ imprimir (30 - tamanho (rod4)) " "  ++ show (vendazero 12) ++ "\n" ++ "\n"
		++ rod5 ++ imprimir (30 - tamanho (rod5)) " "  ++ show valor ++ ",00" ++ "\n\n" 

--função para impressão do relatorio de vendas com cabeçalho, corpo e rodape
relatorio :: Int -> IO()
relatorio n = putStr ((cabecalho) ++ (corpo n) ++ (rodape))



