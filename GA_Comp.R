
setClass(
  "Produto",
  slots = c(
    nome = "character",
    preco = "numeric",
    espaco = "numeric"
  )
)

setClass(
  "Individuo",
  slots = c(
    espacos = "numeric",
    precos = "numeric",
    limiteEspaco = "numeric",
    notaAvaliacao = "numeric",
    espacoOcupado = "numeric",
    geracao = "numeric",
    cromossomo = "character"
  ),
  
  prototype = list(
    espacos = 0,
    precos = 0,
    limiteEspaco = 0,
    notaAvaliacao = 0,
    espacoOcupado = 0,
    geracao = 0
  )
)

setClass(
  "AlgoritmoGenetico",
  slots = c(
    tamanhoPopulacao = "numeric",
    populacao = "list",
    geracao = "numeric",
    melhorIndividuo = "Individuo"
  )
)

gerarCromossomo = function(tamanhoCromossomo)
{
  cromossomo = sample(x = c("0", "1"), size = tamanhoCromossomo, replace = TRUE)
  return (cromossomo)
}

mutacao = function(individuo, taxaMutacao)
{
  for(i in 1:length(individuo@cromossomo))
  {
    if(taxaMutacao > runif(n = 1, min = 0, max = 1))
    {
      if(individuo@cromossomo[i] == '1')
        individuo@cromossomo[i] = '0'
      else
        individuo@cromossomo[i] = '1'
    }
  }
  return(individuo)
}

avaliacao = function(individuo)
{
  nota = 0
  tamanhoUtilizado = 0
  for(i in 1:length(individuo@cromossomo))
  {
    if(individuo@cromossomo[i] == '1')
    {
        nota = nota + individuo@precos[i]
        tamanhoUtilizado = tamanhoUtilizado + individuo@espacos[i]
    }
  }

  if(tamanhoUtilizado > individuo@limiteEspaco)
    nota = 1
  
  individuo@notaAvaliacao = nota
  individuo@espacoOcupado = tamanhoUtilizado
  
  return(individuo)
}

crossover = function(individuo1, individuo2)
{
  tamanhoMax = length(individuo1@cromossomo)
  indices = 1:tamanhoMax
  corte = sample(x = indices, size = 1)
  
  if(corte == tamanhoMax)
  {
    filho1Cromo = individuo2@cromossomo[1:corte]
    filho2Cromo = individuo1@cromossomo[1:corte]
  }
  else
  {
    filho1Cromo = c(individuo2@cromossomo[1:corte], individuo1@cromossomo[(corte+1):tamanhoMax])
    filho2Cromo = c(individuo1@cromossomo[1:corte], individuo2@cromossomo[(corte+1):tamanhoMax])
  }
  
  filhos = list(
    new("Individuo",
        espacos = individuo1@espacos,
        precos = individuo1@precos,
        limiteEspaco = individuo1@limiteEspaco,
        geracao = individuo1@geracao + 1),
    new("Individuo",
        espacos = individuo1@espacos,
        precos = individuo1@precos,
        limiteEspaco = individuo1@limiteEspaco,
        geracao = individuo1@geracao + 1)
  )
  
  filhos[[1]]@cromossomo = filho1Cromo
  filhos[[2]]@cromossomo = filho2Cromo
  
  return(filhos)
}

inicializaPopulacao = function(algoGenetico, espacos, precos, limiteEspaco)
{
   for(i in 1:algoGenetico@tamanhoPopulacao)
   {
     algoGenetico@populacao[[i]] = new("Individuo", espacos = espacos, precos = precos, limiteEspaco = limiteEspaco)
     algoGenetico@populacao[[i]]@cromossomo = gerarCromossomo(length(espacos))
   }
  
  return (algoGenetico)
}

ordenaPopulacao = function(populacao)
{
  populacaoOrdenada = c()
  notasAvaliacao = c()
  for(individuo in populacao)
  {
    notasAvaliacao = c(notasAvaliacao, individuo@notaAvaliacao)
  }
  listaPosicao = order(notasAvaliacao, decreasing = TRUE)
  
  for(i in 1:length(listaPosicao))
  {
    populacaoOrdenada = c(populacaoOrdenada, populacao[[listaPosicao[i]]])
  }
  return(populacaoOrdenada)
}

achaMelhorIndividuo = function(individuo, ag)
{
  if(individuo@notaAvaliacao > ag@melhorIndividuo@notaAvaliacao)
    ag@melhorIndividuo = individuo
  
  return(ag)
}

somaAvaliacoes = function(ag)
{
  soma = 0
  for(i in 1:ag@tamanhoPopulacao)
    soma = soma + ag@populacao[[i]]@notaAvaliacao
  return(soma)
}

selecionaPai = function(ag, somaAvaliacoes)
{
  indicePai = 0
  valorSorteado = runif(n = 1, min = 0, max = 1) * somaAvaliacoes
  soma = 0
  i = 1
  while(i < length(ag@populacao) && soma < valorSorteado)
  {
    soma = soma + ag@populacao[[i]]@notaAvaliacao
    indicePai = indicePai + 1
    i = i +  1
  }
  return (indicePai)
}

visualizaGeracao = function(algoritmoGenetico)
{
  melhor = algoritmoGenetico@populacao[[1]]#A população está ordenada
  #cat("\nG:", melhor@geracao, " Valor: ", melhor@notaAvaliacao,
  #    " Espaço utilizado: ", melhor@espacoOcupado, " Cromossomo: ", melhor@cromossomo)
  soma = somaAvaliacoes(algoritmoGenetico) / algoritmoGenetico@tamanhoPopulacao
  cat(melhor@notaAvaliacao, soma, "\n")
}

resolverAG = function(algoritmoGenetico, taxaMutacao, numeroGeracoes, espacos, precos, limiteEspaco)
{
  ag = algoritmoGenetico
  ag = inicializaPopulacao(ag, espacos, precos, limiteEspaco) #Inicializa a população
  
  for(i in 1:length(ag@populacao)) #Avalia a população
  {
      ag@populacao[[i]] = avaliacao(ag@populacao[[i]])
  }
  
  ag@populacao = ordenaPopulacao(ag@populacao)
  ag = achaMelhorIndividuo(ag@populacao[[1]], ag) #Definição do melhor indivíduo
  
  visualizaGeracao(ag) #Visualizar a geração atual
  
  for(geracao in 1:numeroGeracoes)
  {
    soma = somaAvaliacoes(ag) #Soma todas as avaliações
    
    novaPopulacao = c() #Cria nova população
    for(i in 1:(ag@tamanhoPopulacao/2))
    {
      pai1 = selecionaPai(ag, soma) #Índices dos pais
      pai2 = selecionaPai(ag, soma)

      filhos = crossover(ag@populacao[[pai1]], ag@populacao[[pai2]])

      filhos[[1]] = mutacao(filhos[[1]], probabilidadeMutacao)
      filhos[[2]] = mutacao(filhos[[2]], probabilidadeMutacao)

      novaPopulacao = c(novaPopulacao, filhos[[1]])
      novaPopulacao = c(novaPopulacao, filhos[[2]])
    }
    
    ag@populacao = novaPopulacao #Descarta a população antiga
    
    for(i in 1:ag@tamanhoPopulacao) #Avalia a nova população
    {
      ag@populacao[[i]] = avaliacao(ag@populacao[[i]])
    }
    
    ag@populacao = ordenaPopulacao(ag@populacao)
    visualizaGeracao(ag) #Visualizar a geração atual
    ag = achaMelhorIndividuo(ag@populacao[[1]], ag) #Acha o melhor indivíduo da população
    
  }
  
  cat("\nMelhor solução - G: ", ag@melhorIndividuo@geracao, " Valor: ", ag@melhorIndividuo@notaAvaliacao,
      " Espaco utilizao: ", ag@melhorIndividuo@espacoOcupado, " Cromossomo: ", ag@melhorIndividuo@cromossomo)
  return(ag)
}


listaProdutos = c(new("Produto", nome = "1", espaco = 220, preco = 80),
                  new("Produto", nome = "2", espaco = 208, preco = 82),
                  new("Produto", nome = "3", espaco = 198, preco = 85),
                  new("Produto", nome = "4", espaco = 192, preco = 70),
                  new("Produto", nome = "5", espaco = 180, preco = 72),
                  new("Produto", nome = "6", espaco = 180, preco = 70),
                  new("Produto", nome = "7", espaco = 165, preco = 66),
                  new("Produto", nome = "8", espaco = 162, preco = 50),
                  new("Produto", nome = "9", espaco = 160, preco = 55),
                  new("Produto", nome = "10", espaco = 158, preco = 25),
                  new("Produto", nome = "11", espaco = 155, preco = 50),
                  new("Produto", nome = "12", espaco = 130, preco = 55),
                  new("Produto", nome = "13", espaco = 125, preco = 40),
                  new("Produto", nome = "14", espaco = 122, preco = 48),
                  new("Produto", nome = "15", espaco = 120, preco = 50),
                  new("Produto", nome = "16", espaco = 118, preco = 32),
                  new("Produto", nome = "17", espaco = 115, preco = 22),
                  new("Produto", nome = "18", espaco = 110, preco = 60),
                  new("Produto", nome = "19", espaco = 105, preco = 30),
                  new("Produto", nome = "20", espaco = 101, preco = 32),
                  new("Produto", nome = "21", espaco = 100, preco = 40),
                  new("Produto", nome = "22", espaco = 100, preco = 38),
                  new("Produto", nome = "23", espaco = 98, preco = 35),
                  new("Produto", nome = "24", espaco = 96, preco = 32),
                  new("Produto", nome = "25", espaco = 95, preco = 25),
                  new("Produto", nome = "26", espaco = 90, preco = 28),
                  new("Produto", nome = "27", espaco = 88, preco = 30),
                  new("Produto", nome = "28", espaco = 82, preco = 22),
                  new("Produto", nome = "29", espaco = 80, preco = 50),
                  new("Produto", nome = "30", espaco = 77, preco = 30),
                  new("Produto", nome = "31", espaco = 75, preco = 45),
                  new("Produto", nome = "32", espaco = 73, preco = 30),
                  new("Produto", nome = "33", espaco = 72, preco = 60),
                  new("Produto", nome = "34", espaco = 70, preco = 50),
                  new("Produto", nome = "35", espaco = 69, preco = 20),
                  new("Produto", nome = "36", espaco = 66, preco = 65),
                  new("Produto", nome = "37", espaco = 65, preco = 20),
                  new("Produto", nome = "38", espaco = 63, preco = 25),
                  new("Produto", nome = "39", espaco = 60, preco = 30),
                  new("Produto", nome = "40", espaco = 58, preco = 10),
                  new("Produto", nome = "41", espaco = 56, preco = 20),
                  new("Produto", nome = "42", espaco = 50, preco = 25),
                  new("Produto", nome = "43", espaco = 30, preco = 15),
                  new("Produto", nome = "44", espaco = 20, preco = 10),
                  new("Produto", nome = "45", espaco = 15, preco = 10),
                  new("Produto", nome = "46", espaco = 10, preco = 10),
                  new("Produto", nome = "47", espaco = 8, preco = 4),
                  new("Produto", nome = "48", espaco = 5, preco = 4),
                  new("Produto", nome = "49", espaco = 3, preco = 2),
                  new("Produto", nome = "50", espaco = 1, preco = 1)
)

espacos = c()
valores = c()
nomes = c()
for (produto in listaProdutos) { #******************************************************** MUDEI AQUI ***********
  espacos = c(espacos, produto@preco)
  valores = c(valores, produto@espaco)
  nomes = c(nomes, produto@nome)
}
limite = 1000
tamanho = 100
probabilidadeMutacao = 0.05
numeroGeracoes = 200

ag = new("AlgoritmoGenetico", tamanhoPopulacao = tamanho) #Cria a população

ag = resolverAG(ag, probabilidadeMutacao, numeroGeracoes, espacos, valores, limite)

for(i in 1:length(listaProdutos))
{
  if(ag@melhorIndividuo@cromossomo[[i]] == '1')
    cat("\nNome: ", listaProdutos[[i]]@nome, " Valor: ", valores[i])
}