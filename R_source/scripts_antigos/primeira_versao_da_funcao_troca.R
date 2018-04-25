
m <- matrix(data = sample(c(1,0), size = 900,replace = TRUE,prob = c(0.90,0.10)), ncol = 30)
Nobs <- 50

area_simulada <- function(matriz, obs){
#determinar a linha central
  if(dim(m)[1] %% 2 == 0) {
    l <- dim(m)[1]/2
  }else{
    l <- (dim(m)[1]+1)/2
  }
#determinar a coluna central
  if(dim(m)[2] %% 2 == 0) {
    c <- dim(m)[2]/2
  }else{
    c <- (dim(m)[2]+1)/2
  }
#trocar ou nao o elemento central 
  if( m[l,c] == 1 ){ m[l,c] <- 2 }

i = 1 #primeiro valor do marcador do while, tem que ficar fora do while
  while(length(m[m==2]<=N)){ #para preencher 
  x=0 #marcador do repeat dentro dos ifs
    if (i %% 2 != 0) { #se i for impar:
  repeat{ 
    # 1 unidade para a direita
    c <- c+1 #mudando a posicao do elemento
    if(m[l,c]==1) {m[l,c] <- 2} #trocando ou nao o elemento da matrix
    #controle do repeat
    x <- x+1
    if(x==i){break}
  }
  repeat{ 
    # 1 unidade para cima
    l <- l-1 #mudando a posicao do elemento
    if(m[l,c]==1) {m[l,c] <- 2} #trocando ou nao o elemento da matrix
    #controle do repeat
    x <- x+1
    if(x==i){break}
  }
} else { #ou se i for par:
  repeat{ 
    # 1 unidade para a esquerda
    c <- c-1 #mudando a posicao do elemento
    if(m[l,c]==1) {m[l,c] <- 2} #trocando ou nao o elemento da matrix
    #controle do repeat
    x <- x+1
    if(x==i){break}
  }
  repeat{ 
    # 1 unidade para baixo
    l <- l+1 #mudando a posicao do elemento
    if(m[l,c]==1) {m[l,c] <- 2} #trocando ou nao o elemento da matrix
    #controle do repeat
    x <- x+1
    if(x==i){break}
  }
}
  i <- i+1 #marcador associado com o while loop
  }
}


area_simulada(matriz = m_teste, obs = Nobs)
