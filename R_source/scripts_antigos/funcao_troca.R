
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
while(length(m[m==2])<=N){ #trocar os 1 por 1 até completar 
  if (i %% 2 != 0) { #se i for impar:
    x=0 #marcador do repeat dentro dos ifs
    repeat{ 
      # 1 unidade para a direita
      c <- c+1 #mudando a posicao do elemento
      if(m[l,c]==1) {m[l,c] <- 2} #trocando ou nao o elemento da matrix
      #controle do repeat
      x <- x+1
      if(x==i | length(m[m==2]) == N){break}
    }
    x=0 #marcador do repeat dentro dos ifs
    repeat{ 
      # 1 unidade para cima
      l <- l-1 #mudando a posicao do elemento
      if(m[l,c]==1) {m[l,c] <- 2} #trocando ou nao o elemento da matrix
      #controle do repeat
      x <- x+1
      if(x==i | length(m[m==2]) == N){break}
    }
  } else { #ou se i for par:
    x=0 #marcador do repeat dentro dos ifs
    repeat{ 
      # 1 unidade para a esquerda
      c <- c-1 #mudando a posicao do elemento
      if(m[l,c]==1) {m[l,c] <- 2} #trocando ou nao o elemento da matrix
      #controle do repeat
      x <- x+1
      if(x==i | length(m[m==2]) == N){break}
    }
    x=0 #marcador do repeat dentro dos ifs
    repeat{ 
      # 1 unidade para baixo
      l <- l+1 #mudando a posicao do elemento
      if(m[l,c]==1) {m[l,c] <- 2} #trocando ou nao o elemento da matrix
      #controle do repeat
      x <- x+1
      if(x==i | length(m[m==2]) == N){break}
    }
  }
  i <- i+1 #marcador associado com o while loop
}

m <- matrix(data = sample(c(1,0), size = 400, replace = TRUE,prob = c(0.90,0.10)), ncol = 20)
N <- 10

length(m[m==2])
