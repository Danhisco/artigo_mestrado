#### Função Aggregation Index ####
#ref: He, H. S., DeZonia, B. E., & Mladenoff, D. J. (2000). An aggregation index (AI) to quantify spatial patterns of landscapes. Landscape Ecology, 15(7), 591-601.
#objetivo: calcular o indice de agregação que estima a porcentagem de agregação observada, dado uma agregação máximo inferida a partir de uma aproximação em forma de quadrado
#INPUT: arquivo txt com a matriz trinária
#OUTPUT: indice de agregação do habitat (AI)
aggregation_index <- function(X){
  janela <- matrix(data = c(0,0,0,0,1,1,0,1,0), nrow = 3) #elementos a serem considerados no calculo
  matriz_binaria <- as.matrix(read.table(file = X, header = FALSE, sep = " ")) # txt -> matrix_trinaria
  matriz_binaria[matriz_binaria==2] <- 1 #matrix_trinaria -> matrix_binaria
  mat_temp <- matrix(0,ncol = ( dim(matriz_binaria)[1]+2) , nrow = ( dim(matriz_binaria)[2]+2)) #criando o 'pad'
  mat_temp[-c(1,dim(mat_temp)[1]),-c(1,dim(mat_temp)[2])] <- matriz_binaria #colocando a matriz de interesse na posição central do 'pad'
  for(i in 2:(nrow(matriz_binaria)+1)){
    for(j in 2:(ncol(matriz_binaria)+1)){
      mat <- mat_temp[(i-1):(i+1),(j-1):(j+1)]*janela #a ideia geral é igual da função 'focal'
      if(mat[5]==1) { #se o elemento central for 1 então conta-se o número de 1s menos o elemento central = número de lados compartilhados por 1 cosp (por habitat)
        mat_temp[i,j] <- (length(mat[mat==1]) - 1) # cada elemento recebe o número de 1s que estão abaixo e a esquerda do elemento central
      } else {
        mat_temp[i,j] <- 0
      }
    }
  }
  e_i.i <- sum(mat_temp) #total de lados adjacente a coespecificos i 
  Ai <- length(matriz_binaria[matriz_binaria == 1]) #area total da classe i
  n <- floor(sqrt(Ai)) #"side of the largest integer square smallher than Ai"
  m = Ai - n^2 #area que sobra
  if(m < n) { 
    max.e_i.i <- 2 * n * (n - 1) + 2 * m - 1 #largest number of shared edges for class i
    AI <- e_i.i/max.e_i.i #indice de agregação
    return(AI)
  } else {
    max.e_i.i <- 2 * n * (n - 1) + 2 * m - 2 #largest number of shared edges for class i
    AI <- e_i.i/max.e_i.i #indice de agregação
    return(AI)
  }
}
#o indice é dado pelo número total de lados compartilhados por coespecificos divido pela agregação máxima possível
#esse indice considera apenas os vizinhos imediatos, aqueles coespecificos presentes na diagonal não são considerados