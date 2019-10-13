###Descrição da função:
# Substitui N unidades de habitat comum (1) por unidades de habitat da comunidade local (2)
# inicia-se pela unidade de habitat central e então segue-se convertendo 1s em 2s segundo uma espiral quadrada divergente (?termo correto?)
f_area.simulada <- function(matriz, N){
#@ matriz :: objeto matriz que representa a posição da unidade de habitat (1) e de não habitat (2).
#@ 
  library(BioPhysConnectoR)
  # Janela de observação
  d <- ceiling(sqrt(N)*(3/4)) # metade do lado do janela de observação
  l <- ceiling(dim(matriz)[1]/2) # linha central da paisagem
  c <- ceiling(dim(matriz)[2]/2) # coluna central da paisagem
  # define uma janela central na paisagem onde o for sera aplicado
  m_temp <- matriz[(l-d):(l+d),(c-d):(c+d)]
  # a area da janela de observacao deve ser suficiente se a areaa amostrada pode ser aproximada por um quadrado
  if(length(m_temp[m_temp==1]) < N) { 
    stop("Não ha habitat suficiente na janela de observação")
  # tambem deve estar errado, pois janela de observacao ~ 2.25A_sitio
  } else if (length(m_temp[m_temp==1]) == N) { 
    # m_temp[m_temp==1] <- 2
    # matriz[(l-d):(l+d),(c-d):(c+d)] <- m_temp
    # return(matriz)
    stop("area amostral igual janela de observacao")
  # paisagens que devem estar adequadas para os métodos
  } else { 
    # posição de cada elemento da janela de observação, por coluna em ordem crescente
    col_cresc <- which(m_temp==m_temp, arr.ind = T)
    # idem na ordem contrária
    col_decre <- col_cresc[dim(col_cresc)[1]:1,]
    # posicao de cada elemento, por linha em ordem crescente
    row_cresc <- mat.sort(which(m_temp==m_temp, arr.ind = T),1,decreasing = F) 
    # idem ao controario
    row_decre <- row_cresc[dim(col_cresc)[1]:1,] 
    # (nrow - 1)/2; exclui a posição dos elementos da coluna central da janela de observação
    ciclo <- (dim(m_temp)[1]-1)/2 # 
    l_mat_index <- list() #lista que vou usar dentro do for
    dim_temp <- dim(m_temp)[1]
    for(i in 1:ciclo){
      a1 <- col_cresc[col_cresc[,"col"]==i,] #considerando o primeiro ciclo: 1a coluna em ordem crescente
      b1 <- row_cresc[row_cresc[,"row"]==dim_temp+1-i,] #última linha em ordem crescente 
      c1 <- col_decre[col_decre[,"col"]==dim_temp+1-i,] #última coluna em ordem reversa
      d1 <- row_decre[row_decre[,"row"]==i,] #primeira linha em ordem reversa; os demais ciclos são com a segunda coluna, penúltima linha, penúltima coluna e segunda linha, etc  
      l_mat_index[[i]] <- do.call(rbind,list(a1,b1,c1,d1)) #ao final de cada ciclo eu concateno tudo em uma única matriz
    }
    l_mat_index[[(dim_temp+1)/2]] <- col_cresc[col_cresc[,"col"]==(dim_temp+1)/2,] #a coluna central deve ser a última
    mat_ref <- unique(do.call(rbind, l_mat_index)) #remocao de repeticao. Obtenho uma sequência de elementos que descreve uma espiral quadrada convergente
    length_ref <- length(m_temp[mat_ref][m_temp[mat_ref]==1]) #variável para indexação:
    m_temp[mat_ref][m_temp[mat_ref]==1][(1+length_ref-N):length_ref] <- 2 #os N últimos elementos que são iguais a 1 e troco por 2
    matriz[(l-d):(l+d),(c-d):(c+d)] <- m_temp #substituo a matriz de volta
    return(matriz)
  }
}