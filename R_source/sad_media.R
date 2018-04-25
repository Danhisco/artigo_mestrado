###funcao que pega uma matriz contendo a identidade de cada indivíduo 
##input: matriz cujas linhas são o resultado de uma simulacao: são sempre N elementos (igual o número de 2s da matriz trinária), cada elemento é a identidade do indivíduo (ou seja, sua sp)
##output: um vector que contem a SAD média de todas as simulacoes
#riqueza média - precisa implementar!
##descrição:
##o input é uma matrix que contemm o resultado bruto da simulação, cada linha da simulação é uma replica, cada coluna é uma posição da matrix trinária que antes estava ocupada por '2's



sad_media <- function(x) {
  mat_sad_sim <- x
  sad_media_list <- vector("list", dim(mat_sad_sim)[1])

  for(i in 1:dim(mat_sad_sim)[1]){
    sad_media_list[[i]] <- mat_sad_sim[i,] %>% table %>% as.vector %>% sort(.,decreasing = TRUE)
  }
  
  a <- summary(sad_media_list)  %>% as.data.frame
  mat_zeros <- matrix(0, nrow = length(sad_media_list), ncol = a$Freq %>% levels %>% as.numeric %>% max(.,na.rm=TRUE) )

  for(j in 1:(sad_media_list %>% length) ){
    mat_zeros[j,1:(sad_media_list[[j]] %>% length) ] <- sad_media_list[[j]]
  }
  sad_media_sim <- apply(mat_zeros, 2, mean) %>% sort(.,decreasing = TRUE)
  return(sad_media_sim)
}