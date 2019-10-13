###Descrição da função:
#O algoritmo coalescente identifica 3 tipos de valores: 0,1,2. 0 = não habitat; 1 = habitat disponível; 2 = local de interesse (i.e. onde a simulação vai ocorrer)
#Após ajustar a resolução da imagem, possuo uma matrix com valores [0;100], converto esses valores para 1 e 0 usando um sample ponderado por esses valores.
#Isso resulta em uma matrix que em média possui tantos valores de 1 quanto de indivíduos na paisagem. Contudo, isso tambem gera fragmentos 'furados', com alguns 0s
#no centro dos fragmentos. Isso pode ser inconviente pois pelo menos na região onde existe 

area_simulada <- function(matriz, N){
  library(BioPhysConnectoR) 
  d <- ceiling(sqrt(N)/2) 
  l <- ceiling(dim(matriz)[1]/2) #eu uso o ceiling aqui pois eu garanto que funcione tanto com valores impares e pares de dimensão da matriz de entrada
  c <- ceiling(dim(matriz)[2]/2)
  m_temp <- matriz[(l-d):(l+d),(c-d):(c+d)] #defino a sub-matriz quadrada com lados impares; não há nenhum bom motivo para ser impar, mas se for par tem que mudar a primeira linha depois do for
  
  if(length(m_temp[m_temp==1]) < N) { #da maneira q eu fiz, a sub-matriz contem pelo menos dezenas de vezes mais elementos do N e como elas sempre estarão em locais onde devem ter pouquissimos zeros eu acho que não terei nenhum caso em que essa condição seja verdadeira
    stop("eh mestre, a sub-matriz tem que ser maior")
  } else if (length(m_temp[m_temp==1]) == N) { #caso a submatris tenha tantos elementos quantos '2's necessários
    m_temp[m_temp==1] <- 2
    matriz[(l-d):(l+d),(c-d):(c+d)] <- m_temp
    return(matriz)
  } else { #aqui estão os casos em que length(m_temp[m_temp==1]) > N. Minha primeira abordagem era preencher a planilha com 2 e fiz um for com ifs dentro - ficou muito pesado...essa abordagem ficou bem melhor
    #aqui eu primeiro organizo os elementos da sub-matriz na sequência que me interessa e ai depois substituo os elementos por 2.
    col_cresc <- which(m_temp==m_temp, arr.ind = T) #crio uma matriz com a posição de cada elemento da matriz, organizado pela coluna em ordem crescente
    col_decre <- col_cresc[dim(col_cresc)[1]:1,] #o mesmo de cima lido na ordem contrária
    row_cresc <- mat.sort(which(m_temp==m_temp, arr.ind = T),1,decreasing = F) #auto explicativo
    row_decre <- row_cresc[dim(col_cresc)[1]:1,] # idem
    ciclo <- (dim(m_temp)[1]-1)/2 #criei essa variável só para ficar menos poluído o for: note que assim eu não incluo a coluna central dentro do for, ela é incluida na linha que sucede o for
    l_mat_index <- list() #lista que vou usar dentro do for
    dim_temp <- dim(m_temp)[1]
    for(i in 1:ciclo){
        a1 <- col_cresc[col_cresc[,"col"]==i,] #considerando o primeiro ciclo: 1a coluna em ordem crescente
        b1 <- row_cresc[row_cresc[,"row"]==dim_temp+1-i,] #última linha em ordem crescente 
        c1 <- col_decre[col_decre[,"col"]==dim_temp+1-i,] #última coluna em ordem reversa
        d1 <- row_decre[row_decre[,"row"]==i,] #primeira linha em ordem reversa; os demais ciclos são com a segunda coluna, penúltima linha, penúltima coluna e segunda linha, etc  
      l_mat_index[[i]] <- do.call(rbind,list(a1,b1,c1,d1)) #ao final de cada ciclo eu concateno tudo em uma única matriz
    }
    l_mat_index[[(dim_temp+1)/2]] <- col_cresc[col_cresc[,"col"]==(dim_temp+1)/2,] #a coluna central é a última a entra, não importa a ordem dela nesse caso, ela sempre acrescente apenas 1 elemento. se dim(m_temp) for par então essa linha seria diferente
    
    mat_ref <- unique(do.call(rbind, l_mat_index)) #aqui pego e tiro as repetições. Obtenho uma sequência de elementos que descreve uma espiral quadrada convergente
    length_ref <- length(m_temp[mat_ref][m_temp[mat_ref]==1]) #variável para deixar a próxima indexação mais limpa
    m_temp[mat_ref][m_temp[mat_ref]==1][(1+length_ref-N):length_ref] <- 2 #contínua muito grande rs; aqui eu pego os N últimos elementos que são iguais a 1 e troco por 2
    matriz[(l-d):(l+d),(c-d):(c+d)] <- m_temp #substituo a matriz de volta e é isso ae
    return(matriz)
  }
}