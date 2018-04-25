#OBJETIVO: converter uma imagem png de tamanho ajustado para uma matrix trinária onde ocorre a simulação
#NPUT: um df que contem o nome do arquivo png e número de indivíduos observado/SiteCode - será usado com um mutate: 
#df_conversao[,c("SiteCode", "N", "png.file")] %>% mutate(txt.file = d_ply(.,"SiteCode", function(x) func_mat.tri(png = x$png.file, abund = x$abund)) ) 
#OUTPUT: escreve paisagem_sim_*.txt para cada ordem e retorno o nome do arquivo ou uma mensagem para verificar
func_mat.bi <- function(png){ #png.file, número de indivíduos presente 
  #individuos <- abund #número de indivíduos que irão entrar na paisagem
  func_focal <- function(x) ifelse(length(x[x==1]) >= 5, 1, x[5]) #considerei que se 5/9 dos valores da matriz forem iguais a 1 
  #então o valor central tem que ser 1 NOTA: rever isso aqui, mas 5/9 é um bom número.
  #A partir de uma avaliação visual acredito que o objetivo de tapar os buracos foi feito sem levar a uma deformação da configuração espacial
  janela <- matrix(1,3,3) #a dim(janela) minima para usar a função focal do pacote raster
  #NOTA: Se o valor central for igual a 1, então nada mudara, pois a função focal retorna 1 ou o valor central da janela. Se for zero, as possíveis configurações espaciais de ter 
  #3 zeros na matriz me parece ser justa para adicionar um valor 1 - esse é o tipo de coisa que me parece arbitrário. A comparação visual mostra que as coisas não mudam muito.
  raster <- raster(png) #png -> raster NOTA: tive problemas com as funções destinadas ao uso de .png, então optei por essa solução
  #NOTA: os valores vão de 0 a 255, excluindo os '0', os valores parecem tender a uma distribuição assimétrica para a direita (pixels com valores mais pŕoximos de 255)
  #coisas a fazer: verificar se o padao da distribuição se mantem entre os tifs e pngs
  mat <- matrix(getValues(raster)/255, ncol = raster@ncols) #matriz com os valores do png escalonados(Qual o nome correto?) para [0;1]
  raster_binario <- raster( matrix(nrow = nrow(mat), ncol = ncol(mat), sapply(mat, function(x) ifelse(x >= 0.7, 1, 0)) ) ) #critério do Renato Lima
  binario.focal <- as.matrix( focal(raster_binario, janela, func_focal, pad=TRUE, padvalues = 0)) #aplicando a função focal
  #mat_tri.x <- try(area_simulada(matriz = mat_bi_focal.x, N = individuos)) #aplicando area_simulada
  if(class(binario.focal) == "matrix"){ 
    try(write.table(x = binario.focal, 
                    file = gsub(".png",".txt", png), #mantenho o nome, mudo a extensão
                    sep = " ", row.names = FALSE, col.names = FALSE)) #o padrão da função coalescente
  }
}  