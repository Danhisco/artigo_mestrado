####Coisas para arrumar: quando o sub-matriz é pequena o que fazer?
                      
                      # iii) no for colocar um if com o critério de ter a sub-matriz com pelo menos o mesmo número de 1s
                      # iv) se não rolar pensar em aumentar a sub-matriz..

library(png)
require(raster)
source("~/Documents/dissertacao/R_source/area_simulada.R")

files_ajustado <- Sys.glob("~/Documents/dados/mestrado/imagens/imagens_ajustadas/*_landscape.png") #todos os pngs com resolucao ajustada
treeco <- read.table(file = "///home/danilo/treeco_paisagens_selecionadas.txt", header = T, sep = "\t", dec = ".") # lendo o treeco

treeco[treeco$refID == as.numeric(gsub("[^0-9]","",files_ajustado[2])),"N"]

mat_erro <- list()

for(i in 1:length(files_ajustado)) {
  paisagem <- raster(files_ajustado[i]) #chamando o png ajustado
  mat_lands <- matrix(getValues(paisagem), ncol = paisagem@ncols) #convertendo de raster para mat
  mat_lands_bi <- try(matrix(ncol = ncol(mat_lands), nrow = nrow(mat_lands), #transformando em matriz binaria
                         data = sapply(mat_lands/255, function(x) sample(c(1,0), 1, prob = c(x,1-x))))) #eu optei por sortear os valores da matriz
  
  mat_focal <-matrix(nrow = (paisagem@nrows+2), ncol = (paisagem@ncols+2)) #essa parte precisa ser implementada
  
  mat_lands_tri <- try(area_simulada(matriz = mat_lands_bi, N = treeco[treeco$refID == as.numeric(gsub("[^0-9]","",files_ajustado[i])),"N"])) #aqui eu uso a funcao area simulada
  if(class(mat_lands_tri) == "numeric"){
  mat_erro[[i]] <- mat_lands_tri
  names(mat_erro[[i]]) <- paste("landscape_error_", gsub("[^0-9]","",files_ajustado[[i]]), sep = "")
  } else {
  try(write.table(x = mat_lands_tri, file = paste("landscape_sim_",gsub("[^0-9]","",files_ajustado[i]),".txt",sep = ""), sep = " ",row.names = FALSE, col.names = FALSE))
  }
} 



####Auditoria
##começando de trás pra frente: 1) matriz trinária, 2) matriz binária, 3) matriz a partir do raster, 4) leitura do raster, 5) o raster

paisagens_mat <- Sys.glob("*.txt")
list_paisagens_mat <- list()
for(i in 1:length(paisagens_mat)){
  list_paisagens_mat[[i]] <- read.table(file = paisagens_mat[i], header = F, sep = " ")
  #image(list_paisagens_mat[[i]], main = paisagens_mat[i])
}
names(list_paisagens_mat) <- paisagens_mat
summary(list_paisagens_mat)


for(i in 1:length(list_paisagens_mat)) {
  if(length((list_paisagens_mat[[i]])) != 1) {
    image(as.matrix(list_paisagens_mat[[i]]), main = paisagens_mat[i])
  }
}

##O problema não está na matriz trinária, é anterior
list_mat_bi <- list()
for(i in 1:length(files_ajustado)) {
  paisagem <- try(raster(files_ajustado[i]))
  mat_lands <- try(matrix(getValues(paisagem), ncol = paisagem@ncols))
  list_mat_bi[[i]] <- try(matrix(ncol = ncol(mat_lands), nrow = nrow(mat_lands),
                             data = sapply(mat_lands/255, function(x) sample(c(1,0), 1, prob = c(x,1-x)))))
}
names(list_mat_bi) <- paste("mat_bi_", gsub("[^0-9]","",files_ajustado), sep = "")
summary(list_mat_bi)

for(i in 1:length(list_mat_bi)) {
  if(length((list_mat_bi[[i]])) != 1) {
    image(as.matrix(list_mat_bi[[i]]), main = names(list_mat_bi)[i])
  }
}

##Com excecao do refID_1028 todos os refIDs que estavam zicando eram por conta que a sub-matriz era muito pequena. 
##O que eu posso fazer é usar a funcao 'focal' do pacote raster, essa funcao preenche celulas baseado nas células vizinhas e alternativamente eu posso aumentar a sub-matriz...
##O refID_1028 parece que tem algum problema na hora de recortar o raster, já como imagem bruta ele deu problema; no google maps esse ponto está totalmente no mar ¬¬
##Para garantir que tudo vai funfar vou usar a funcao 'focal' e vou aumentar a sub-matriz.
