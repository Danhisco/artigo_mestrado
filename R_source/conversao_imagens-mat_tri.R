##### Conversão das imagens ajustadas em matrix trinária #####
# Como não houve troca de rótulos durante o processamento das imagens (ver auditoria_imagens em R_source/ e os arquivos em pdf na pasta auditoria_imagens/),
# eu não vou modificar o resto do script, vou apenas mudar o necessário - a forma como obtemos a matriz binária 
### Código original ###
library(gtools)
library(raster)
library(magrittr)
library(plyr)
library(dplyr)
source("/home/danilo/Documents/dissertacao/R_source/area_simulada_beta.R")
setwd("~/Documents/dissertacao/dados/imagens/paisagens_selecionadas/imagens_ajustadas/")

paisagens_ajustadas <- Sys.glob("*.png") #paisagens ajustadas
gsub("[^\\d]+", "", ., perl=TRUE) #exemplo de como retirar a porção númerica de um vetor de letras
paisagens_ajustadas %<>% data.frame(ordem = as.numeric(gsub("[^0-9]", "", ., perl=TRUE)), 
                                    png.file = .) #transformando em df para o inner_join
paisagens_ajustadas$png.file %<>% levels %>% as.character

#Filtrando os 93 refID
load("~/Documents/dissertacao/dados/imagens/paisagens_selecionadas/simulacao_all_ranges_94.Rdata")
load("~/Documents/dissertacao/dados/imagens/paisagens_selecionadas/resultados_DaniloPMori.Rdata")
rm(df_dispersal, lista_dispersal, lista_SAD.sim, df_U.est)
df_simulacao %>% select(refID, ordem, SiteCode) %>% inner_join(., y = paisagens_ajustadas, by = "ordem") -> df_conversao #informações para a conversão
#função para converter arquivo raster em matrix trinária
  
############################# TESTE func_matrix.R #############################
load("~/Documents/dissertacao/dados/imagens/paisagens_selecionadas/simulacao_all_ranges_94.Rdata")
load("~/Documents/dissertacao/dados/imagens/paisagens_selecionadas/resultados_DaniloPMori.Rdata")
rm(df_dispersal, lista_dispersal, lista_SAD.sim, df_U.est)
paisagens_ajustadas <- Sys.glob("~/Documents/dissertacao/dados/imagens/paisagens_selecionadas/imagens_ajustadas/*.png") #paisagens ajustadas
paisagens_ajustadas <- data.frame(ordem = sapply(paisagens_ajustadas,function(x) as.numeric(gsub("[^0-9]", "", x, perl=TRUE)) ), png.file = paisagens_ajustadas)
paisagens_ajustadas$png.file %<>% levels %>% as.character
#paisagens_ajustadas <- as.data.frame(sapply(paisagens_ajustadas, function(x) gsub("/home/danilo/Documents/dissertacao/dados/imagens/paisagens_selecionadas/imagens_ajustadas/","",x)))
paisagens_ajustadas$ordem %<>% as.character %>% as.numeric
paisagens_ajustadas$png.file %<>% as.character
paisagens_ajustadas %<>% arrange(ordem)

load("~/Documents/dissertacao/dados/imagens/paisagens_selecionadas/df_i.conversao.Rdata")
df_simulacao %>% select(refID, ordem, SiteCode) -> df_conversao #informações para a conversão
df_conversao %<>% inner_join(., y = paisagens_ajustadas, by = "ordem") #colocando a origem
df_conversao %<>% inner_join(.,y = df_i.conversao, by = c("refID","ordem","SiteCode"))
df_conversao %>% str
#system("ls *.png")
df_conversao %>% select(SiteCode) %>% table %>% as.data.frame %>% filter(Freq == 1) %>% dim #todos os refIDS tem apenas 1 valor 
df_conversao %>% mutate(txt.file = daply(.,"SiteCode", function(x) func_mat.tri(png = x$png.file, abund = x$N)) ) -> a
df_conversao %>% mutate(txt.file = d_ply(.,"SiteCode", function(x) func_mat.tri(png = x$png.file, abund = x$N)) ) %>% head

#acredito que o problema está nos buracos do png. vou verificar como que essa implementação modificou o png
func_focal <- function(x) ifelse(length(x[x==1]) >= 7, 1, x[5]) 
janela <- matrix(1,3,3)
x11()
1mat_x <- matrix(getValues(raster_x)/255, ncol = raster_x@ncols)
raster_binario.x <- matrix(nrow = nrow(mat_x), ncol = ncol(mat_x), sapply(mat_x, function(x) ifelse(x >= 0.7, 1, 0)) ) %>% raster
mat_bi_focal.x <- as.matrix(focal(raster_binario.x, janela, func_focal, pad=TRUE, padvalues = 0))
image(raster_binario.x)

  




















#################################### código original ####################################
#objetos para o for#
paisagens_zicadas <- rep("NA",length(paisagens_ajustadas)) #lista para as eventuais paisagens com algum problema
focal_function <- function(x) ifelse(length(x[x==1]) >= 5, 1, x[5]) #considerei que se 5/9 dos valores da matriz forem iguais a 1 então o valor central tem que ser 1. A partir de uma avaliação visual acredito que o objetivo de tapar os buracos foi feito sem levar a uma deformação da configuração espacial
janela <- matrix(1,3,3) #ajanela minima para usar a função focal do pacote raster


for( j in 1:length(paisagens_ajustadas)){
  
  paisagem <- raster(paisagens_ajustadas[1])
  mat_paisagem0 <- matrix(getValues(paisagem)/255, ncol = paisagem@ncols)
  mat_paisagem1 <- matrix(getValues(paisagem)/255, ncol = paisagem@ncols, byrow = TRUE)
  mat_paisagem2 <- as.matrix(paisagem)
  par(mfrow=c(2,2))
  par(mar = rep(1.2,4))
  image(paisagem, main = "original", xaxt ="n", yaxt="n")
  image(mat_paisagem0, main = "getValues byrow = FALSE", xaxt ="n", yaxt="n")
  image(mat_paisagem1, main = "getValues byrow = TRUE", xaxt ="n", yaxt="n")
  image(mat_paisagem2, main = "as.matrix(raster(paisagem))", xaxt ="n", yaxt="n")
  
  mat_bi <- matrix(ncol = ncol(mat_paisagem), 
                   nrow = nrow(mat_paisagem),
                   data = sapply(mat_paisagem, function(x) sample( c(1,0), 1, prob = c(x, 1-x) ) ) 
  )
  mat_bi_raster <- raster(mat_bi)
  mat_bi_focal <- as.matrix(focal(mat_bi_raster, janela, focal_function, pad=TRUE, padvalues = 0))
  
  individuos <- df_ajuste[df_ajuste$ordem == as.numeric(gsub("[^0-9]","",paisagens_ajustadas[j])), "N"]
  mat_tri <- try(area_simulada(matriz = mat_bi_focal, N = individuos))
  
  if(class(mat_tri) == "matrix"){
    try(write.table(x = mat_tri, 
                    file = paste("paisagem_sim_",gsub("[^0-9]","",paisagens_ajustadas[j]),".txt", sep = ""), 
                    sep = " ", row.names = FALSE, col.names = FALSE))
  }else{
    paisagens_zicadas[j] <- paisagens_ajustadas[j]
  }
}
