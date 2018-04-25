####Calculo da cobertura vegetal####
#Afazeres: i) rotina para calculo da cobertura vegetal, ii) indexação dos valores de cobertura na planilha de dados geral
setwd("~/Documents/dados/mestrado/simulacao/matriz_trinaria")

require(reshape2)
require(magrittr)
require(plyr)

###Rotina para calculo da coberta vegetal###

mat_tri <- Sys.glob("*.txt")
mat_tri_index <- gsub("[^0-9]","",mat_tri) %>% as.numeric
df_mat_tri <- data.frame(refID = mat_tri_index , arquivos = mat_tri, cobertura = rep(NA, length(mat_tri)))
df_mat_tri <- df_mat_tri[order(df_mat_tri$refID),]

for(i in 1:dim(df_mat_tri)[1]){
  mat <- mat_tri[i] %>%  read.csv(. , head = FALSE, sep = " ") %>% as.matrix
  df_mat_tri[i,3] <- 1-(length(mat[mat==0])/length(as.vector(mat))) ###eu prefiro contar o numero de 0s pois esse método funciona com mat_bi e mat_tri
}