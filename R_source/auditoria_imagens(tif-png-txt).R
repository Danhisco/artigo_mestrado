########## Auditoria Imagens ##########
#I) tif -> png ajustado
#II) png ajustado -> matriz trinaria
#II.a) implementar nova maneira de selecionar 0s e 1s

#library(raster)
#library(magrittr)
#library(plyr)
#library(dplyr)

# *.tif #
#tif.file <- Sys.glob("*.tif")
#separando em três partes: refID, estado, ordem
#tif.file %<>% sapply(., function(x) unname( unlist( strsplit(x, "_", fixed = TRUE) ) ) )
#transformando em df[,c("refID", "ordem", "file_name")]
#tif.file <- data.frame(row.names = NULL,
#                       refID = unname(sapply(unname(tif.file[1,]), function(x) gsub("ref","",x))), #extraindo o padrão numerico do refID
#                       ordem = unname(sapply(unname(tif.file[3,]), function(x) gsub(".tif","",x))), #extraindo o padrão numerico da ordem
#                      tif.file = colnames(tif.file)) #nome do arquivo
#tif.file %<>% arrange(ordem)

# *.png #
#png.file <- Sys.glob("*.png")
#separando em três partes: refID, estado, ordem
#png.file %<>% sapply(., function(x) unname( unlist( strsplit(x, "_", fixed = TRUE) ) ) )
#transformando em df[,c("refID", "ordem", "file_name")]
#png.file <- data.frame(row.names = NULL,
#                       refID = unname(sapply(unname(png.file[1,]), function(x) gsub("ref","",x))), #extraindo o padrão numerico do refID
#                       ordem = unname(sapply(unname(png.file[3,]), function(x) gsub(".png","",x))), #extraindo o padrão numerico da ordem
#                       png.file = colnames(png.file)) #nome do arquivo
#png.file %<>% arrange(ordem)

# *.tif #
#txt.file <- Sys.glob("*.txt")
#separando em três partes: refID, estado, ordem
#txt.file %<>% sapply(., function(x) unname( unlist( strsplit(x, "_", fixed = TRUE) ) ) )
#transformando em df[,c("refID", "ordem", "file_name")]
#txt.file <- data.frame(row.names = NULL,
#                       refID = unname(sapply(unname(txt.file[1,]), function(x) gsub("ref","",x))), #extraindo o padrão numerico do refID
#                       ordem = unname(sapply(unname(txt.file[3,]), function(x) gsub(".txt","",x))), #extraindo o padrão numerico da ordem
#                       txt.file = colnames(txt.file)) #nome do arquivo
#txt.file %<>% arrange(ordem)

#tif.file %>% inner_join(., y = png.file, by = c("refID", "ordem")) %>% inner_join(., y = txt.file, by = c("refID", "ordem")) -> df_auditoria.imagens

#Comparação visual#
x11()
par(mfrow = c(1,3))
for(i in 98){ #para cada uma das 98 paisagens
  image(raster(df_simulacao[i,7]), main = df_simulacao[i,7], axes = FALSE) #tif
  image(t(as.matrix(raster(df_simulacao[i,8]))), main = df_simulacao[i,8], axes = FALSE) #png
  image(as.matrix(read.table(df_simulacao[i,9], header = FALSE, sep = " ")), main = df_simulacao[i,9], axes = FALSE) #txt
}


# ou se cria arquivos para visualização externa
#x11()
for(i in 1:94){ #para cada uma das 94 paisagens NOTA: tem que tirar o refID que não tem .txt ai fica 93
  pdf(file = gsub(".tif",".pdf",df_simulacao[i,7]), width = 11.69*0.75, height = 8.30*0.75)
  par(mfrow = c(1,3))
  image(raster(df_simulacao[i,7]), main = df_simulacao[i,7], axes = FALSE)
  image(t(as.matrix(raster(df_simulacao[i,8]))), main = df_simulacao[i,8], axes = FALSE)
  image(as.matrix(read.table(df_simulacao[i,9], header = FALSE, sep = " ")), main = df_simulacao[i,9], axes = FALSE)
  dev.off()
}

pdf(file = paste(df_temp[1,1],".pdf", sep = ""), width = 11.69*0.75, height = 8.30*0.75)
par(mfrow = c(1,3))
image(raster(df_temp[1,6]), main = df_temp[1,6]) #*.tif
image(t(as.matrix(raster(df_temp[1,8]))), main = paste(df_temp[1,7], " \n", "cobertura: ",round(df_temp[1,2], 4), sep = "") ) #*.png
image(as.matrix(read.table(df_temp[1,7], header = FALSE, sep = " ")), main = df_temp[1,7]) #*.txt
dev.off()


###### Comparando a cobertura vegetal da planilha com a calculada aqui ######
names(df_auditoria.imagens)[c(2,7)] <- c("cobertura_dados","txt.file") #ajeitando os nomes
#função que calcula a porcentagem de c(1,2) na matrix trinária (cobertura vegetal)
fun_cobertura <- function(X){ 
  matriz <- as.matrix(read.table(file = X, header = FALSE, sep = " ")) #lendo o arquivo
  cobertura <- 1-length(matriz[matriz==0])/length(matriz) #1 - porcentagem de 0s
  return(cobertura)
}

df_auditoria.imagens %>% mutate(cobertura_auditoria = sapply(.$txt.file, fun_cobertura)) -> df_temp
df_temp %<>% mutate(cobertura = cobertura_dados == cobertura_auditoria)

#Conferindo no google maps NOTA: to sem earth, mas não muda muita coisa
df_treeco <- read.csv(file= "/home/danilo/Documents/dissertacao/dados/refIDs_selecionados.csv", header = TRUE, sep="\t", dec=",") 
df_treeco %<>% select(refID, ordem, SiteCode, lat_correct, long_correct, lat, long)
df_auditoria.imagens %>% inner_join(., y = df_treeco, by = c("ordem","refID","SiteCode")) -> df_temp




# adpatar para apply family
func_aud.imagens <- function(x){
                      temp_x <- x
                      if_x <- substr(x, nchar(x)-3+1, nchar(x)) 
                      if(if_x == "tif"){
                        image(raster(x), main = temp_x)
                      } else if(if_x == "txt"){
                        image(as.matrix(read.table(x, header = FALSE, sep = " ")), main = temp_x)
                      } else { 
                        image(raster(x), main = temp_x)
                      }
} 