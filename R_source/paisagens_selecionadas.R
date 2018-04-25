###Selecao de sites TreeCo###
require(magrittr)
require(dplyr)

treeco_selecao <- read.csv(file = "treeco.csv", head =T, sep = "\t",as.is = TRUE, na.strings = c("","NA"))
names(treeco_selecao)
treeco_selecao <- treeco_selecao[,c(1,2,9,10,14,16,20,22,23,24,25,26,31,35,36,37,39,40,43,44,45,46,49,50,69,73,74,27)]
#transformando em números - usar o colwise do plyr pra diminuir as linhas aqui 
treeco_selecao$effort_ha <- as.numeric(treeco_selecao$effort_ha)
treeco_selecao$N <- as.numeric(treeco_selecao$N)
treeco_selecao$DA <- as.numeric(treeco_selecao$DA)
treeco_selecao$forest_size <- as.numeric(treeco_selecao$forest_size)
treeco_selecao$S <- as.numeric(treeco_selecao$S)
treeco_selecao$forest_age <- as.numeric(treeco_selecao$forest_age)
treeco_selecao$year <- as.numeric(treeco_selecao$year)
treeco_selecao$lat <- as.numeric(treeco_selecao$lat)
treeco_selecao$long <- as.numeric(treeco_selecao$long)
treeco_selecao$lat_correct <- as.numeric(treeco_selecao$lat_correct)
treeco_selecao$long_correct <- as.numeric(treeco_selecao$long_correct)

##filtros##
dim(treeco_selecao)
treeco_selecao <- treeco_selecao[grep("^yes",treeco_selecao$status),] #selecionando os 'yes's
treeco_selecao <- treeco_selecao[treeco_selecao$status_diagnostico %in% c("ok NAB digitado!","ok N digitado!","ok N digitado! foram digitados os traits?", "ok N digitado! email enviado_email com problemas"),]# selecionando aqueles que tem SADs
treeco_selecao <-treeco_selecao[treeco_selecao$confiabilidade %in% c("boa","exata","precisa"),] #selecionando os sites com coordenadas adequadas
treeco_selecao <- treeco_selecao[treeco_selecao$state %in% c("ES","MG","PR","RJ","RS","SC","SP"),]#selecionando apenas os estados do sul e sudeste - como ficou combinado no comitê
#treeco_selecao <- treeco_selecao[grep("^ok",treeco_selecao$status_diagnostico),] 
treeco_selecao_sem_privado <- treeco_selecao[treeco_selecao$Unidade_de_conservacao!="private",]#retirando os sites em locais privados
dim(treeco_selecao_sem_privado)

#ver se existe NAs em colunas chave
names(treeco_selecao)
summary(treeco_selecao[,c(3,4,5,6,8,21,17,18)])
treeco_selecao$DA[which(is.na(treeco_selecao$DA))] <- treeco_selecao[which(is.na(treeco_selecao$DA)),4]/treeco_selecao[which(is.na(treeco_selecao$DA)),3] #calculando a DA para os NA
NAs_lat_correct <- table(treeco_selecao[which(is.na(treeco_selecao$lat_correct)),"refID"]) #refIDs com NAs nas coordenadas
NAs_lat <- table(treeco_selecao[which(is.na(treeco_selecao$lat)),"refID"]) #refIDs com NAs nas coordenadas

###adaptar essa parte do código para todos os refIDs
coordenadas_sample_refID <- treeco[treeco$refID %in% sample_refIDs, c("refID","S","N","DA","SiteCode","lat_correct","long_correct","lat","long")]
coordenadas_sample_refID[which(is.na(coordenadas_sample_refID$lat_correct)),c("lat_correct","long_correct")] <- coordenadas_sample_refID[which(is.na(coordenadas_sample_refID$lat_correct)),c("lat","long")]
coord_sample_refID <- coordenadas_sample_refID[,c(1,6,7)]

###refIDs selecionadas segundo a unidade de conservação
treeco_selecao %>% select(Unidade_de_conservacao) %>% table(.,useNA = "ifany") %>% as.data.frame %>% arrange(desc(Freq))

##escrever tabela##
#Duas preocupações minhas: i) estou considerando "private" e ii) todas as áreas independente da idade do conjunto de dados
head(treeco_selecao[,c(1:15,24,25)])
#adicionar o número de pixels total da imagem a ser convertida
#treeco_selecao <- cbind(treeco_selecao,treeco_selecao$DA*2500)
#names(treeco_selecao)[27] <- "conversao_imagemagick"

write.table(treeco_selecao, file = "treeco_paisagens_selecionadas.txt", sep = "\t", dec = ".", row.names = FALSE)
