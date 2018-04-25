####Construção de grafícos para resultados parciais
###Esse script precisa ser arrumado; há muita coisa aqui; e eu basicamente gasto mais tempo arrumando os dados do que fazendo os gráficos per se
###Acho melhor clivar esse script em dois
###carregar "df_dados_geral.Rdata" e seguir para a seccao dos gráficos

setwd("/home/danilo/Documents/dados/mestrado/simulacao/resultados")

require("reshape2")
require("ggplot2")
require("magrittr")
require("plyr")

load("dados_por_range_config1.Rdata")

###Construcao do data frame e reorganizacao
##wide -> long

riqueza_config1 %<>% melt(id.vars = "refID", variable.name = "range", value.name = "riqueza")
U_est_ranges_config1 %<>% melt(id.vars = "refID", variable.name = "range", value.name = "U_est_ranges")
p_value_config1 %<>% melt(id.vars = "refID", variable.name = "range", value.name = "p_value")
KS_config1 %<>% melt(id.vars = "refID", variable.name = "range", value.name = "KS")
df_dados <- cbind(riqueza_config1,U_est_ranges_config1[,3],KS_config1[,3],p_value_config1[,3])
names(df_dados)[-c(1:3)] <- c("U_est","KS","p_value")

##colocando em ordem
df_dados$range %<>% levels %>% as.numeric
df_dados$refID %<>% levels %>% as.numeric
df_dados %<>% arrange(. ,range,refID)
df_dados$range %<>% as.factor
levels(df_dados$refID)

###
##Dados do treeco
treeco <- read.table(file = "///home/danilo/treeco_paisagens_selecionadas.txt", header = T, sep = "\t", dec = ".")
riqueza <- treeco[treeco$refID %in% as.numeric(levels(df_dados$refID)),c("refID","S")]
riqueza %>% dim

##Adicionando os dados do treeco no data frame
df_df <- df_dados
df_NA <- as.data.frame(matrix(nrow = 16, ncol = 6))
names(df_NA) <- names(df_df)
df_df <- rbind(df_df,df_NA)
#arrumando os factors
df_df[is.na(df_df$KS),"range"] <- NA
df_df$fator <- factor(rep("frag",dim(df_df)[1]), levels = c("frag","sem_frag","sem_espaco","obs"))
levels(df_df$range) <- c(levels(df_df$range),"obs")
#repassando
df_dados <- df_df
#dados de riqueza obs
df_dados[is.na(df_dados$U_est),c("refID","riqueza","fator","range")] <- cbind(riqueza,rep("obs",dim(riqueza)[2]),rep("obs",dim(riqueza)[2]))
config <- rep("1",dim(df_dados)[1])
config <- factor(config, levels = c("30","1","obs"))
df_dados <- cbind(df_dados,config)
df_dados[is.na(df_dados$U_est),"config"] <- "obs"

df_dados

save(df_dados_geral, file = "df_dados_geral.Rdata")

##Juntando os dados config30 e config1##
load("df_dados_geral.Rdata")
head(df_dados_geral)
levels(df_dados_geral$fator)

config <- factor(rep("1",dim(df_dados)[1]), levels = c("30","1","obs"))
fator <- factor(rep("frag",dim(df_dados)[1]), levels = c("frag","sem_frag","sem_espaco","obs"))
df_dados <- cbind(df_dados, fator, config)
df_dados_geral <- rbind(df_dados_geral, df_dados)

df_dados_geral[is.na(df_dados_geral$U_est),c(2,7)] <- "obs"

###Enfim gráficos!###
load("df_dados_geral.Rdata")
str(df_dados_geral)

df_dados_geral_teste <- df_dados_geral
teste <- df_dados_geral_teste[is.na(df_dados_geral_teste$U_est),]
df_dados_geral_teste[is.na(df_dados_geral_teste$U_est),"config"] <- "30"
df_dados_geral_teste <- rbind(df_dados_geral_teste, df_dados_geral_teste[is.na(df_dados_geral_teste$U_est),])
df_dados_geral_teste[-(1:(dim(df_dados_geral_teste)[1]-16)),"config"] <- "1"

df_dados_geral <- df_dados_geral_teste

df_dados_geral$tree_cover <- 1-df_dados_geral$tree_cover

(df_dados_geral$U_est[df_dados_geral$config==1] == df_dados_geral$U_est[df_dados_geral$config==30]) %>% table
