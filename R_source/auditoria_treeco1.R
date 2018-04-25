###### Auditoria TreeCo ######
##OBJETIVO: i) obter N, DA, S para cada SiteCode utilizando df_SAD.obs; ii) comparar com os valores utilizados nas simulações
library(magrittr)
library(plyr)
library(dplyr)
library(reshape2)
setwd("~/Documents/dissertacao/dados/")

### Dados 15dez2016 ###
load("/home/danilo/Documents/dissertacao/dados/imagens/paisagens_selecionadas/resultados_DaniloPMori.Rdata") # df com os resultados utilizados
df_resultados %>% filter(rep == "media_rep", Sindrome == "wind") %>% select(SiteCode, cobertura) %>% arrange(cobertura) -> df_SiteCode
### N, DA, S para cada SiteCode a partir de df_SAD.obs ###
df_SAD.obs <- read.csv(file = "data_para_PI_new.csv", header = TRUE, sep = ",") #versão atualizada da planilha com as abundâncias; email: Renato mandou novo .Rdata 22dez
df_SAD.obs %<>% select(SiteCode, sp, N, ha, area_ha, Ntotal, method, forest_type, forest_subtype, state)
##Vou criar uma planilha de referência para consulta a partir desta aqui
df_SAD.obs$SiteCode %<>% factor
df_SAD.obs %<>% filter(N != 0, sp != "morta") # filtrando as espécies mortas e aquelas presentes apenas nos trabalhos florísticos (observou-se a espécie mas não sua abund)
df_refences.obs <- df_SAD.obs %>% ddply(., "SiteCode", summarise, S = length(sp),
                                                                  N = sum(N),
                                                                  ha = mean(area_ha),
                                                                  Ntotal_treeco = mean(Ntotal),
                                                                  method = head(method,n=1), #quando tiver internet pesquisar melhor maneira
                                                                  fitofisio_type = head(forest_type,n=1),
                                                                  fitofisio_subtype = head(forest_subtype,n=1),
                                                                  estado = head(state,n=1))
df_refences.obs %>% mutate(N.true = N == Ntotal_treeco) %>% filter(N.true != TRUE) %>% dim
df_refences.obs %<>% mutate(DA = N/ha) %>% select(SiteCode, S,N,DA,ha,method,fitofisio_type,fitofisio_subtype,estado) #retirando Ntotal_treeco
write.table(df_refences.obs, file = "~/Documents/dissertacao/dados/df_references.obs.csv", row.names = FALSE, col.names = TRUE)
df_refences.obs %>% filter(SiteCode %in% df_SiteCode$SiteCode) -> df_simulacao0 # info para a simulação
df_SAD.obs %>% filter(SiteCode %in% df_SiteCode$SiteCode) -> df_SAD.obs0 # SADsobs 
df_SAD.obs0$sp %<>% factor
df_SAD.obs0$SiteCode %<>% factor
df_SAD.obs0 %>% str

# comparação com a planilha de dados de 15dez2016 #
load("~/Documents/dissertacao/dados/imagens/paisagens_selecionadas/simulacao_all_ranges_94.Rdata")
rm(df_dispersal,df_simulacao1,df_U.est,lista_SAD.sim,lista_dispersal)
df_simulacao %<>% select(-c(S,dif_S))
names(df_simulacao)[6] <- "S.x"
#df_simulacao %>% str
df_simulacao %<>% inner_join(.,y=df_simulacao0, by = "SiteCode")
#df_simulacao %>% head
df_simulacao %<>% mutate(dif_N = N.x - N.y, dif_DA = DA.x - DA.y, dif_S = S.x - S)
x11()
df_simulacao %>% select(dif_N,dif_DA,dif_S) %>% boxplot()
#Podemos ver que há um erro muito grande entre os valores usados nas duas planilhas. Vou seguir usando os dados calculados a partir da SAD.obs
#Vou refazer todas os ajustes de imagens e a simulação utilizando esses valores
df_simulacao %>% select(refID,SiteCode, ordem, paisagens, N.y, DA.y, S, ha, method, fitofisio_type, fitofisio_subtype, estado) -> df_simulacao0
head(df_simulacao0)
save(df_simulacao0, file = "~/Documents/dissertacao/dados/df_simulacao022dez.Rdata")
