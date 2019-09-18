## Rodar as Simulações ##
### configuracao
library(doMC)
library(GUILDS)
library(magrittr)
library(tidyverse)
library(plyr)
setwd("/home/danilo/Documentos/Doutorado/artigo_mestrado/simulacao/")
#funcao coalescente
source("dinamica_coalescente_beta.R") 
# dados
df_referencia <- read.csv(file="df_referencia.csv",row.names = 1,as.is = TRUE)
# df_referencia %<>% filter(S %in% c(195,230,226,26,45,52))
  #ddply("quantil_p",summarise,S_max=max(S),S_min=min(S))

# df_referencia %>% str

######################################################
#################### MNEE ############################
######################################################

### 1 estimar U ###
#preparação
func1 <- function(x,replicas=10) {
  x$U <- NA
  x <- x[rep(1:dim(x)[1],each=replicas),]
}
df_referencia %<>% func1()

### for da simulação ##
# valores de k
k_factor <- unique(df_referencia$k)
for(a in 19:length(k_factor)){
  # a <- 1
  # i <- 1
  # por k
  df_simU <- df_referencia %>% filter(k == k_factor[a])
  ### funcao para paralelizar o programa
  op <- options(digits.secs=6)
  funcao_imigracao <- function(i,df_temp=df_simU){
    aviao <- list()
    aviao <- dinamica_coalescente(U = 1.25e-06, 
                                  S = df_temp[i,"S"], 
                                  N_simul = 1, 
                                  seed = as.numeric(Sys.time()), 
                                  disp_range = df_temp[i,"d"], 
                                  disp_kernel = df_temp[i,"kernel_code"], 
                                  landscape = df_temp[i,"txt.file"])
    return(aviao$U_est)
  }  
  # paralelização da simulacao
  registerDoMC(3)
  replica.sim <- as.list(1:dim(df_simU)[1])
  sim.coal_U <- llply(.data = replica.sim, .fun = funcao_imigracao, .parallel = TRUE)
  df_simU[,"U"] <- unlist(sim.coal_U)
  write.csv(df_simU, 
            file=paste0("./U/","df_simU__k",k_factor[a],".csv"),row.names = FALSE)
}

# Leitura e preparação para simulação da SAD 
df_simulacao <- map_df(Sys.glob("./U/*.csv"),read.csv)
df_simulacao %<>% ddply(names(.)[-13],summarise,U_med=mean(U),U_var=var(U))
df_simulacao$txt.file %<>% as.character()


# Simulação da SAD
op <- options(digits.secs=6)
for(a in 19:length(k_factor)){
  # a <- 20
  # i <- 1
  df_sim <- df_simulacao %>% filter(k == k_factor[a])
  # df_temp=df_sim
  ### funcao para paralelizar o programa
  funcao_simulacao <- function(i,df_temp=df_sim){
    aviao <- list()
    aviao <- dinamica_coalescente(U = df_temp[i,"U_med"], 
                                  S = 0, 
                                  N_simul = 100, 
                                  seed = as.numeric(Sys.time()), 
                                  disp_range = df_temp[i,"d"], 
                                  disp_kernel = df_temp[i,"kernel_code"], 
                                  landscape = df_temp[i,"txt.file"])
    l_SADs <- alply(aviao,.margins = 1,.fun = function(X) sort(as.vector(table(X)),decreasing = TRUE) )  
    file_name <- paste0("./SADs_preditas/",gsub(".txt","",df_temp[,"txt.file"]),"__k",df_temp[,"k"],".EE.", "rep_",1:length(l_SADs),".csv")
    for(b in 1:length(l_SADs)){
      write.csv(l_SADs[[b]],
                file=file_name[b],
                row.names = FALSE,col.names = FALSE)
    }
 }  
  # paralelização da simulacao
  registerDoMC(3)
  replica.sim <- as.list(1:dim(df_sim)[1])
  l_ply(.data = replica.sim, .fun = funcao_simulacao, .parallel = TRUE)
}


######################################################
#################### MNEI ############################
######################################################

# Leitura e preparação para simulação da SAD 
df_simulacao <- map_df(Sys.glob("./U/*.csv"),read.csv)
df_simulacao %<>% ddply(names(.)[-13],summarise,U_med=mean(U),U_var=var(U))
df_simulacao$txt.file %<>% as.character()
## Conversão dos Parâmetros de MNEE para MNEI ##
df_simulacao %<>% mutate(L_plot = 100/sqrt(J/DA),
                         m = d * ( 1 - exp(-L_plot/d) ) / L_plot, 
                         m_ = m * p / (1 - (1-p) * m),
                         I = m_ * (J-1)/(1-m_),
                         J_M=p*DA*2500,
                         theta=(U_med*(J_M-1))/(1-U_med))
df_simulacao %<>% mutate(k_prop=k) %>%  group_by(SiteCode,k_prop) %>% nest
## Predição da SAD
registerDoMC(3)
df_simulacao$SADs <- llply(df_simulacao[["data"]],function(X) replicate(100,generate.ESF(theta = X$theta, I = X$I, J = X$J)),.parallel = TRUE)
f_d_ply <- function(X){
  # df_name <- df_simulacao[1,]$data %>% as.data.frame
  # l_SADs <-  df_simulacao[1,]$SADs[[1]]
  df_name <- X$data %>% as.data.frame
  l_SADs <- X$SADs[[1]]
  file_name <- paste0("./SADs_preditas/",gsub(".txt","",df_name[,"txt.file"]),"__k",df_name[,"k"],".EI.", "rep_",1:length(l_SADs),".csv")
  for(i in 1:length(l_SADs)){
    write.csv(l_SADs[[i]],
              file=file_name[i],
              row.names = FALSE,col.names = FALSE)
  }
}
d_ply(df_simulacao,c("SiteCode","k_prop"),f_d_ply,.parallel = TRUE)


######################################################
############## Sintese dos dados #####################
######################################################

## df_geral ##
df_resultados <- map_df(Sys.glob("./U/*.csv"),read.csv)
df_resultados %<>% ddply(names(.)[-13],summarise,U_med=mean(U),U_var=var(U))
df_resultados$txt.file %<>% as.character()
df_resultados %<>% mutate(file.tag=gsub(".txt","",txt.file)) 

## df_SAD.obs
df_SAD.obs <- read.csv(file = "/home/danilo/Documentos/Doutorado/artigo_mestrado/simulacao/SADs_observadas/df_SAD_obs.csv") 
df_SAD.obs %<>% filter(N!=0 & sp != "Mortas") %>% group_by(SiteCode) %>% nest
names(df_SAD.obs)[2] <- "SAD.obs"

## df_SAD.predita ##
df_SAD.predita <- data.frame(SAD_MN.name=Sys.glob("./SADs_preditas/*.csv"))
find.string <- paste(c("./SADs_preditas/",".csv"),collapse = "|")
df_SAD.predita %<>% mutate(SAD_obs.name=gsub("__k","__SAD.txt",str_match(SAD_MN.name,"ref(.*?)__k")[,1]),
                  MN=str_match(SAD_MN.name,"EE|EI")[,1],
                  k=str_match(SAD_MN.name,"__k(.*?).E")[,2],
                  file.tag=gsub("__k","",str_match(SAD_MN.name,"ref(.*?)__k")[,1]),
                  rep=str_match(SAD_MN.name,"rep_(.*?).csv")[,2])
df_SAD.predita %>% str
df_resultados %>% str
df_SAD.predita %<>% left_join(x=.,y=unique(df_resultados[,c("file.tag","SiteCode")]),by="file.tag")

df_SAD.predita[,"file.tag"] %>% unique
