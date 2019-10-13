## Rodar as Simulações ##
### configuracao
library(doMC)
library(GUILDS)
library(lme4)
library(merTools)
library(magrittr)
library(tidyverse)
library(plyr)
setwd("/home/danilo/Documentos/Doutorado/artigo_mestrado/1A.P_MOVER/simulacao")
#funcao coalescente
source("dinamica_coalescente_beta.R") 
# dados
df_referencia <- read.csv(file="df_dados_auditados.csv",row.names = 1,as.is = TRUE)
##### padronização do sistema #####
n_cores <- 4 # número de cores do computador 
n_rep.U <- 10 # número de Us réplicas
n_rep.SAD <- 100 # nũmero de SADs replicas


######################################################
#################### MNEE ############################
######################################################

### estimar U ###
# número de réplicas
func1 <- function(x,replicas=n_rep.U) {
  x$U <- NA
  x <- x[rep(1:dim(x)[1],each=replicas),]
}
df_referencia %<>% func1()

### simulação ##
# valores de k
k_factor <- unique(df_referencia$k)
for(a in 1:length(k_factor)){
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
  registerDoMC(n_cores)
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
df_simulacao %<>% mutate(k_prop=k) %>% group_by(SiteCode,k_prop) %>% nest
#funcao para simulacao
f_simulacao <- function(i,df_=df_simulacao){
  X <- df_[i,][["data"]] %>% as.data.frame()
  mat_sim <- dinamica_coalescente(U = X[,"U_med"], 
                                  S = 0, 
                                  N_simul = n_rep, 
                                  seed = as.numeric(Sys.time()), 
                                  disp_range = X[,"d"], 
                                  disp_kernel = X[,"kernel_code"], 
                                  landscape = X[,"txt.file"])
}
registerDoMC(n_cores)
simulacao <- as.list(1:dim(df_simulacao)[1])
df_simulacao$SADs.EE  <- llply(simulacao,f_simulacao,.parallel = TRUE) 
#funcao para escrita das SADs em .csv
f_d_ply.EE <- function(X){
  # df_name <- df_simulacao[1,]$data %>% as.data.frame
  # l_SADs <-  df_simulacao[1,]$SADs.EE[[1]]
  df_name <- X$data %>% as.data.frame
  l_SADs <- X$SADs.EE[[1]] %>% alply(.,1,function(X) sort(as.integer(table(X))) )
  file_name <- paste0("./SADs_preditas/",gsub(".txt","",df_name[,"txt.file"]),"__k",df_name[,"k"],".EE.", "rep_",1:length(l_SADs),".csv")
  for(i in 1:length(l_SADs)){
    write.csv(l_SADs[[i]],
              file=file_name[i],
              row.names = FALSE,col.names = FALSE)
  }
}
d_ply(df_simulacao,c("SiteCode","k_prop"),f_d_ply.EE,.parallel = TRUE)

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
registerDoMC(n_cores)
df_simulacao$SADs <- llply(df_simulacao[["data"]],function(X) replicate(n_rep,generate.ESF(theta = X$theta, I = X$I, J = X$J)),.parallel = TRUE)
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
df_SAD.obs <- read.csv(file = "/home/danilo/Documentos/Doutorado/artigo_mestrado/1A.P_MOVER/simulacao/SAD_observada/abundances.csv") 
# df_SAD.obs %>% dim
df_SAD.obs %<>% group_by(SiteCode) %>% nest
names(df_SAD.obs)[2] <- "SAD.obs"


## df_SAD.predita ##
df_SAD.predita <- data.frame(SAD_MN.name=as.character(Sys.glob("./SADs_preditas/*.csv")))
find.string <- paste(c("./SADs_preditas/",".csv"),collapse = "|")
df_SAD.predita %<>% mutate(SAD_obs.name=gsub("__k","__SAD.txt",str_match(SAD_MN.name,"ref(.*?)__k")[,1]),
                           MN=str_match(SAD_MN.name,"EE|EI")[,1],
                           k=str_match(SAD_MN.name,"__k(.*?).E")[,2],
                           file.tag=gsub("__k","",str_match(SAD_MN.name,"ref(.*?)__k")[,1]),
                           rep=str_match(SAD_MN.name,"rep_(.*?).csv")[,2])
# df_SAD.predita %>% str
# df_resultados %>% str
# Merges #
df_SAD.predita$SAD_MN.name %<>% as.character()
df_SAD.predita %<>% left_join(x=.,y=unique(df_resultados[,c("file.tag","SiteCode")]),by="file.tag") %>% 
  group_by(SiteCode) %>% nest
df_SAD.predita %<>% left_join(x=.,y=df_SAD.obs,by="SiteCode")

## função para os resultados ##
f_resultados <- function(X){
  # df_referencia <- df_SAD.predita[1,]$data %>% as.data.frame()
  # v_SAD.obs <- df_SAD.predita[1,]$SAD.obs %>% as.data.frame() %>% .$N
  df_referencia <- X$data %>% as.data.frame()
  v_SAD.obs <- X$SAD.obs %>% as.data.frame() %>% .$N
  f_KSeS <- function(OBS = v_SAD.obs,df_predicao){
      # df_predicao <- df_referencia[1,]
      v_SAD.predita <- read.csv(file = df_predicao[,"SAD_MN.name"]) %>% .$x
      # teste de Kolmogoro-Smirnov #
        a <- suppressWarnings(ks.test(x = OBS,
                                      y = v_SAD.predita))
        a <- data.frame(KS.D = a$statistic, KS.p = a$p.value)
      a$S_SAD.predita <- length(v_SAD.predita)
      a$S_SAD.obs <- length(v_SAD.obs) 
      return(a)
      }  
  # teste de KS e armazenamento #
  df_resultados <- adply(df_referencia,1,function(X) f_KSeS(df_predicao = X))
  return(df_resultados)
}
registerDoMC(n_cores)
df_SAD.predita %<>% ddply(.,"SiteCode",f_resultados,.parallel = TRUE)
# registro
write.csv(df_SAD.predita,file="./resultados/df_replicas.csv",row.names = F)

