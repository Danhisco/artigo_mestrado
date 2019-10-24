## Rodar as Simulações ##
### configuracao
library(doMC)
library(GUILDS)
library(lme4)
library(merTools)
library(magrittr)
library(tidyr)
library(plyr)
library(purrr)
library(dplyr)
setwd("/home/danilo/Documentos/Doutorado/artigo_mestrado/1A.P_MOVER/simulacao")
#funcao coalescente
source("dinamica_coalescente_beta.R") 
# dados
df_referencia <- read.csv(file="df_p_simulacao.csv",header = TRUE,as.is = TRUE)
# df_sitios_nao_simulados <- read.csv("/home/danilo/Documentos/Doutorado/artigo_mestrado/1A.P_MOVER/df_sitios_nao_simulados.csv",header = TRUE,as.is = TRUE)
##### padronização do sistema #####
n_cores <- 4 # cores para paralelizar
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
  df_simU <- df_referencia %>% dplyr::filter(k == k_factor[a])
  ### funcao para paralelizar o programa
  op <- options(digits.secs=6)
  funcao_imigracao <- function(i,df_temp=df_simU){
    aviao <- list()
    aviao <- dinamica_coalescente(U = 1.25e-06, 
                                  S = df_temp[i,"Stotal"], 
                                  N_simul = 1, 
                                  seed = as.numeric(Sys.time()), 
                                  disp_range = df_temp[i,"d"], 
                                  disp_kernel = df_temp[i,"kernel_code"], 
                                  landscape = df_temp[i,"txt.name"])
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
registerDoMC(n_cores)
df_simulacao %<>% ddply(c("SiteCode","k"),summarise,U_med=mean(U),U_var=var(U),.parallel = TRUE)
df_simulacao %<>% left_join(x=.,
                            y=unique(dplyr::select(df_referencia,
                            SiteCode,refID,ordem,p,Ntotal,Stotal,txt.name,k,kernel_type,kernel_code,DA,dist_0,d)),
                            by=c("SiteCode","k"))
df_simulacao$txt.name %<>% as.character()
df_simulacao %<>% mutate(k_prop=k)
######################## para auditoria dos dados ##############################
######################## para auditoria dos dados ##############################
# df_simulacao %<>% filter(SiteCode %in% sitios_n_simulados)
######################## para auditoria dos dados ##############################
######################## para auditoria dos dados ##############################
#funcao para simulacao
  # i <- 1
# for(i in 1:nrow(df_simulacao)){
  # atribuição
f_simulacao <- function(i,df_=df_simulacao){
  X <- df_[i,]
  # simulacao
  mat_sim <- dinamica_coalescente(U = X[,"U_med"], 
                                  S = 0, 
                                  N_simul = n_rep.SAD, 
                                  seed = as.numeric(Sys.time()), 
                                  disp_range = X[,"d"], 
                                  disp_kernel = X[,"kernel_code"], 
                                  landscape = X[,"txt.name"])
  # forma de 
  l_SADs.preditas <- alply(mat_sim,1,function(Y) sort(as.integer(table(Y))) )
  file_name <- gsub("/home/danilo/Documentos/Doutorado/artigo_mestrado/1A.P_MOVER/simulacao/","",
                    X[,"txt.name"])
  file_name <- gsub(".txt","",file_name)
  path.file <- paste0(getwd(),"/SADs_preditas/",file_name,"__k",X[,"k"],".EE.", "rep_",1:length(l_SADs.preditas),".csv")
  for(j in 1:length(l_SADs.preditas)){
    write.csv(data.frame(SAD_predita = l_SADs.preditas[[j]]),
              file=path.file[j],
              row.names = FALSE)
  }
}
registerDoMC(4)
simulacao <- as.list(1:dim(df_simulacao)[1])
l_ply(simulacao,f_simulacao,.parallel = TRUE)
######################################################
#################### MNEI ############################
######################################################

# Leitura e preparação para simulação da SAD 
df_simulacao <- map_df(Sys.glob("./U/*.csv"),read.csv)
df_simulacao %<>% ddply(c("SiteCode","k"),summarise,U_med=mean(U),U_var=var(U))
df_simulacao %<>% left_join(x=.,
                            y=unique(dplyr::select(df_referencia,
                                                   SiteCode,p,Ntotal,Stotal,txt.name,k,kernel_type,kernel_code,DA,dist_0,d)),
                            by=c("SiteCode","k"))
df_simulacao$txt.name %<>% as.character()
## Conversão dos Parâmetros de MNEE para MNEI ##
df_simulacao %<>% mutate(L_plot = 100/sqrt(Ntotal/DA),
                         m = d * ( 1 - exp(-L_plot*sqrt(2)/d) ) / (L_plot*sqrt(2)), 
                         m_ = m * p / (1 - (1-p) * m),
                         I = m_ * (Ntotal-1)/(1-m_),
                         J_M=p*DA*2500,
                         theta=(U_med*(J_M-1))/(1-U_med))
# write.csv(df_simulacao,"/home/danilo/Documentos/Doutorado/artigo_mestrado/1A.P_MOVER/df_simulacao_EEeEI.csv",row.names = FALSE)

# df_simulacao %<>% mutate(k_prop=k) %>% group_by(SiteCode,k_prop) %>% nest
## Predição da SAD
f_simulacaoEI <- function(i,df_=df_simulacao){
  # i <- 1
  # n_rep.SAD <- 100
  df_name <- df_[i,]
  l_SADs <- replicate(n_rep.SAD,generate.ESF(theta = df_name$theta, I = df_name$I, J = df_name$Ntotal))
  file_name <- gsub("/home/danilo/Documentos/Doutorado/artigo_mestrado/1A.P_MOVER/simulacao/","",
                    df_name[,"txt.name"])
  file_name <- gsub(".txt","",file_name)
  path.file <- paste0(getwd(),"/SADs_preditas/",file_name,"__k",df_name[,"k"],".EI.", "rep_",1:length(l_SADs),".csv")
  for(j in 1:length(l_SADs)){
    write.csv(data.frame(SAD_predita = l_SADs[[j]]),
              file=path.file[j],
              row.names = FALSE)
  }
}
registerDoMC(4)
simulacao <- as.list(1:dim(df_simulacao)[1])
l_ply(simulacao,f_simulacaoEI,.parallel = TRUE)


######################################################
############## Sintese dos dados #####################
######################################################

## df_geral ##
df_referencia <- map_df(Sys.glob("./U/*.csv"),read.csv)
registerDoMC(4)
df_temp <-  ddply(df_referencia,c("SiteCode","k"),summarise,U_med=mean(U),U_var=var(U),.parallel = TRUE)
df_referencia %<>% dplyr::select(.,SiteCode,UC_area_ha,S,N,Ntotal,Stotal,txt.name,p,k,DA,d) %>% unique
df_referencia %<>% left_join(x=df_temp,y=.,by=c("SiteCode","k"))
df_referencia$txt.name %<>% as.character()
df_referencia %<>% mutate(file.tag=gsub("/home/danilo/Documentos/Doutorado/artigo_mestrado/1A.P_MOVER/dados_brutos/","",gsub(".txt","",txt.name)))

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
df_SAD.predita %<>% left_join(x=.,y=unique(dplyr::select(df_referencia,file.tag,SiteCode)),by="file.tag") %>% 
  group_by(SiteCode) %>% nest
df_SAD.predita %<>% left_join(x=.,y=df_SAD.obs,by="SiteCode")

## função para os resultados ##
f_resultados <- function(X){
  # df_referencia <- df_SAD.predita[1,]$data[[1]] %>% as.data.frame()
  # v_SAD.obs <- df_SAD.predita[1,]$SAD.obs[[1]] %>% as.data.frame() %>% .$N
  df_referencia <- X$data[[1]] %>% as.data.frame()
  v_SAD.obs <- X$SAD.obs[[1]] %>% as.data.frame() %>% .$N
  f_KSeS <- function(OBS = v_SAD.obs,df_predicao){
      # df_predicao <- df_referencia[1,]
      v_SAD.predita <- read.csv(file = df_predicao[,"SAD_MN.name"]) %>% .$SAD_predita %>% as.vector
      # teste de Kolmogoro-Smirnov #
        a <- suppressWarnings(ks.test(x = OBS,
                                      y = v_SAD.predita))
        a <- data.frame(KS.D = a$statistic, KS.p = a$p.value)
      a$S_SAD.predita <- length(v_SAD.predita)
      a$S_SAD.obs <- length(v_SAD.obs) 
      return(a)
      }  
  # teste de KS e armazenamento #
  resultados <- adply(df_referencia,1,function(Y) f_KSeS(df_predicao = Y))
  return(resultados)
}
registerDoMC(n_cores)
df_SAD.predita %<>% ddply(.,"SiteCode",f_resultados,.parallel = TRUE)
# registro
write.csv(df_SAD.predita,file="./resultados/df_replicas.csv",row.names = F)

