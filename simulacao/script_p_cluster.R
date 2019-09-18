## Rodar as Simulações ##
### configuracao
library(doMC)
library(magrittr)
library(tidyverse)
library(plyr)
setwd("/home/danilo/Documentos/Doutorado/artigo_mestrado/simulacao/")
#funcao coalescente
source("dinamica_coalescente_beta.R") 
# dados
df_referencia <- read.csv(file="df_referencia.csv",row.names = 1,as.is = TRUE)
# df_referencia %<>% #ddply("quantil_p",summarise,S_max=max(S),S_min=min(S))
                # filter(S %in% c(195,230,226,26,45,52))
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
    write.csv(aviao,
              file = paste0("./SADs/",
                            gsub(".txt","",df_temp[i,"txt.file"]),"_k",df_temp[i,"k"],".EE.csv")
    )
 }  
  # paralelização da simulacao
  registerDoMC(3)
  replica.sim <- as.list(1:dim(df_sim)[1])
  l_ply(.data = replica.sim, .fun = funcao_simulacao, .parallel = TRUE)
}


######################################################
#################### MNEI ############################
######################################################


