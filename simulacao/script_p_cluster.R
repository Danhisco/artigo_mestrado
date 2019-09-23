## Rodar as Simulações ##
### configuracao
library(doMC)
library(GUILDS)
library(lme4)
library(merTools)
library(magrittr)
library(tidyverse)
library(plyr)
setwd("/home/danilo/Documentos/Doutorado/artigo_mestrado/simulacao/")
#funcao coalescente
source("dinamica_coalescente_beta.R") 
# dados
df_referencia <- read.csv(file="df_referencia.csv",row.names = 1,as.is = TRUE)
# df_referencia %<>% filter(S %in% c(195,230,226,26,45,52)) # sítios com S extremos em cada terço do gradiente de p
df_referencia %<>% filter(k %in% c(0.99,0.5,0.05))
# ddply("quantil_p",summarise,S_max=max(S),S_min=min(S))

# df_referencia %>% str
# df_referencia %>% ggplot(aes(x=k,y=d,group=k)) + geom_boxplot() + geom_jitter() +
  # labs(y="dist. média (metros)")


##### padronização do sistema #####
n_cores <- 2 # número de cores do computador 
n_rep <- 10 # nũmero de SADs replicas
######################################################
#################### MNEE ############################
######################################################

### estimar U ###
# número de réplicas
func1 <- function(x,replicas=10) {
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
registerDoMC(2)
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
registerDoMC(2)
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
df_SAD.obs <- read.csv(file = "/home/danilo/Documentos/Doutorado/artigo_mestrado/simulacao/SAD_observada/abundances.csv") 
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
# df_SAD.predita <- read.csv("./resultados/df_replicas.csv")
################################################
############ auditoria f_resultados ############
################################################
df_SAD.predita %>% head
df_SAD.predita %>% ggplot(aes(x=KS.D,y=KS.p)) + 
  geom_point() +
  facet_wrap(MN~k,ncol=3,scales = "free") +
  labs(x="estatística D, teste KS",y="p-valor")
df_SAD.predita %>% ggplot(aes(x=S_SAD.obs,y=KS.p)) + 
  geom_point() +
  facet_wrap(MN~k,ncol=3,scales = "free")
df_SAD.predita %>% ggplot(aes(x=S_SAD.predita,y=KS.p)) + 
  geom_point() +
  facet_wrap(MN~k,ncol=3,scales = "free")


df_SAD.predita %<>% ddply(.,c("SiteCode","MN","k","S_SAD.obs"),summarise,
                          GOF=sum(KS.p>=0.05),
                          p.value_mean=mean(KS.p),p.value_var=var(KS.p),
                          S_mean=mean(S_SAD.predita),S_var=var(S_SAD.predita))
df_SAD.predita %<>% left_join(x=.,y=df_resultados[,c(1:6,11,13:14)],by=c("SiteCode","k"))
write.csv(df_SAD.predita,file="./resultados/df_resultados.csv",row.names = F)

######################################################
############## Analise dos Dados #####################
######################################################
# leitura
df_resultados <- read.csv(file="./resultados/df_resultados.csv")
# z score
f_z <- function(x){
  m <- base::mean(x,na.rm=TRUE)
  sd <- sd(x,na.rm=TRUE)
  output <- (x-m)/sd
  return(output)
}
df_resultados %<>% mutate(p.z = f_z(p),S.z = f_z(S))
names(df_resultados)[1] <- "Site"

########## Auditoria dos Dados ############
# df_resultados%>% head
## figura S X S
df_resultados %>% 
  ggplot(aes(x=S,y=S_SAD.obs)) + 
  geom_point() + 
  geom_abline(intercept = 0,slope=1,color="red") +
  labs(x="S planilha references on line",
       y="S vetor utilizado em KS")
# df_auditoria <- df_resultados %>% mutate(sitecode_difS=S.obs_mean-S) %>% filter(sitecode_difS>0) %>% select(Site,S,S.obs_mean) %>% unique
# write.csv(df_auditoria,file="./resultados/df_auditoria.csv",row.names = FALSE)
df_resultados %>% 
  ggplot(aes(x=p.value_mean,y=GOF)) + 
  geom_point() +
  facet_wrap(MN~k,ncol = 3)


###################### GOF ######################

# estimativa do modelo mais plausível (até o momento) 
md_GOF <- glmer(cbind(GOF,100-GOF) ~ p.z * k * MN + (MN|Site), family = "binomial",data=df_resultados,
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
## bootmer ##
df_new.data <- expand.grid(Site = df_resultados$Site[1],
                           p.z = seq(min(df_resultados$p.z)*1.1,max(df_resultados$p.z)*1.1, length=length(unique(df_resultados$p))),
                           k = unique(df_resultados$k),
                           MN = unique(df_resultados$MN))
# previsto considerando a estrutura fixa e aleatória #
f1 <- function(.) predict(.,newdata=df_new.data)
# previsto considerando a estrutura fixa #
f2 <- function(.) predict(.,newdata=df_new.data, re.form=~0)
## Os dois bootstraps. Ajuste o argumento ncpus para o numero de cores de seu computador
b1 <- bootMer(md_GOF, FUN = f1, nsim=1000, parallel="multicore", ncpus=n_cores)
b2 <- bootMer(md_GOF, FUN = f2, nsim=1000, parallel="multicore", ncpus=n_cores)
# preparacao dos dados
df_new.data$p <- df_new.data$p.z*sd(df_resultados$p) + mean(df_resultados$p) 
df_new.data$mean <- apply(b1$t,2,mean)
df_new.data$IC.low <- apply(b1$t,2,quantile, 0.025)
df_new.data$IC.upp <- apply(b1$t,2,quantile, 0.975)
df_new.data$mean.fixed <- apply(b2$t,2,mean)
df_new.data$IC.low.fixed <- apply(b2$t,2,quantile, 0.025)
df_new.data$IC.upp.fixed <- apply(b2$t,2,quantile, 0.975)
# escrita
write.csv(df_new.data,file="./resultados/df_GOF__bootmer.csv")
