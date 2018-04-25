###A ideia é ter dois data frames: i) refID+range+U_est+riqueza+KS+tree_cover; ii) refID+range+abundance
##

require(plyr)
require(dgof)
require(magrittr)
require(reshape2)
require(dplyr)
source("~/Documents/dissertacao/R_source/sad_media.R")

##Leitura e arranjo dos dados simulados
#o primeiro elemento de cada elemento da lista será transformado em um data frame com o nome da espécie e a frequência dessa espécie
#load("SAD_sim_all_ranges.Rdata") #carregando os dados brutos da simulação, N_simul = 30
load("/home/danilo/Documents/dissertacao/dados/simulacao/resultados/sim_6jun.Rdata") #lista que contem duas listas, uma com os outputs brutos das simulações e outra com um df com as taxas de especiação

##reshape a U_est wide -> long
U_est <- sim_6jun[[2]]
refID <- U_est %>% rownames %>% as.numeric
U_est %<>% melt
names(U_est) <- c("range","U_est")
U_est %<>% cbind(refID,.)
exc_rows <- U_est[U_est$refID %in% c(847,831,889), ] %>% rownames %>% as.numeric
U_est <- U_est[-exc_rows, ]
U_est$refID %<>% factor #para tirar os levels não usados
#U_est %>% head


##Calculando a SAD media para cada range e para cada refID
sad_sim <- sim_6jun[[1]] #pegando o output bruto das simulações, não o df de taxas de especiação estimada

#calculando as SADs medias e organizando em um formato único - mesmo formato do treeco
for(b in 1:(sad_sim %>% length) ){
  for(c in 1:(sad_sim[[b]] %>% length) ){
    sad_sim[[b]][[c]] %<>% sad_media
    sad_sim[[b]][[c]] <- data.frame(SAD = names(sad_sim[[b[c]]]), abundance = sad_sim[[b]][[c]], range = names(sad_sim)[b] )
  }
}
#juntando todas as SADs em apenas um data frame
sad_sim %<>% rbind.fill
names(sad_sim) <- c("refID","N","range")
#sad_sim %>% str
#juntando todos as SADs em apenas um data frame por range
for(d in 1:(sad_sim %>% length) ){ 
  sad_sim[[d]] %<>% rbind.fill
}


##KS para cada valor de range
#Leitura e arranjo dos dados observados
treeco <- read.table(file = "///home/danilo/treeco_paisagens_selecionadas.txt", header = T, sep = "\t", dec = ".")
SAD_obs <- read.csv(file = "///home/danilo/abundances-1.csv",header = TRUE, sep = "\t", dec = ".")
SAD_obs <- SAD_obs[,c("RefID","species.correct","N")]
names(SAD_obs) <- c("refID","sp","N")
SAD_obs %<>% filter(sp != "Mortas", N != 0)


#criando os objetos que vão armazenar os resultados do teste
KS <- as.data.frame( matrix(ncol = ranges  %>% length, nrow = refIDs %>% length) )
p_value  <- as.data.frame( matrix(ncol = ranges  %>% length, nrow = refIDs %>% length) )
#aplicando o teste e realocando o output
for(g in 1:dim(KS)[2] ){
  for(h in 1:dim(KS)[1] ){
    a <- sad_sim %>% filter(refID == refIDs[h],  range == ranges[g]) %>% select(N)
    b <- SAD_obs %>% filter(refID == refIDs[h]) %>% arrange(., desc(N)) %>% select(N)
    teste_ks <- suppressWarnings( ks.test(x = a, y = b$N) ) #por algum motivo eu tive que colocar o $N no 'b', contudo no 'a' não.
    KS[h,g] <- teste_ks$statistic
    p_value[h,g] <- teste_ks$p.value
  }
}










for(e in 1:dim(df_KS)[1]){
  SAD.obs <- df_SAD.obs.f %>% filter(SiteCode %in% df_KS[e,1]) %>% arrange(.,desc(N)) %>% .$N
  SAD.sim <- df_SAD.sim0 %>% filter(SiteCode %in% df_KS[e,1])
  for(f in 1:length(levels(SAD.sim$sindrome) ) ){
    sad.y <- SAD.sim %>% filter(sindrome %in% levels(SAD.sim$sindrome)[f]) %>% arrange(.,desc(N)) %>% .$N
    teste_ks <- suppressWarnings( ks.test(x = SAD.obs, y = sad.y) )
    df_KS[e,(f+1)] <- teste_ks$statistic
  }
}



















#meltando KS e p_value
KS %<>% melt
names(KS) <- c("fator","KS")
p_value %<>% melt
names(p_value) <- c("fator","p_value")

###Colando tudo em apenas um data_frame
df_resultados <- cbind(U_est, riqueza_df$riqueza, KS$KS, p_value$p_value)
names(df_resultados)[4:6] <- c("riqueza","KS","p_value")

###Arrumando a ordem
df_resultados$range <- as.numeric( levels(df_resultados$range) )[df_resultados$range]
df_resultados %<>% arrange(range,refID)
df_resultados$range %<>% factor
###Adicionando a riqueza observada

S_obs <- data.frame(refID = refIDs, range = NA, U_est = NA,
                    riqueza = treeco %>% filter(refID %in% refIDs) %>% select(S),
                    KS = NA, p_value = NA ) 
names(S_obs)[4] <- "riqueza"
df_resultados <- rbind(df_resultados,S_obs)
levels(df_resultados$range)[8] <- "obs"
df_resultados[is.na(df_resultados$U_est),"range"] <- "obs"


###Adicionando a cobertura vegetal
##abrir 'cobertura_vegetal' e realizar o calculo
reference <- df_resultados$refID %>% levels %>% as.numeric
df_resultados$tree_cover <- NA
df_resultados$tree_cover <- (df_mat_tri %>% filter(refID %in% reference) %>% .$cobertura)

##Juntar tudo em um data frame único
save(df_resultados,sad_sim, file = "resultados_sim6jun.Rdata")




##Calculando a riqueza
#Vou criar um data frame e depois vou usar o melt, como fiz com o U_est
ranges <- sad_sim$range %>% levels
refIDs <- c(1067,178,246,2817,437,619,677,868,870,887,89,8,987) %>% sort #se tirar o sort, a riqueza observada fica fora de ordem
riqueza_df <- as.data.frame( matrix(ncol = ranges  %>% length, nrow = refIDs %>% length) )

for(e in 1:dim(riqueza_df)[2]){
  for(f in 1:dim(riqueza_df)[1]){
    riqueza_df[f,e] <- sad_sim %>% filter(refID == refIDs[f],  range == ranges[e]) %>% nrow
  }
}
riqueza_df %<>% melt
names(riqueza_df) <- c("fator_igual_U_est","riqueza")
