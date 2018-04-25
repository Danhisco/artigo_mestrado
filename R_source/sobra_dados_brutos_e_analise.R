


func_ddply <- function(x){
  a <- suppressWarnings(ks.test(x = sad.obs, y = sort(x$N.log) ))
  a <- data.frame(KS.D = a$statistic, KS.p = a$p.value)
  return(a)}

df_sim <- df_SAD.sim[df_SAD.sim$SiteCode == SiteCodes[1], -1]
sad.obs <- df_SAD.obs %>% filter(SiteCode == SiteCodes[1]) %>% .$N.obs %>% sort %>% log 

for(i in 1000){
  as.character(df_resultados.rep$SiteCode[i]) -> fator.obs
  df_resultados.rep[i,] -> df_KS.for
  df_SAD.obs0 %>% filter(SiteCode == fator.obs) %>% .$N.obs %>% sort(.) %>% log -> sad.obs
  df_SAD.sim %>% filter(rep == df_KS.for[,"rep"], SiteCode == df_KS.for[,"SiteCode"], cluster_medio == df_KS.for[,"cluster_medio"]) %>% .$N %>% sort(.) %>% log -> sad.sim
  teste_ks <- suppressWarnings( ks.test(x = sad.obs, y = sad.sim) ) 
  df_KS.for[1,"KS.D"] <- teste_ks$statistic
  df_KS.for[1,"KS.p"] <- teste_ks$p.value
  df_KS.f <- KS_position(X.obs = sad.obs, X.sim = sad.sim)
  cbind(df_KS.for, df_KS.f)
}

## posição onde o KS ocorre ##
source("~/Documents/dissertacao/R_source/posicao_KS.R")
df_resultados %<>% mutate(fator = as.factor(paste(SiteCode, Sindrome, sep=""))) #gambiarra, pois tive problemas com o group_by
df_temp <- data.frame(df_resultados[,c(1,2,10)], KS_log.N = NA, KS_N = NA, KS_ac_obs = NA, KS_ac_sim = NA, KS_ac_diff = NA) #df que vai armazenar os outputs da função
df_SAD.sim0 %<>% mutate(fator = paste(SiteCode, sindrome, sep="")) #gambiarra, pois tive problemas com o group_by
for(i in 1:length(levels(df_temp$fator))){
  df_SAD.sim0 %>% filter(fator == levels(df_temp$fator)[i]) %>% arrange(N) -> sad.sim #df com vetor numerico e informações de fatores
  df_SAD.obs.f %>% filter(SiteCode == as.character(unique(sad.sim$SiteCode))) %>% arrange(N) %>% .$N %>% log-> sad.obs #a log(SAD.obs) do sitecode
  sad.sim %<>% .$N %>% log #sad.sim em escala log
  df_temp[df_temp$fator == levels(df_temp$fator)[i],-(1:3)] <- KS_position(X.obs = sad.obs, X.sim = sad.sim)
}
df_resultados %<>% inner_join(.,df_temp, by = c("fator","SiteCode", "Sindrome"))





df_SAD.obs %>% filter(SiteCode %in% unique(df_SAD.sim0$SiteCode)) -> df_SAD.obs.f
df_KS <- df_U.est # o df_KS terá a mesma estrutura que df_U.est
df_KS[,-1] <- NA #limpando os valores para 

## estatística de KS ##
# O 'for' irá pegar a SADsim e SADobs de cada sitecode e vai realizar a estatística de KS. Preciso aprender como calcular o local onde a maior diferença entre as acumuladas se dá.
for(e in 1:dim(df_KS)[1]){
  SAD.obs <- df_SAD.obs.f %>% filter(SiteCode %in% df_KS[e,1]) %>% arrange(.,desc(N)) %>% .$N #selecionando por SiteCode
  SAD.sim <- df_SAD.sim0 %>% filter(SiteCode %in% df_KS[e,1]) #idem
  for(f in 1:length(levels(SAD.sim$sindrome) ) ){
    sad.y <- SAD.sim %>% filter(sindrome %in% levels(SAD.sim$sindrome)[f]) %>% arrange(.,desc(N)) %>% .$N #selecionando por sindrome a SADsim
    teste_ks <- suppressWarnings( ks.test(x = SAD.obs, y = sad.y) ) 
    df_KS[e,(f+1)] <- teste_ks$statistic
  }
}
names(df_KS)[1] <- "SiteCode"
names(df_U.est)[1] <- "SiteCode"

### Fazendo o mesmo para o p value ###

#df_KS_p <- dcast(df_KS, SiteCode ~ Sindrome, value.var = "KS") #df_KS_p = df_KS em wide format - exemplo de como desmeltar
df_KS_p <- df_U.est
df_KS_p[,-1] <- NA #limpando os valores

## estatística de KS ##
# O 'for' irá pegar a SADsim e SADobs de cada sitecode e vai realizar a estatística de KS. Preciso aprender como calcular o local onde a maior diferença entre as acumuladas se dá.
for(e in 1:dim(df_KS_p)[1]){
  SAD.obs <- df_SAD.obs.f %>% filter(SiteCode %in% df_KS_p[e,1]) %>% arrange(.,desc(N)) %>% .$N
  SAD.sim <- df_SAD.sim0 %>% filter(SiteCode %in% df_KS_p[e,1])
  for(f in 1:length(levels(SAD.sim$sindrome) ) ){
    sad.y <- SAD.sim %>% filter(sindrome %in% levels(SAD.sim$sindrome)[f]) %>% arrange(.,desc(N)) %>% .$N
    teste_ks <- suppressWarnings( ks.test(x = SAD.obs, y = sad.y) )
    df_KS_p[e,(f+1)] <- teste_ks$p.value
  }
}
names(df_KS_p)[1] <- "SiteCode"


## Riqueza média ##
#summarise(group_by(df_S.sim0, SiteCode, sindrome), mean=mean(S), sd=sd(S)) #pq não está funcionando?
df_S.sim0 %<>% mutate(fator = paste(SiteCode,sindrome, sep=""))
df_S_medio_sd <- ddply(df_S.sim0, "fator", function(x) data.frame(SiteCode = x$SiteCode, Sindrome = x$sindrome, S_medio = mean(x$S), S_sd = sd(x$S)) ) %>% unique()
df_S_medio_sd <- df_S_medio_sd[,-1]
## Meltando tudo ## 
df_KS %<>% melt
names(df_KS)[-1] <- c("Sindrome","KS")
df_KS_p %<>% melt
names(df_KS_p)[-1] <- c("Sindrome","KS_p")
df_U.est %<>% melt
names(df_U.est) <- c("SiteCode", "Sindrome","U.est")
## inner_join ##
df_S_medio_sd %>% inner_join(.,df_KS, by = c("SiteCode", "Sindrome")) -> df_resultados
df_resultados %<>% inner_join(.,df_KS_p, by = c("SiteCode", "Sindrome")) 
#df_U.est$sindrome %<>% as.character #preparando df_U.est
#df_U.est <- df_U.est[,-2]
#names(df_U.est)[2] <- "Sindrome"
df_resultados %<>% arrange(SiteCode, Sindrome)
df_U.est %<>% arrange(SiteCode, Sindrome)
df_resultados$U.est <- df_U.est$U.est
df_resultados %<>% inner_join(.,df_simulacao[,c("SiteCode","cobertura")], by = "SiteCode")
names(df_dispersal)[1] <- "Sindrome"
df_resultados %<>% inner_join(.,df_dispersal[,-3], by = "Sindrome")
df_resultados %<>% inner_join(., y = df_simulacao[,c("SiteCode","aggregation.index")], by = "SiteCode")
#df_SAD.sim0
df_SAD.sim0$SiteCode %<>% as.factor
df_SAD.sim0 <- df_SAD.sim0[,-1]
df_SAD.sim0$cobertura <- NA
df_resultados$SiteCode %<>% as.character
for(i in 1:length(levels(df_SAD.sim0$SiteCode))){
  df_SAD.sim0[df_SAD.sim0$SiteCode == levels(df_SAD.sim0$SiteCode)[i],"cobertura"] <- unique(df_resultados[df_resultados$SiteCode==levels(df_SAD.sim0$SiteCode)[i],"cobertura"])
}
## ultimo tapa ##
df_simulacao <- df_simulacao[,-12]
names(df_simulacao)[10] <- "arquivo.paisagem"
df_resultados %>% arrange(Sindrome) %>% select(SiteCode, Sindrome, cluster_medio, cobertura, S_medio, S_sd, KS, KS_p, U.est) -> df_resultados #NOTA:falta A posição do KS
#df_SAD.sim0 %<>% select(-SAD)
df_S.sim0 %<>% select(-fator)



## posição onde o KS ocorre ##
source("~/Documents/dissertacao/R_source/posicao_KS.R")
df_resultados %<>% mutate(fator = as.factor(paste(SiteCode, Sindrome, sep=""))) #gambiarra, pois tive problemas com o group_by
df_temp <- data.frame(df_resultados[,c(1,2,10)], KS_log.N = NA, KS_N = NA, KS_ac_obs = NA, KS_ac_sim = NA, KS_ac_diff = NA) #df que vai armazenar os outputs da função
df_SAD.sim0 %<>% mutate(fator = paste(SiteCode, sindrome, sep="")) #gambiarra, pois tive problemas com o group_by
for(i in 1:length(levels(df_temp$fator))){
  df_SAD.sim0 %>% filter(fator == levels(df_temp$fator)[i]) %>% arrange(N) -> sad.sim #df com vetor numerico e informações de fatores
  df_SAD.obs.f %>% filter(SiteCode == as.character(unique(sad.sim$SiteCode))) %>% arrange(N) %>% .$N %>% log-> sad.obs #a log(SAD.obs) do sitecode
  sad.sim %<>% .$N %>% log #sad.sim em escala log
  df_temp[df_temp$fator == levels(df_temp$fator)[i],-(1:3)] <- KS_position(X.obs = sad.obs, X.sim = sad.sim)
}
df_resultados %<>% inner_join(.,df_temp, by = c("fator","SiteCode", "Sindrome"))


## salvar os objetos ##
save(df_simulacao, df_resultados, df_SAD.sim0, df_S.sim0, df_SAD.obs.f, file = "resultados.Rdata")
```


### Calculos considerando as réplicas ao inves da SAD media ###

Vou fazer os calculos de KS para cada uma das réplicas, pretendo ter no final uma tabela como a de df_S.sim0 mas com todas as colunas relacionadas com KS

```{r métricas de comparação da dinâmica ecológica considerando as replicas}
## posicao_KS.R ##
df_SAD.rep <- lista_dispersal
func_apply <-function(X){
  X %<>% table %>% as.data.frame() %>% arrange(desc(Freq)) #transformo cada linha da matrix em uma SAD
  data.frame(N = X$Freq, sp_ID = paste("sp.",X$., sep = ""))
}
for(b in 1:length(df_SAD.rep) ){ #número de classes de sindrome de dispersão
  for(c in 1:length(df_SAD.rep[[b]]) ){ #número de sitecodes
    l_SAD.rep <- df_SAD.rep[[b]][[c]] #matrix com as 100 réplicas da simulação do par (SiteCode,Sindrome)
    l_SAD.rep %<>% apply(., 1, func_apply) #transforma a matrix de replicas em uma lista com 100 data frames, cada um com uma SAD.rep
    sitecode <- gsub("SAD_sim_","",names(df_SAD.rep[[b]])[c]) #extração do sitecode 
    df_SAD.obs.f %>% filter(SiteCode == sitecode) %>% arrange(N) %>% .$N %>% log -> SAD.obs #SAD.obs do sitecode ordenado e transformado em log
    df_SAD.rep[[b]][[c]] <- lapply(l_SAD.rep, function(x) KS_position(X.obs = SAD.obs, X.sim = log(x$N))) #aplica a func a cada SAD.rep do par (SiteCode, Sindrome)
    names(df_SAD.rep[[b]][[c]]) <- 1:100
  }
}





##como existem algumas réplicas que possuem mais de um ponto onde KS é máximo, alguns SiteCodes possuem mais de uma observação
##para não perder esssa informação eu vou adicionar a coluna rep aos data frames presentes nas listas
for(b in 1:length(df_SAD.rep) ){ #número de classes de sindrome de dispersão
  for(c in 1:length(df_SAD.rep[[b]]) ){ #número de sitecodes
    for(d in 1:100){
      temp_SAD.rep <- df_SAD.rep[[b]][[c]][[d]]
      temp_SAD.rep$rep <- names(df_SAD.rep[[b]][[c]])[d]
      df_SAD.rep[[b]][[c]][[d]] <- temp_SAD.rep
    }
  }
}
#concatenando tudo em um data frame unico para cada par (SiteCode, Sindrome)
for(b in 1:length(df_SAD.rep) ){
  for(c in 1:length(df_SAD.rep[[b]]) ){
    df_SAD.rep[[b]][[c]] %<>% rbind.fill
    df_SAD.rep[[b]][[c]] <- cbind(df_SAD.rep[[b]][[c]], gsub("SAD_sim_","",names(df_SAD.rep[[b]])[c]), names(df_SAD.rep)[b])
    names(df_SAD.rep[[b]][[c]]) <- c("KS_log.N","KS_N","KS_ac_obs","KS_ac_sim","KS_ac_diff","rep","SiteCode", "Sindrome")
  }
}
#acoplando os df_KS_SAD.rep de cada conjunto de simulações para um mesmo valor de dispersão
for(d in 1:length(df_SAD.rep) ){
  df_SAD.rep[[d]] %<>% rbind.fill
}

#concatenando em um único df
df_SAD.rep %<>% rbind.fill
names(df_SAD.rep)
str(df_SAD.rep)
#load("resultados.Rdata")

## ks.test ##
df_SAD.rep1 <- lista_dispersal
for(b in 1:length(df_SAD.rep1) ){
  for(c in 1:length(df_SAD.rep1[[b]]) ){
    l_SAD.rep <- df_SAD.rep1[[b]][[c]] 
    l_SAD.rep %<>% apply(., 1, func_apply)
    sitecode <- gsub("SAD_sim_","",names(df_SAD.rep1[[b]])[c])
    df_SAD.obs.f %>% filter(SiteCode == sitecode) %>% arrange(N) %>% .$N %>% log -> SAD.obs
    l_SAD.rep <- lapply(l_SAD.rep, function(x) suppressWarnings(ks.test(x = SAD.obs, y = log(x$N) ) ) )
    df_SAD.rep1[[b]][[c]] <- lapply(l_SAD.rep, function(x) data.frame(KS = x$statistic, KS_p = x$p.value))
  }
}

rep <- 1:100
for(b in 1:length(df_SAD.rep1) ){
  for(c in 1:length(df_SAD.rep1[[b]]) ){
    df_SAD.rep1[[b]][[c]] %<>% rbind.fill
    df_SAD.rep1[[b]][[c]] <- cbind(df_SAD.rep1[[b]][[c]], rep ,gsub("SAD_sim_","",names(df_SAD.rep1[[b]])[c]), names(df_SAD.rep1)[b])
    names(df_SAD.rep1[[b]][[c]]) <- c("KS","KS_p","rep","SiteCode", "Sindrome")
  }
}

#acoplando os df_KS_SAD.rep de cada conjunto de simulações para um mesmo valor de dispersão
for(d in 1:length(df_SAD.rep1) ){
  df_SAD.rep1[[d]] %<>% rbind.fill
}

#concatenando em um único df
df_SAD.rep1 %<>% rbind.fill
names(df_SAD.rep1)
str(df_SAD.rep1)

#inner_join df_SAD.rep e df_SAD.rep1 com df_resultados 
df_resultados$rep <- "sad_media"
df_S.sim0$rep <- as.character(rep(1:100, times = 752))
names(df_S.sim0)[3] <- "Sindrome"
df_SAD.rep %<>% inner_join(.,y=df_resultados[,c(1:4,9,16)], by = c("SiteCode", "Sindrome"))
df_SAD.rep %<>% inner_join(., y = df_S.sim0[,-4], by = c("SiteCode", "Sindrome", "rep"))
df_SAD.rep1$rep %<>% as.character()
df_SAD.rep %>% inner_join(.,y = df_SAD.rep1, by = c("rep", "SiteCode", "Sindrome")) -> df_resultados.rep
df_resultados.rep %<>% select(rep, SiteCode, Sindrome, cluster_medio, cobertura, aggregation.index, S, U.est, KS, KS_p, KS_log.N, KS_N, KS_ac_obs, KS_ac_sim, KS_ac_diff)
df_resultados %>% select(rep, SiteCode, Sindrome, cluster_medio, cobertura, aggregation.index, S_medio, U.est, KS, KS_p, KS_log.N, KS_N, KS_ac_obs, KS_ac_sim, KS_ac_diff) -> a
names(a)[7] <- "S"
df_resultados.rep %<>% rbind(.,a)
df_simulacao %>% select(SiteCode, S_obs)-> a
names(a)[2] <- "S"
a$rep <- "obs"
df_resultados.rep %<>% rbind.fill(., a)

save(df_simulacao, df_resultados.rep, df_resultados, df_SAD.sim0, df_S.sim0, df_SAD.rep, df_SAD.rep1, df_SAD.obs.f, file = "resultados.Rdata")


####### Replicas #########
df_resultados.rep[df_resultados.rep$rep == "obs",] %>% select(-(cobertura), -(aggregation.index)) -> df_temp 
df_resultados.rep %>% filter(rep == "sad_media" & Sindrome == "wind") %>% select(SiteCode, cobertura, aggregation.index) -> df_temp1
df_temp %<>% inner_join(.,df_temp1)
df_temp[,c(1:4,14:15,5:13)] %>% names == df_resultados.rep %>% names
df_temp <- df_temp[,c(1:4,14:15,5:13)]
df_resultados.rep %>% na.omit %>% rbind(.,df_temp) -> df_resultados
df_resultados %>% filter(rep == "obs") %>% select(SiteCode, S) -> df_temp
names(df_temp)[2] <- "S_obs"
df_resultados %<>% inner_join(.,df_temp, by = "SiteCode") %>% na.omit

####### Media das réplicas #########
df_resultados %>% select(rep, SiteCode, Sindrome) %>% duplicated() -> a
df_resultados[!a,] %>% filter(rep != "sad_media" & rep != "media_rep") -> df_ae

#df_ae %>% ddply(.,"rep", summarise, )
df_ae <- ddply(df_ae, c("SiteCode", "Sindrome","cluster_medio", "cobertura", "aggregation.index"), summarise, S = mean(S), 
               U.est = mean(U.est), 
               KS = mean(KS), 
               KS_p = mean(KS_p), 
               KS_log.N = mean(KS_log.N), 
               KS_N = mean(KS_N),
               KS_ac_obs = mean(KS_ac_obs),
               KS_ac_sim = mean(KS_ac_sim),
               KS_ac_diff = mean(KS_ac_diff) ,
               S_obs = mean(S_obs))
rep <- rep("media_rep", dim(df_ae)[1])
df_ae <- cbind(rep, df_ae)
df_resultados <- rbind(df_resultados[df_resultados$rep != "media_rep",], df_ae)

```

############### Auditando os dados ###############
i) KS e KS_ac_diff devem ter valores muito próximos
ii) KS_N tem que ser inteiro se KS_ac_sim for maior que 0

```{r auditoria KS}
#considerando apenas a SAD_media
str(df_resultados.rep)
summary(df_resultados.rep)
df_resultados.rep %>% filter(KS_N < 1) %>% arrange(KS_N) #não é para ter KS_N < 1 se KS_ac_obs > 0 (nenhuma SAD.obs deve ter valores menores do que '1')
df_SAD.obs.f %>% filter(SiteCode == "MGlavr7") %>% tail
#como a SAD.obs de MGlavr7 está incorreta eu vou desconsiderar esse SiteCode dos resultados
df_resultados.rep %<>% filter(SiteCode != "MGlavr7")

df_resultados.rep %>% filter(KS_N > 200) %>% arrange(KS_N)
df_simulacao %>% filter(SiteCode == "SCvolta2")

```

Auditoria df_simulação!
  i) avaliar se o N da planilha é o mesmo N da tabela
ii) verificar como isso pode ter influenciado na densidade

```{r auditoria df_simulada}
#contando o número de espécies
df_resultados.rep$SiteCode %>% unique -> sitecodes 
df_SAD.obs.f %>% filter(SiteCode %in% sitecodes) -> df_N.obs
df_N.obs %<>% ddply(.,"SiteCode", summarise, N_obs = sum(N))
df_N.obs %<>% inner_join(.,df_simulacao[,-c(1,3)], by = "SiteCode")
df_N.obs %<>% mutate(diff_N = N_obs - N, DA_obs = N_obs*DA/N)
save(df_N.obs, file = "df_N.obs.Rdata")
```
vou refazer todas as etapas da simulação usando esses dados agora, a influência vai ser pequena acredito pois são poucos sitecodes que difererem
