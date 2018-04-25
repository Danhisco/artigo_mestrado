library(ggplot2) 
library(gridExtra) 
load("/home/danilo/Documents/dissertacao/dados/resultados_DaniloPMori.Rdata")

pdf(file="~/Desktop/p_J.pdf")
par(mfrow=c(1,2))

hist(df_resultados$N, # N
     col="chartreuse4",
     border="black",
     prob = TRUE,
     xlab = "indivíduos",
     main = "J",
     ylim=c(0, 7.748e-4 ),
     breaks=32)
hist(log(df_resultados$N), # log(N)
     col="chartreuse4",
     border="black",
     prob = TRUE,
     xlab = "ln(indivíduos)",
     main = "ln(J)",
     breaks=26)

dev.off()

df_ae <- inner_join(df_ref[,c(1,5,9:10,12:15)], unique(df_resultados[,c(1,4,17,2)]), by = "SiteCode")
df_ae$cluster_medio %<>% as.character() %>% as.numeric()
df_ae %<>% arrange(cobertura, cluster_medio)
df_ae$cluster_medio %<>% factor
ggplot(df_ae, aes(x=forest_succession,y=KS)) + geom_boxplot() + geom_jitter(aes(colour=cluster_medio))
ggplot(df_ae, aes(x=cobertura,y=KS, group=forest_succession)) + geom_point(aes(colour=forest_succession)) + geom_smooth(method="lm",se=F,aes(colour=forest_succession) )

.plot(KS ~ forest_succession, df_ae)