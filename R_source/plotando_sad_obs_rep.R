### Plotando RADs obs + réplicas ###
#OBJETIVO: plotar cada RAD por SiteCode com as respectivas réplicas simuladas para cada Sindrome de Dispersão. Ao final cada SiteCode terá 1 arquivo PDF
# contendo os conjuntos de RADs para uma mesmo SiteCode e Sindrome
#NOTA: não salvar nenhum objeto manipulado aqui

library(sads)
library(magrittr)
library(plyr)
library(dplyr)
library(reshape2)

setwd("/home/danilo/Documents/dissertacao/dados/")
#load("/home/danilo/Documents/dissertacao/dados/dados_brutos.Rdata")
#load("/home/danilo/Documents/dissertacao/dados/resultados_DaniloPMori.Rdata")
#rm(df_KS.rep, df_ref)

# criando objetos para o for #
df_temp <- df_resultados[,c(1:9,17,18,20:22)] %>% arrange(cobertura, cluster_medio)
sitecode <- unique(as.character(df_temp$SiteCode))
cluster <- as.character(sort(as.numeric(unique(df_temp$cluster_medio)) ) )
df_temp$legendas <- paste("U =", round(df_temp[,"U.medio"], digits = 4), ";\n",
                          "KS =", round(df_temp[,"KS"], digits = 4), ";\n",
                          "KS.p =", round(df_temp[,"KS.p"], digits = 4), ";\n",
                          "KS.abund =",round(df_temp[,"KS.abund"], digits = 4), ";\n",
                          "KS.ac.obs =",round(df_temp[,"KS.ac.obs"], digits = 4), ";\n",
                          "KS.ac.sim =",round(df_temp[,"KS.ac.sim"], digits = 4), ";\n",
                          "cobertura =",round(df_temp[,"cobertura"], digits = 4), ";\n",
                          "S.obs =",df_temp[,"S.obs"],
                          sep=" ")


### Criando os arquivos .pdf ###

for(a in 1:length(sitecode)){
  pdf(file = paste(sitecode[a], ".pdf", sep =""), width = 11.69*2, height = 7)
  par(mfrow = c(1,8))
  par(mar = c(4,4,2.3,0.8))
  sad.obs <- df_SAD.obs %>% filter(SiteCode == sitecode[a]) %>% .$N.obs %>% sort(., decreasing = TRUE) %>% rad
  df_for1 <- df_SAD.sim %>% filter(SiteCode == sitecode[a]) %>% select(N, rep, cluster_medio)
  for(b in 1:8){
    df_for1 %>% filter(cluster_medio == cluster[b]) -> df_for2
    plot(sad.obs, main = paste(sitecode[a], ", ", cluster[b], "m", sep = ""), type = "n")
    legenda <- df_temp %>% filter(SiteCode == sitecode[a], cluster_medio == cluster[b]) %>% .$legendas
    text(par("usr")[2]*0.5, par("usr")[4]*35,legenda, cex = 0.9)
    for(c in 1:100){
      df_for2 %>% filter(rep == as.character(c)) %>% .$N %>% rad -> sad.sim
      lines(sad.sim, col = "red")
    }
    lines(sad.obs, col = "black")
    KS_abund <- df_temp %>% filter(SiteCode == sitecode[a], cluster_medio == cluster[b]) %>% .$KS.abund
    abline(h=KS_abund, col = "blue")
  }
  dev.off()
}
rm(a,b,c,sad.obs, cluster,sitecode, df_for1, df_for2,legenda,sad.sim,KS_abund)

# armazendo as informações em um df e concatenando todos os .pdf #
pdf.files <- Sys.glob("*.pdf")
pdf.files <- data.frame(SiteCode = gsub(".pdf","",pdf.files), pdf = pdf.files)
df_temp %<>% filter(SiteCode %in% pdf.files$SiteCode)
pdf.files %<>% inner_join(.,y = df_temp[,-15], by = "SiteCode") %>% arrange(cobertura) %>% filter(Sindrome == "media_sin")
pdf.files$pdf %<>% as.character
#pdf.files %<>% arrange(col interesse)
system( paste("pdfjoin --landscape ", paste(pdf.files$pdf, collapse = " "), " -o SAD_cobertura.pdf", sep = ""))


