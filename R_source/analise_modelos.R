#################################################################################
########################## Criação e ajuste de Modelos ##########################
#################################################################################

# Objetivo: criar e selecionar os modelos afim de melhor analisar a variável KS. 
# Vou misturar o protocolo proposto por Bolker et al 2008 com algumas práticas propostas por Zuur et al. (como para escolha da distribuição e escolha da melhor estrutura fixa e aleatoria)

### Bolker et al 2008 BOX 4: Procedures - creating a full model ###
# *1)"Specify fixed and random effects; only important interactions"*
# *2)"Choose an error distribution and link function"*
# *3)"Graphical checking: are variances of data (transformed by the link function) homogeneous across categories? 
     #Are responses of transformed data linear with respect to continuous predictors? Are there outlier individuals or groups? 
     #Do distributions within groups match the assumed distribution?"*
# *4)"Fit fixed-effect GLMs both to the full (pooled) data set and within each level of the random factors [28,50]. 
     #Estimated parameters should be approximately normally distributed across groups (group-level parameters can have large uncertainties, 
     #especially for groups with small sample sizes). Adjust model as necessary (e.g. change link function or add covariates)."*
# *5)"Fit the full GLMM."*
# *6)"Recheck assumptions for the final model (as in step 3) and check that parameter estimates and 
     #confidence intervals are reasonable (gigantic confidence intervals could indicate fitting problems). 
     #The magnitude of the standardized residuals should be independent of the fitted values. 
     #Assess overdispersion (the sum of the squared Pearson residuals should be x2 distributed [66,67]). If necessary, change distributions or estimate a scale parameter. 
     #Check that a full model that includes dropped random effects with small standard deviations gives similar results to the final model. 
     #If different models lead to substantially different parameter esti- mates, consider model averaging."*
  
####################################################################
################## preparação dos dados e pacotes ##################
####################################################################

library(gridExtra) 
library(ggplot2) 
library(fitdistrplus)
library(MASS)
library(bbmle)
library(nlme)
library(lme4)
library(magrittr)
library(plyr)
library(dplyr)
setwd("/home/danilo/Documents/dissertacao/dados/")
load("/home/danilo/Documents/dissertacao/dados/resultados_DaniloPMori.Rdata")
#arrumando os dados#
rm(df_va)
df_resultados$cluster_medio %<>% as.numeric 
df_resultados$SiteCode %<>% as.character()
names(df_resultados)[c(2:3,17,20)] <- c("kernel","U", "p", "J")
df_resultados %<>% mutate(Jl = DA * 500, Jl.p = DA * 500 * p)
df_resultados %<>% arrange(p, kernel)
df_resultados$Sindrome %<>% factor(.,levels=c("ballistic","gravity","gyration","wind","media_sin","animal_fruit<2cm","animal_fruit2-5cm","animal_fruit>5cm"))
names(df_ref)[c(7,12,15)] <- c("fitofisio","succession","UC")
df_ref %<>% filter(succession != "capoeira")
df_ref$succession %<>% factor(.,levels=c("secondary","primary/secondary","primary"))

#df de uso
df_md <- inner_join(df_resultados[,c(1,18,4,17,22,20,29,30,2)],df_ref[,c(1,7,12,15)], by = "SiteCode")
df_md$fitofisio %<>% factor #drop levels
df_md$succession %<>% factor
df_md$UC %<>% factor
# df_md %>% head(.,n=10)
# df_md %>% str

####################################################################
########################### Protocolo ##############################
####################################################################

################## 1)"Specify fixed and random effects; only important interactions" ##################
#   -VR: KS  
# -fixed: i) cobertura, ii) S, iii) log(J), iv) log(Jl) v) I(log(Jl*p))
# -random: i) kernel, ii) succession (random or fixed?) , iii) fitofisio, iv) unidade de conservação
# -possíveis interações: 
#   #efeito fixo
#   - log(J) : log(Jl) <!-- questão da variável de exposição: ver email-->
#   #efeito aleatório
#   - p : kernel <!-- # da análise exploratória eu vi que parece haver uma interação entre cobertura e kernel -->
#   - S.obs : kernel <!-- # preciso reler, mas acredito que pode existir uma interação entre fragmentação e débito de extinção (reler os artigos do Campos) -->

################## 2)"Choose an error distribution and link function" ##################
l_md_dist <- vector("list", length = 5) #lista com os modelos para selecionar a distribuição de KS
l_md_dist[[1]] <- glm(KS ~ p + S.obs + log(J) + log(Jl) + succession, data = df_md) #norm
l_md_dist[[2]] <- glm(KS ~ p + S.obs + log(J) + log(Jl) + succession, family = gaussian(link = "log"), data = df_md) #lognorm
l_md_dist[[3]] <- glm(KS ~ p + S.obs + log(J) + log(Jl) + succession, family = Gamma(link = "identity"), data = df_md) #Gamma; identity
l_md_dist[[4]] <- glm(KS ~ p + S.obs + log(J) + log(Jl) + succession, family = Gamma(link = "log"), data = df_md) #Gamma; log
l_md_dist[[5]] <- glm(KS ~ p + S.obs + log(J) + log(Jl) + succession, family = Gamma(link = "inverse"), data = df_md) #Gamma; inverse
AICctab(l_md_dist, weights = TRUE)

################## 3)"Graphical checking" ##################
# 3.a) Are variances of data (transformed by the link function) homogeneous across categories? #
x11()
l_p_KS.categ <- vector("list",4)
l_p_KS.categ[[1]] <- ggplot(df_md, aes(x = fitofisio, y =I(1/KS)) ) + geom_boxplot() + geom_jitter()
l_p_KS.categ[[2]] <- ggplot(df_md, aes(x = succession, y =I(1/KS)) ) + geom_boxplot() + geom_jitter()
l_p_KS.categ[[3]] <- ggplot(df_md, aes(x = UC, y =I(1/KS)) ) + geom_boxplot() + geom_jitter()
l_p_KS.categ[[4]] <- ggplot(df_md, aes(x = Sindrome, y =I(1/KS)) ) + geom_boxplot() + geom_jitter()
do.call(grid.arrange,l_p_KS.categ)

# 3.b) Are responses of transformed data linear with respect to continuous predictors?
p_inv.KS.p<-ggplot(df_md, aes(x = p, y =I(1/KS)) ) + geom_point() + geom_smooth(method = "lm", se = FALSE)

### Do distributions within groups match the assumed distribution? ###

## considerando succession ##
p_KS.categ1 <- fitdist(1/df_md$KS[df_md$succession == "primary"], "gamma")
p_KS.categ2 <- fitdist(1/df_md$KS[df_md$succession == "primary/secondary"], "gamma")
p_KS.categ3 <- fitdist(1/df_md$KS[df_md$succession == "secondary"], "gamma")
par(mfrow=c(1,3))
denscomp(p_KS.categ1, main = "Primary", addlegend =  FALSE, xlab = "1/KS", ylab = "")
denscomp(p_KS.categ2, main = "Primary/secondary", addlegend =  FALSE, xlab = "1/KS", ylab = "")
denscomp(p_KS.categ3, main = "Secondary", addlegend =  FALSE, xlab = "1/KS", ylab = "")
par(mfrow=c(1,1))

## considerando kernel ##
levels_sindrome <- levels(df_md$Sindrome)
par(mfrow=c(2,4))
for(i in 1:8){
  temp<-fitdist(1/df_md$KS[df_md$Sindrome == levels_sindrome[i]], "gamma")
  denscomp(temp, main = levels_sindrome[i], addlegend =  FALSE, xlab = "1/KS", ylab = "")
}
par(mfrow=c(1,1))


#################################### 4)"Fit fixed-effect GLMs both to the full (pooled) data set and within each level of the random factors ####################################

# criando lista que vai armazenar os modelos
l_md_glm <- vector("list", length=(1+8))
# full glm -> full data
l_md_glm[[1]] <- glm(KS ~ p + S.obs + log(J) + log(Jl) + succession, family = Gamma(link = "inverse"), data = df_md)
# full glm -> each level of kernel
for(i in 2:9){
  l_md_glm[[i]] <- glm(KS ~ p + S.obs + log(J) + log(Jl) + succession, family = Gamma(link = "inverse"), data = subset(df_md, Sindrome == levels_sindrome[i-1]))
}
# pegando os coeficientes e armazenando em um df
df_coef <- l_md_glm %>% ldply(.,function(X) X$coefficients)
# plotando os coeficientes e comparando com a distribuição normal
par(mfrow=c(2,4))
for(i in 1:dim(df_coef)[2]){
  temp<-fitdist(df_coef[,i], "norm") #ajustando uma distribuição normal aos dados
  denscomp(temp, main = names(df_coef)[i], addlegend =  FALSE, xlab = "", ylab = "") #plotando o histograma com a curva ajustada
  abline(v=df_coef[i,i], col = "red") #parâmetro estimado a partir do conjunto completo de dados
}
par(mfrow=c(1,1))



######################################## *5)"Fit the full GLMM."* ########################################
# Dado a estrutura fixa, podemos pensar em selecionar o componente aelatório do GLMM. 
a <- glmmPQL(KS ~ p + S.obs + log(J) + log(Jl) + succession, ~ 1|kernel ,family = Gamma(link = "inverse"), data = df_md)
b <- glmmPQL(KS ~ p + S.obs + log(J) + log(Jl) + succession, ~ p|kernel ,family = Gamma(link = "inverse"), data = df_md)
c <- glmmPQL(KS ~ p + S.obs + log(J) + log(Jl) + succession, ~ S.obs|kernel ,family = Gamma(link = "inverse"), data = df_md)
d <- glmmPQL(KS ~ p + S.obs + log(J) + log(Jl) + succession, ~ p|kernel + S.obs|kernel ,family = Gamma(link = "inverse"), data = df_md)





################### Reunião com Renato 24/01 ###################
ggplot(df_md, aes(x=kernel, y=KS, group = SiteCode)) + geom_line(aes(colour=SiteCode)) +theme(legend.position="none")

a <- lm(log(KS) ~ S.obs + succession + fitofisio, df_md)
par(mfrow=c(1,1))
avPlots(a)
b<-lmer(log(KS) ~ S.obs + (1|fitofisio) + (1|succession),data = df_md)
c<-lmer(log(KS) ~ p + (1|kernel) + (S.obs|fitofisio) + (S.obs|succession),data = df_md)
d<-lmer(log(KS) ~ S.obs + (1|fitofisio) + (1|succession),data = df_md)

c<-lmer(log(KS) ~ p + S.obs + (1|kernel),data = df_md)
d<-lmer(log(KS) ~ p + S.obs + (p|kernel),data = df_md)
AICctab()


#### NOTA: 
a<-lmer(log(KS) ~ p + S.obs + (p|kernel),data = df_md)
b<-lmer(log(KS) ~ p + S.obs + (p|kernel),data = df_md)
c<-lmer(log(KS) ~ p + S.obs + (p|kernel) + (1|fitofisio),data = df_md)
d<-lmer(log(KS) ~ p + S.obs + (p|kernel) + (1|fitofisio) + (1|succession),data = df_md)
e<-lmer(log(KS) ~ p + S.obs + (p|kernel) + (1|fitofisio) + (p|succession),data = df_md)
AICctab(a,b,c,d,e)
summary(c)
plot(log(KS) ~ p, df_md)
curve(-1.6185734 + 0.0056947*x, add = T)
abline(lm(log(KS)))








#### testamos a interação S*p mas parece que fitofisio ta dando conta
f<-lmer(log(KS) ~ p * S.obs + (p|kernel),data = df_md)
j<-lmer(log(KS) ~ p * S.obs + (p|kernel),data = df_md)
k<-lmer(log(KS) ~ p * S.obs + (p|kernel) + (1|fitofisio),data = df_md)
l<-lmer(log(KS) ~ p * S.obs + (p|kernel) + (1|fitofisio) + (1|succession),data = df_md)
m<-lmer(log(KS) ~ p * S.obs + (p|kernel) + (1|fitofisio) + (1|succession),data = df_md)

AICtab(a,b,c,d,e,f,j,k,l,m)
summary(d)

plot(resid(b) ~ df_md$S.obs)
summary(b)


ggplot(df_md, aes(x=S.obs,y=log(KS)) ) + geom_point() + geom_smooth(method="lm",se=F) + facet_wrap(~kernel)
