# pacotes #
library(gridExtra) 
library(MuMIn)
library(ggplot2) 
library(sjPlot)
library(tidyr)
library(broom)
library(purrr)
library(lme4)
library(sads)
library(magrittr)
library(plyr)
library(dplyr)
## dados ##
load("/home/danilo/Documents/dissertacao/dados/df_ad.Rdata")
#load("df_ad.Rdata")
df_ad %<>% select(SiteCode, kernel_percentil, S.obs, p, U, GOF) %>% mutate(K = 1-kernel_percentil)
df_ad %<>% mutate(p.z = (p - mean(p))/sd(p),
                  K.z = (K - mean(K))/sd(K),
                  S.log = log(S.obs), # acrescenta log da riqueza
                  S.z = (S.obs - mean(S.obs))/sd(S.obs),
                  lS.z = (S.log - mean(S.log))/sd(S.log), ## log da riqueza padronizada
                  p.GOF = GOF/100)
names(df_ad)[1:2] <- c("Site","percentil_1cel")
df_ad %<>% mutate(lU = log(U/(1-U)), K.f = factor(K))

## Selecao Modelos ##
l_md <- vector("list",length=10)
names(l_md) <- c("p * K.f + S", "p + K.f + S", "p + S", "K.f + S", "S", "p * K.f", "p + K.f", "p", "K.f", "1")
l_md[[1]] <- lmer(lU ~ p.z * K.f + lS.z + (1 | Site), data = df_ad)
l_md[[2]] <- lmer(lU ~ p.z + K.f + lS.z + (1 | Site), data = df_ad)
l_md[[3]] <- lmer(lU ~ p.z + lS.z + (1 | Site), data = df_ad)
l_md[[4]] <- lmer(lU ~ K.f + lS.z + (1 | Site), data = df_ad)
l_md[[5]] <- lmer(lU ~ lS.z + (1 | Site), data = df_ad)
l_md[[6]] <- lmer(lU ~ p.z * K.f + (1 | Site), data = df_ad)
l_md[[7]] <- lmer(lU ~ p.z + K.f + (1 | Site), data = df_ad)
l_md[[8]] <- lmer(lU ~ p.z + (1 | Site), data = df_ad)
l_md[[9]] <- lmer(lU ~ K.f + (1 | Site), data = df_ad)
l_md[[10]] <- lmer(lU ~ 1 + (1 | Site), data = df_ad)
# selecao #
AICctab(l_md, weights = TRUE)
# R2 marginal e condicional #
(r2_U <- sapply(l_md, r.squaredGLMM))

## Uma regressao entre lSz e pz 
plot(lS.z ~ p.z, data=df_ad[duplicated(df_ad$Site),]) ## relacao linear em log(S)
lm1 <- lm(lS.z ~ p.z, data=df_ad[duplicated(df_ad$Site),])
cf1 <- unname(coef(lm1)) ## guarda os coeficientes dessa regressao para usar no bootstrap

## Intervalo de Confiança Bootstrap ##
## Comentei o seu códigom original
## df_newdat <- expand.grid(Site=df_ad$Site[1], 
##                          K.f=unique(df_ad$K.f), 
##                          p.z = seq(min(df_ad$p.z)*1.1,max(df_ad$p.z)*1.1, length=50),
##                          lS.z = seq(min(df_ad$lS.z)*1.1,max(df_ad$lS.z)*1.1, length=50))
df_newdat <- expand.grid(Site=df_ad$Site[1], 
                         K.f=unique(df_ad$K.f), 
                         p.z = seq(min(df_ad$p.z)*1.1,max(df_ad$p.z)*1.1, length=50))
## Adiciona a média de log(S) padronizado para cada cobertura, estimado pela regrassao Sxp
df_newdat %<>% mutate(lS.z = cf1[1] + cf1[2]*p.z) %>% as.data.frame()
## Passo 2: crie as função que devem ser calculadas dos modelos a cada simulação
## Previstos por efeitos fixos e aleatórios
f1 <- function(.) predict(., newdata=df_newdat)
## Previstos por efeitos fixos (argumento re.form=~0)
f2 <- function(.) predict(., newdata=df_newdat, re.form=~0)
## Os dois bootstraps. Ajuste o argumento ncpus para o numero de cores de seu computador
md_U <- l_md[["p * K.f + S"]]
b3 <- bootMer(md_U, FUN = f1, nsim=1000, parallel="multicore", ncpus=4)
b4 <- bootMer(md_U, FUN = f2, nsim=1000, parallel="multicore", ncpus=4)
## calcula as médias e intervalos de confiança quantílicos para cada combinação de preditoras
## no novo conjunto de dados
df_newdat$p <- df_newdat$p.z*sd(df_ad$p) + mean(df_ad$p)
df_newdat$mean <- apply(b3$t,2,mean)
df_newdat$IC.low <- apply(b3$t,2,quantile, 0.025)
df_newdat$IC.upp <- apply(b3$t,2,quantile, 0.975)
df_newdat$mean.fixed <- apply(b4$t,2,mean)
df_newdat$IC.low.fixed <- apply(b4$t,2,quantile, 0.025)
df_newdat$IC.upp.fixed <- apply(b4$t,2,quantile, 0.975)


## Plots de logito(U) x cobertura standardizada com intervalos de predição ##
## PI: note que a incerteza total diminuiu um pouco com a adicao da riqueza ao modelo
## (as faixas cinza claro são mais largas em sua figura antiga, que nao tinha este efeito)
df_ad %>%
  ggplot(aes(x=p,y=lU)) + 
  geom_ribbon(aes(y = mean, ymin=IC.low, ymax=IC.upp), data=df_newdat, col="gray", alpha=0.5) +
  geom_ribbon(aes(y=mean, ymin=IC.low.fixed, ymax=IC.upp.fixed), data=df_newdat, col="gray", alpha=0.5) +
  geom_line(aes(x=p, y=mean.fixed), data=df_newdat) +
  geom_point() +
  labs(x="% cobertura vegetal", y = "logito(U)") +
  facet_wrap(~K.f)
