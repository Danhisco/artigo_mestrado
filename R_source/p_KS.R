#INPUT: dois vetores numericos em escala log (duas SADs)
#OUTPUT: i) c.data = valor de abundância em que ocorreu a maior divergência
#        ii) c.data.e = exp(c.data)
#        iii) acumulada.obs = valor da acumulada da curva obs q ocorreu a maior divergência entre as curvas
#        iv) acumulada.sim = valor da acumulada da curva sim q ocorreu a maior divergência entre as curvas
#        v) diff.acumulada = valor da estatística de KS
KS_position <- function(X.obs, X.sim){
                  sad.obs <- X.obs #sad observada
                  sad.sim <- X.sim #sad simulada
                  c_obs.sim <- sort(c(sad.obs,sad.sim)) #os dados concatenados e ordenados
                  df_KS_st <- data.frame(c.data = c_obs.sim, #idem
                                         c.data.e = exp(c_obs.sim), #exp dos valores para tirar da escala log
                                         acumulada.obs = sapply(c_obs.sim, FUN = function(x) length(sad.obs[sad.obs<x])/length(sad.obs)), #valor da acumulada obs
                                         acumulada.sim = sapply(c_obs.sim, FUN = function(x) length(sad.sim[sad.sim<x])/length(sad.sim)) #valor da acumulada sim
                                         ) #ambas são calculadas como o número de observações menores do que o valor de c.data
                  df_KS_st %<>% mutate(diff.acumulada = abs(df_KS_st$acumulada.obs-df_KS_st$acumulada.sim)) #obtendo a diferença não absoluta
                  KS_posicao <- unique(df_KS_st[df_KS_st$diff.acumulada == max(df_KS_st$diff.acumulada),]) 
                  return(KS_posicao) #a função retorna o valor de X onde ocorre a maior divergência e os dois valores da acumulada que levam a maior divergência
}
