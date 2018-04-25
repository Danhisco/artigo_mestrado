###Somentes os plots aqui
setwd("~/Documents/dados/mestrado/simulacao/resultados/")
require(magrittr)
require(ggplot2)
load("resultados_sim6jun.Rdata")

####Plotando os gráficos####
x11()
##Riqueza por refID
#opcao 1
riqueza_refID <- ggplot(data = df_resultados, aes(x = refID, y = riqueza)) + 
  geom_point(aes(colour = range), size = 2.5) + 
  scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"))

riqueza_refID + scale_y_continuous(breaks = NULL)

#opcao 2 de cores
#scale_colour_manual(values=c("red","blue","green","yellow","orange","cyan","violet","black"))

##U_est
#qplot(refID, U_est, data = df_dados, colour = range, facets = config ~ .) #idem
U_refID <- ggplot(data = df_resultados, aes(x = refID, y = U_est)) +
  geom_point(aes(colour = range), size = 2.5)
U_refID + scale_y_continuous(breaks = NULL)

##KS
#Preciso calcular a cobertura vegetal
KS <- ggplot(data = df_resultados, aes(x = tree_cover, y = KS, colour = range)) +
  geom_point() + 
  geom_smooth(method = lm, fill = NA) +
  facet_wrap(~ range, ncol=4)
KS 

ggplot(data = df_resultados, aes(x = tree_cover, y = U_est)) + 
  geom_point(aes(colour = range), size = 2.5) +
  facet_wrap(~range, ncol=4)
