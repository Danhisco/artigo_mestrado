####Auditória de dados simulados
setwd("~/Documents/dados/mestrado/simulacao/resultados/")
require(plyr)
require(dgof)
require(magrittr)
require(reshape2)
require(dplyr)
require(ggplot2)
load("df_dados_geral.Rdata")
load("resultados_sim6jun.Rdata")

###comparação entre os dados da simulacao antiga com a nova
###eu espero que o config1 antigo seja mais próximo do simulado atual (que é config30 mas com valores medios)
###a simulacao antiga é com 16 refIDs a nova é com 13, então primeiro eu defino quais os refIDs que serão usados, depois deixo tudo no mesmo formato
refID_reference <- df_resultados$refID %>% levels %>% as.numeric
df_dados_geral %<>% filter(config == 1, refID %in% refID_reference) %>% select(refID,range,U_est,riqueza,KS,p_value,tree_cover)
df_dados_geral$refID %<>% factor

###Comparando coisas que tem que ser iguais: refID, range, tree_cover
df_resultados == df_dados_geral ##ok!!!!

###comparando, por range, coisas que tem ser próximas: U_est, riqueza, KS
###comparacao visual
x11()
require(gridExtra)

##Riqueza
#. X refID
S_refID_novo <- ggplot(data = df_resultados, aes(x = refID, y = riqueza)) + geom_point(aes(colour = range), size = 2.5) + scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")) + scale_y_continuous(limits = c(40,350) ) + labs(title = "30 replica")
S_refID_antigo <- ggplot(data = df_dados_geral, aes(x = refID, y = riqueza)) + geom_point(aes(colour = range), size = 2.5) + scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")) + scale_y_continuous(limits = c(40,350) ) + labs(title = "1 replicas")
grid.arrange(S_refID_novo,S_refID_antigo, ncol = 2)

#. X tree_cover
S_tree_cover_novo <- ggplot(data = df_resultados, aes(x = tree_cover, y = riqueza)) + geom_point(aes(colour = range), size = 2.5) + scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")) + scale_y_continuous(limits = c(40,350) ) + labs(title = "novo")
S_tree_cover_antigo <- ggplot(data = df_dados_geral, aes(x = tree_cover, y = riqueza)) + geom_point(aes(colour = range), size = 2.5) + scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")) + scale_y_continuous(limits = c(40,350) ) + labs(title = "antigo")
grid.arrange(S_tree_cover_novo, S_tree_cover_antigo, ncol = 2)

##U_est
#. X refID
U_refID_novo <- ggplot(data = df_resultados, aes(x = refID, y = U_est)) + geom_point(aes(colour = range), size = 2.5) + scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")) + labs(title = "novo")
U_refID_antigo <- ggplot(data = df_dados_geral, aes(x = refID, y = U_est)) + geom_point(aes(colour = range), size = 2.5) + scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")) + labs(title = "antigo")
grid.arrange(U_refID_novo,U_refID_antigo, ncol = 2)

#. X tree_cover
U_tree_cover_novo <- ggplot(data = df_resultados, aes(x = tree_cover, y = U_est)) + geom_point(aes(colour = range), size = 2.5) + scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")) + labs(title = "novo")
U_tree_cover_antigo <- ggplot(data = df_dados_geral, aes(x = tree_cover, y = U_est)) + geom_point(aes(colour = range), size = 2.5) + scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")) + labs(title = "antigo")
grid.arrange(U_tree_cover_novo, U_tree_cover_antigo, ncol = 2)

##KS
#. X tree_cover
x11()
KS_novo <- ggplot(data = df_resultados, aes(x = tree_cover, y = KS, colour = range)) +  geom_point() +   geom_smooth(method = lm, fill = NA) +  facet_wrap(~ range, ncol=4) + labs(title = "novo")
KS_antigo <- ggplot(data = df_resultados, aes(x = tree_cover, y = KS, colour = range)) +  geom_point() +   geom_smooth(method = lm, fill = NA) +  facet_wrap(~ range, ncol=4) + labs(title = "antigo")
