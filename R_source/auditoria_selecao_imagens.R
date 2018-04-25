##### Auditoria seleção de paisagens #####
library(magrittr)
library(dplyr)
library(plyr)
df_treeco <- read.csv(file = "./Documents/Pasta sem título/dados/planilhas/refIDs_selecionados.csv", head =T, sep = "\t",as.is = FALSE, na.strings = c("","NA")) #yes, >= 1ha
df_treeco %>% names
df_treeco <- df_treeco[,c(80,1,86,7,23,24,28,29,30,26,45,46,49,50)]
df_treeco$arrangement %>% table %>% as.data.frame()
df_treeco %<>% filter(arrangement == "contiguous")
#retirando as que não são contíguas ou razoavelmente contínuas
df_simulacao0 %<>% inner_join(x = df_treeco,y = ., by = "SiteCode")


