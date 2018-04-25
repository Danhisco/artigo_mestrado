###Auditória dos dados do treeco
##vou usar os dados de abundance para ver se os dados do treeco estão certos
require(magrittr)
require(dplyr)

##Carregando os dados
treeco <- read.table(file = "///home/danilo/treeco_paisagens_selecionadas.txt", header = T, sep = "\t", dec = ".")
treeco <- treeco[,c(1,4,6)]
abundance <- read.csv(file = "///home/danilo/abundances-1.csv", header = T, sep = "\t", dec = ".")
abundance %>% names
abundance <- abundance[,c(2,10,18)]
abundance %<>% filter(species.correct != "Mortas", N != 0)

sample_refID <- sort(c(1067, 178, 246, 2817, 437, 619, 677, 831, 847, 868, 870, 887, 889, 89, 8, 987), decreasing = FALSE)

###Comparando a riqueza###
S_abundance <- rep(NA, length(sample_refID) )
for(a in 1:length(sample_refID) ){ S_abundance[a] <- abundance[abundance$RefID == sample_refID[a], ] %>% nrow }
S_treeco <- treeco[treeco$refID %in% sample_refID, 3]

df_S <- data.frame(refID = sample_refID, S_abundance, S_treeco, igual = (S_abundance == S_treeco) ) 

a <- abundance[abundance$RefID == sample_refID[13], c(2,3)] %>% table %>% as.data.frame
a[a$Freq != 0, ] %>% nrow
a[a$Freq == 2,]
abundance %>% filter(RefID == sample_refID[9], species.correct == "Pera glabrata")

###Comparando a abundância###
N_abundance <- rep(NA, length(sample_refID) )
for(b in 1:length(sample_refID) ){ N_abundance[b] <- abundance[abundance$RefID == sample_refID[b], "N"] %>%  sum }
N_treeco <- treeco[treeco$refID %in% sample_refID, 2]

df_N <- data.frame(refID = sample_refID, N_abundance, N_treeco, igual = (N_abundance == N_treeco))

a <- abundance[abundance$RefID == sample_refID[8], c(2,3)]
a %>% names
a[a$species.correct == "Prunus myrtifolia", ] 
a_table <- a$species.correct %>% table %>% as.data.frame
a_table <- a_table[a_table$Freq != 0, ]
a_table[a_table$Freq == 2, ]

####Resumo 8jun2016###
#refID com problemas: 847, 831 e 889 ~> ta zicado, retirar
#refID que tem que arrumar: 437 ~> exluir umas das linhas com Cordia ecalyculata no abundance


