###Lista de coisas pra fazer: Rodar as simulações \o/
##Seq: 
#i) ler todos os arquivos (mat_tri e o treeco (dados de DA e S)), 
#ii) construir o disp_range (cada simulacao tem o seu próprio baseado na resolucao de cada sitio),  
#iii) auditoria dos dados da simulacao

#(U, S=0, N_simul, seed, disp_range, disp_kernel, landscape)

setwd("~/Documents/dados/mestrado/simulacao/matriz_trinaria")
source("~/Documents/dissertacao/dinamica/dinamica_coalescente.R")
require(magrittr)
require(dplyr)

#mean_dispersal <- sample(c(195,17.4,5378,7.82,6104,996,163),size = 1) #Clark et al. 1999. Seed dispersal near and far... vou pegar o maior valor 
#mean_dispersal <- mean(c(195,17.4,5378,7.82,6104,996,163)) ##realmente to sem criatividade
dispersal <- c(195,17.4,5378,7.82,6104,996,163)

landscapes <- Sys.glob("*.txt") ##todas as matrizes
landscapes <- landscapes[-c(8,9,13)]
treeco <- read.table(file = "///home/danilo/treeco_paisagens_selecionadas.txt", header = T, sep = "\t", dec = ".") ## dados do treeco
refIDs <- as.numeric(gsub("[^0-9]","",landscapes))

treeco %>% filter(refID %in% refIDs) %>% select(refID, S)

riqueza <- treeco[treeco$refID %in% as.numeric(gsub("[^0-9]","",landscapes)),"S"] # a riqueza de cada site
p_m <- sqrt(treeco[treeco$refID %in% as.numeric(gsub("[^0-9]","",landscapes)),"DA"]*500)/5000


lista_range <- vector("list", length(dispersal))
names(lista_range) <- dispersal

SAD_sim <- vector("list",length(landscapes)) #lista vazia para o for
names(SAD_sim) <- paste("SAD_sim_",gsub("[^0-9]","",landscapes),sep = "") #nomes da lista

U_est_ranges <- as.data.frame( matrix(ncol = length(dispersal), nrow = length(landscapes)), row.names = gsub("[^0-9]","",landscapes) )
names(U_est_ranges) <- dispersal

for(i in 1:length(dispersal) ) {
  range <- dispersal[i]*p_m
  laranja <- list()
  for(l in 1:length(SAD_sim)){
    laranja <- dinamica_coalescente(U = 2.5e-05, 
                                    S = riqueza[l], 
                                    N_simul = 200, 
                                    seed = 10000, 
                                    disp_range = range[l], 
                                    disp_kernel = 2, 
                                    landscape = landscapes[l])
    SAD_sim[[l]] <- laranja$r
    U_est_ranges[l,i] <- laranja$U_est
  }
  lista_range[[i]] <- SAD_sim
}

sim_6jun <- list(lista_range,U_est_ranges)
save(sim_6jun, file = "sim_12jun_config200.Rdata")
