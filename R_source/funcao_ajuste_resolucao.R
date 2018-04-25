###Script para automatizar o ajuste da resolução das imagens
setwd("~/Documents/dados/mestrado/imagens/imagens_brutas")

##Selecionando os arquivos 
refID_resolucao <- as.numeric(gsub("[^0-9]","",Sys.glob("refID_*.png"))) ## identificando os refIDs necessários
treeco <- read.table(file = "///home/danilo/treeco_paisagens_selecionadas.txt", header = T, sep = "\t", dec = ".") ## carregando a planilha com os valores de DA
resolucao <- treeco[treeco$refID %in% refID_resolucao, c("refID","DA")] ## somente o necessario


for(i in 1:length(refID_resolucao)){ #Rodando e cantando em ritmo de festa o ajuste da resolução
  system(paste("convert refID_", 
         as.character(resolucao[i,1]),".tif"," -resize ",
         as.character(resolucao[i,2]*500),"@ ",
         as.character(resolucao[i,1]), "_landscape.png", sep = ""))
}

system("mv *.png ../imagens_ajustadas/") #movendo as imagens convertidas para a pasta certa
