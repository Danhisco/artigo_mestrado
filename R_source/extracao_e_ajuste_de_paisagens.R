###Esse script é uma versão de 'data_from_raster.R' de autoria de Renato A. Lima
###coisa pra arrumar nesse script: i) merge todos os rasters necessários (por hora eu vou deixar assim sem fazer), ii) inserir as coordenadas - FEITO!, iii) fazer o recorte em PNG (não rolou, fiz em tiff mesmo, quero ve se consigo mudar isso)
require(raster)
require(rgdal)
require(sp)
require(squash)

list_raster <- list() ##carrego todos os rasters e armazeno em uma lista
raster_names <- Sys.glob("~/Downloads/imagens_satelite/Hansen_GFC2015/*.tif") ##se ficar muito lento, trabalhe de dentro do diretório 
for( i in 1:length(raster_names)){
  list_raster[[i]] <- raster(raster_names[i]) 
  names(list_raster)[i] <- paste("raster",substr(raster_names[i], nchar(raster_names[i])-12, nchar(raster_names[i])-4),sep = "") #só pra ficar bonitinho
}
##raster_paisagem <- do.call(merge, list_raster) #### Danilo 15jun2016: se eu conseguir concatenar todos os raster em apenas um o próximo for deixa de ser necessário e o processo todo ficaria bem mais rápido e direto;
###Danilo 15jun2016: apesar de funcionar com as coordenadas amostradas aqui essa abordagem falhara naqueles pontos cujo recorte esteja localizado em dois raster distintos. 
###Danilo 15jun2016: Uma alternativa que eu imagino é encontrar os pontos que zicam e merge raster em pares (apesar que mesmo em pares zicaram)

sample_surveys <- read.table(file = "coord_sample_refID.txt", sep = "\t", dec = ".", header = T) #carregando o arquivo com as coordenadas
sample_surveys <- sample_surveys[,c(1,3,2)] #não tenho certeza se isso é necessário, mas eu achei que ficava mais clara a minha leitura
coordinates(sample_surveys) <- c("long","lat") 
crs.geo <- CRS("+proj=longlat +ellps=intl +towgs84=-206,172,-6,0,0,0,0 +no_defs")  # geographical, datum WGS84 ###Danilo 14jun2016: CRS - get or set the coordinate reference system of a Raster* object 
proj4string(sample_surveys) <- crs.geo  # define projection system of our data ###Danilo 14jun2016: proj4string é similar ao CRS contudo é compatível com mais pacotes

for(i in 1:length(list_raster)){ #como não consegui merge todos os raster estou usando essa abordagem: 1o crio vetores lógicos para selecionar aqueles refIDs que estão dentro de cada raster
  #2o se há refIDs que estão dentro do raster então eu recorto e salvo o recorte com o nome do refID em arquivo .tif
  log_lat <- sample_surveys$lat <= list_raster[[i]]@extent@ymax & sample_surveys$lat >= list_raster[[i]]@extent@ymin #vetor lógico com refIDs cuja lat estao dentro do raster
  log_long <- sample_surveys$long <= list_raster[[i]]@extent@xmax & sample_surveys$long >= list_raster[[i]]@extent@xmin #vetor lógico com refIDs cuja long estao dentro do raster
  locs <- sample_surveys[log_long & log_lat,] #apenas para tornar a indexação mais clara
  path = "~/Documents/dados/mestrado/imagens/imagens_brutas/"
  dim1=0.025; dim2=0.025 ### ainda não tenho certeza se essas são as dimensões corretas
  
  if(dim(locs)[1] != 0){ 
    for(j in 1:dim(locs)[1]){
      recorte <- as(extent(locs@coords[j,1] - dim1, locs@coords[j,1] + dim1, locs@coords[j,2] - dim2, locs@coords[j,2] + dim2), "SpatialPolygons") #long eh x e lat eh y - que coisa, nao
      raster <- crop(list_raster[[i]], recorte)
      mat_raster <- matrix(data = raster@data@values, ncol = 200)
      savemat(x = mat_raster/100, filename = paste(path,"refID_",locs@data$refID[j],".png",sep = ""))
      } 
  } else { NULL }
}

###Automatizar o ajuste da resolução das imagens
setwd("~/Documents/dados/mestrado/imagens/imagens_brutas")

###Ajustando a resolucao###
refID_resolucao <- as.numeric(gsub("[^0-9]","",Sys.glob("refID_*.png"))) ## identificando os refIDs necessários
treeco <- read.table(file = "///home/danilo/treeco_paisagens_selecionadas.txt", header = T, sep = "\t", dec = ".") ## carregando a planilha com os valores de DA
resolucao <- treeco[treeco$refID %in% refID_resolucao, c("refID","DA")] ## somente o necessario

for(i in 1:length(refID_resolucao)){ #Rodando e cantando em ritmo de festa o ajuste da resolução
  system(paste("convert refID_", 
               as.character(resolucao[i,1]),".png"," -resize ",
               as.character(resolucao[i,2]*500),"@ ",
               as.character(resolucao[i,1]), "_landscape.png", sep = ""))
}
system("mv *_landscape.png ~/Documents/dados/mestrado/imagens/imagens_ajustadas") #movendo as imagens convertidas para a pasta certa