treeco <- read.table(file = "treeco_paisagens_selecionadas.txt", header = T, sep = "\t", dec = ".")
names(treeco)

sites_refIDs <- table(treeco$refID)
sample_refIDs <- as.numeric(names(sample(sites_refIDs[sites_refIDs==1], size = 30)))
sample_refIDs <- sort(sample_refIDs)

treeco[treeco$refID %in% sample_refIDs, c("refID","S","N","DA","SiteCode","lat_correct","long_correct","lat","long")]
treeco[which(is.na(treeco$lat_correct)),c("lat_correct","long_correct")] <- treeco[which(is.na(treeco$lat_correct)),c("lat","long")]

coordenadas_sample_refID <- treeco[treeco$refID %in% sample_refIDs, c("refID","S","N","DA","SiteCode","lat_correct","long_correct","lat","long")]
coordenadas_sample_refID[which(is.na(coordenadas_sample_refID$lat_correct)),c("lat_correct","long_correct")] <- coordenadas_sample_refID[which(is.na(coordenadas_sample_refID$lat_correct)),c("lat","long")]
coord_sample_refID <- coordenadas_sample_refID[,c(1,6,7)]
names(coord_sample_refID)[2:3] <- c("lat","long")


write.table(coord_sample_refID, file = "coord_sample_refID.txt", sep = "\t", dec = ".", row.names = FALSE)