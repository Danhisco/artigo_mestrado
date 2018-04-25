##### Auditoria sad media ######
source("/home/danilo/Documents/dissertacao/R_source/sad_media.R")


df_SAD.rep <- lista_dispersal #dados brutos da simulação
func_apply <-function(X){
                X %<>% table %>% as.data.frame() %>% arrange(desc(Freq)) #transformo cada linha da matrix em um SAD
                data.frame(N = X$Freq, sp_ID = paste("sp.",X$., sep = ""))
}
mat_auditoria <- df_SAD.rep[[sample(1:8,1)]][[sample(1:94,1)]] #pegando apenas um par (SiteCode, Sindrome)
l_SAD.rep <- mat_auditoria %>% apply(., 1, func_apply)
names(l_SAD.rep) <- as.character(1:100,sep="")
dim_mat <- l_SAD.rep %>% sapply(.,function(x) dim(x)[1]) %>% max
mat_rep <- matrix(data = 0, nrow = dim_mat, ncol = 100)
for(i in 1:100){
  mat_rep[1:dim(l_SAD.rep[[i]])[1],i] <- l_SAD.rep[[i]]$N
}
mat_rep %>% apply(.,1, mean) -> SAD_manual
mat_rep %>% apply(.,1, sd) -> SsdD_manual

mat_auditoria %>% sad_media %>% suppressWarnings -> SAD_func
SAD_func == SAD_manual
