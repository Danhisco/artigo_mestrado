####Usando a funcao 'focal'####
#library(raster)
require(magrittr)
#require(plyr)
setwd("/home/danilo/Documents/dados/mestrado/simulacao/auditoria_matriz")
###Motivacao: as matrizes binárias geradas possuem alguns buracos de 'nao-habitat' no meio de grandes fragmentos e no meio da matriz há ilhas de mono habitats;
###objetivo: tapar buracos dos grandes fragmentos e acabar com as ilhas de mono habitat
teste_focal <- read.csv("landscape_sim_987.txt", header = FALSE, sep = " ")
teste_focal %<>% as.matrix
teste_focal[teste_focal==2] <- 1
image(teste_focal)

if(length(x[x==1]) == 8) 
  
  
mat_teste <- matrix(sample(c(1,0),100,replace = TRUE),nrow = 10)
mat_teste %>% plot



##Simulando um raster que seria a matriz binária
r <- raster(ncols=9, nrows=9, xmn=0)
r[] <- sample(x = c(1,0), size = ncell(r), replace = TRUE) #prob = c(0.9,0.1), 
# moving_window de 3X3, tem que ser um número impar - aqui defino a janela que a funcao irá considerar
moving_window <- matrix(1,nrow = 3, ncol = 3) 

###Funcao para ser usada dentro do focal
#Se dentro da moving_window tiver mais de dois 1s então retorna 1 caso contrário retorna 0
#focal_fun <- function(x) if(length(x[x==1]) >= 2) {return(1)} else {return(0)}
#focal_fun <- function(x){
#  ifelse(length(x[x==1]) == 1, 0,
#         if(length(x[x==1]) == 8)){1}
#}
  
  

mat_1 <- matrix(c(0,0,0,0,1,0,0,0,0), ncol=3)
mat_8 <- matrix(c(1,1,1,1,0,1,1,1,1), ncol=3)


focal_fun(mat)

a <- matrix(sample(c(1,0),size = 9, replace = TRUE), nrow = 3, ncol = 3 )
b <- matrix(sample(c(1,0), replace = TRUE), nrow = 3, ncol = 3 )
focal_fun(b)

focal_fun(matrix(sample(c(1,0), replace = TRUE), nrow = 3, ncol = 3 ))

r3 <- focal(r,
            w=matrix(1,nrow = 3, ncol = 3), 
            fun = focal_fun,
            pad = TRUE) # pensar em uma função 

r %>% as.matrix
r3 %>% as.matrix 
