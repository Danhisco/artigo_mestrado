library(png)


#Função que sorteia um valor de c(0,1), segundo o valor do elemento da matriz de entrada; o output é uma matriz de mesmo tamanho
funcao_escolha <- function(matriz) {
  matriz_transformada <- matrix(ncol = ncol(matriz), nrow = nrow(matriz),
                                data = sapply(matriz, function(x) sample(c(2,0),1,prob = c(x,1-x))))
  return(matriz_transformada)
}

#par(mar=rep(0,4),xpd=NA) #essa configuração de par deixa apenas a imagem dentro do dispositivo, sem margem nem nada

rfid532 <- readPNG(source = "RFID532.png", native = FALSE, info = FALSE)
paisagem_532 <- rfid532[,,1]/max(rfid532[,,1]) #como os valores são muito próximos de 0 eu reescalonei os valores, eu acho que tem problemas nessa abordagem, vou usar a outra
paisagem_532 <- funcao_escolha(paisagem_532)
write(x = paisagem_532, file = "landscape_532.txt",sep = " ")
raio_kernel_532 <- 84.72107*sqrt(length(paisagem_532))/500
dim(paisagem_532)


rfid677 <- readPNG(source = "RFID677.png", native = FALSE, info = FALSE)
paisagem_677 <- rfid677[,,1]/max(rfid677[,,1])
paisagem_677 <- funcao_escolha(paisagem_677)
write(x = paisagem_677, file = "landscape_677.txt",sep = " ")
raio_kernel_677 <- 84.72107*sqrt(length(paisagem_677))/500 #aqui eu to dividindo a raiz quadrada da imagem para obter o valor do "lado" (essa imagem é retangular), 500 é quanto é o lado do recorte espacial (são 25ha de área)
#assim, eu sei quantos pixels correspondem em metros. Agora eu sei quantos pixels tem a largura do kernel. 84.72107 é a media do kernel de Sapium marmieri
dim(paisagem_677)

rfid855 <- readPNG(source = "RFID855.png", native = FALSE, info = FALSE)
paisagem_855 <- rfid855[,,1]/max(rfid855[,,1])
paisagem_855 <- funcao_escolha(paisagem_855)
write(x = paisagem_855, file = "landscape_855.txt",sep = " ")
raio_kernel_855 <- 84.72107*sqrt(length(paisagem_855))/500
dim(paisagem_855)

rfid2568 <- readPNG(source = "RFID2568.png", native = FALSE, info = FALSE)
paisagem_2568 <- rfid2568[,,1]/max(rfid2568[,,1])
paisagem_2568 <- funcao_escolha(paisagem_2568)
write(x = paisagem_2568, file = "landscape_2568.txt",sep = " ")
raio_kernel_2568 <- 84.72107*sqrt(length(paisagem_2568))/500
(paisagem_2568)
