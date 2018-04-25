library(rmutil)
## Nota: ainda falta arredondamento devido à quadrícula
## Para uma densidade de 1400 individuos/ha qual a distancia entre os individuos
d_ind_MA  <- 100/sqrt(1400)
## Estimando quantis de distancia para coordenadas sorteadas independentes de laplacianas
sigma <- 4 ## valor de kernel 
b <- sigma*sqrt(2) ## relação entre sigma e b, a variância da laplaciana
X <- d_ind_MA * round(rlaplace(1e6, s=b) / d_ind_MA) # gerando 1e6 valores de uma distribuição laplaciana e convertendo para a distância em metros
Y <- d_ind_MA * round(rlaplace(1e6, s=b) / d_ind_MA) # idem para Y
dist_MA <- sqrt(X^2+Y^2) # distância euclidiana de cada ponto, 
## Plot de um amostra de pontos
plot(sample(X,1e4), sample(Y,1e4), cex=0.1)
## Quantis de distancia
quantile(dist_MA, seq(0,1,by=0.05))
length(dist_MA[dist_MA==0]) / length(dist_MA)
mean(dist_MA)

## Para kernel Gaussiano
## Distancia entre individos em BCI: 20852 individuos em 50 ha
d_ind_BCI  <- 100/sqrt(20852/50)
X <- d_ind_BCI * round(rnorm(1e6, sd=55) / d_ind_BCI)
Y <- d_ind_BCI * round(rnorm(1e6, sd=55) / d_ind_BCI)
dist_BCI <- sqrt(X^2+Y^2)
x11()
plot(sample(X,1e4), sample(Y,1e4), cex=0.1)
summary(dist_BCI)
## Quantis de distancia
quantile(dist_BCI, seq(0,1,by=0.05))
length(dist_BCI[dist_BCI==0]) / length(dist_BCI)
mean(dist_BCI)
