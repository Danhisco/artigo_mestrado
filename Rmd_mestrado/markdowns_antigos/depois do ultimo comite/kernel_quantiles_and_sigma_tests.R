source("utility_functions.R")
## Verificando precisao das estimativas de quantil em funcao do n de pontos
qkernel(10, "laplace", p=0.99, npoints=1e2)
qkernel(10, "laplace", p=0.99, npoints=1e3)
qkernel(10, "laplace", p=0.99, npoints=1e4)
qkernel(10, "laplace", p=0.99, npoints=1e5)
qkernel(10, "laplace", p=0.99, npoints=1e6)
qkernel(100, "laplace", p=0.99, npoints=1e2)
qkernel(100, "laplace", p=0.99, npoints=1e3)
qkernel(100, "laplace", p=0.99, npoints=1e4)
qkernel(100, "laplace", p=0.99, npoints=1e5)
qkernel(100, "laplace", p=0.99, npoints=1e6)
## Parece que 1e5 pontos dá uma boa aproximação

## Alguns testes da funcao que acha as distâncias
## Se tiver um aviso:
## Error in uniroot(f1, lower = sigma.min, upper = sigma.max) : 
##  f() values at end points not of opposite sign
## Tente aumentar o intervalo sigma.min sigma.max (deve ser aumentando sigma.max)
## Uniforme
## Mediana para 100m de distância
(sigma <- sigkernel("uniform", p = 0.5, distance = 100, sigma.max=300)$root)
## Deve retornar a distância de dispersao igual ao argumento distance acima
qkernel(sigma, kernel="uniform", p=0.5)
## quantil de 99% para 100 m de distância
(sigma <- sigkernel("uniform", p = 0.99, distance = 100, sigma.max=300)$root)
qkernel(sigma, kernel="uniform", p=0.99)

## Normal
## Mediana para 100m de distância
(sigma <- sigkernel("normal", p = 0.5, distance = 100, sigma.min=1e-6, sigma.max=1e6)$root)
## Deve retornar a distância de dispersao igual ao argumento distance acima
qkernel(sigma, kernel="normal", p=0.5)
## quantil de 99% para 100 m de distância
(sigma <- sigkernel("normal", p = 0.99, distance = 100, sigma.min=1e-6, sigma.max=1e6)$root)
qkernel(sigma, kernel="normal", p=0.99)

## Laplace
## Mediana para 100m de distância
(sigma <- sigkernel("laplace", p = 0.5, distance = 100, sigma.min=1e-6, sigma.max=1e6)$root)
## Deve retornar a distância de dispersao igual ao argumento distance acima
qkernel(sigma, kernel="laplace", p=0.5)
## quantil de 99% para 100 m de distância
(sigma <- sigkernel("laplace", p = 0.99, distance = 100, sigma.min=1e-6, sigma.max=1e6)$root)
qkernel(sigma, kernel="laplace", p=0.99)

## Distancias pequenas: parecem dar um erro maior (arredondamento?)
## Mediana para 5m de distância
(sigma <- sigkernel("uniform", p = 0.5, distance = 5, sigma.max=300)$root)
## Deve retornar a distância de dispersao igual ao argumento distance acima
qkernel(sigma, kernel="uniform", p=0.5)
## quantil de 99% para 100 m de distância
(sigma <- sigkernel("uniform", p = 0.99, distance = 5, sigma.max=300)$root)
qkernel(sigma, kernel="uniform", p=0.99)
## Aumentando o n de pontos simulados: nao muda muito
(sigma <- sigkernel("uniform", p = 0.99, distance = 5, sigma.max=300, npoints=1e6)$root)
qkernel(sigma, kernel="uniform", p=0.99)

## Normal
## Mediana para 5m de distância
(sigma <- sigkernel("normal", p = 0.5, distance = 5, sigma.max=300)$root)
## Deve retornar a distância de dispersao igual ao argumento distance acima
qkernel(sigma, kernel="normal", p=0.5)
## quantil de 99% para 100 m de distância: note que o erro é o mesmo do kernel uniforme!
(sigma <- sigkernel("normal", p = 0.99, distance = 5, sigma.min=1e-6,)$root)
qkernel(sigma, kernel="normal", p=0.99)

## Laplace
## Mediana para 5m de distância
(sigma <- sigkernel("laplace", p = 0.5, distance = 5, sigma.max=300)$root)
## Deve retornar a distância de dispersao igual ao argumento distance acima
qkernel(sigma, kernel="laplace", p=0.5)
## quantil de 99% para 100 m de distância: note que o erro é o mesmo do kernel uniforme!
(sigma <- sigkernel("laplace", p = 0.99, distance = 5, sigma.min=1e-6,)$root)
qkernel(sigma, kernel="laplace", p=0.99)


## Há um erro que é proporcionalmente maior quando as distâncias são
## pequenas. Acho que é arredondamento que está na função de quantil,
## que faz ela andar aos saltos. Mas a boa notícia é que o erro é o mesmo para todos os
## kerneis. Então, apesar da função não achar exatamente o sigma
## necessário para que p indivíduos caiam até distance de distância, o
## sigma encontrado resulta na mesma distância (quantil), o que torna
## os sigmas calculados com os mesmos parametros p e distance equivalentes.
