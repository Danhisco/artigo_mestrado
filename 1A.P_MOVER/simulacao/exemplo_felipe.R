# setwd
setwd("simulacao")
# função base
source("source/dinamica_coalescente.R")
# função específica para esta bateria de simulação
f_simCoalescente <- function(U = 1.25e-06, S=100,N_simul=1){
  v <- dinamica_coalescente(U, S, disp_range = 8, N_simul,landscape = "ref1425_NA_2296.txt")
  if(S==0){
    return(v)
  } else{
    v_U <- v$U_est
    return(v_U)
    }
}
## 1o estimar a taxa U: 
## prob. de colonização de uma nova espécie na paisagem por evento de nascimento
v_U <- f_simCoalescente() # obs: para replicar U é necessário invocar a função repetidas vezes 

## 2o obter as SADs preditas:
## gera uma matriz com 1 linha por réplica, e N colunas igual ao número de indivíduos na parcela
mat_SAD <- f_simCoalescente(U = v_U, S=0, N_simul = 100) # aqui é possível replicar a SAD diretamente na função
# para obter a SAD aplique a função table em cada linha da matriz gerada:
table(mat_SAD[1,])