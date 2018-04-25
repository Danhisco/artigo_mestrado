setwd("/home/danilo/Documents/dissertacao/dados/imagens/paisagens_selecionadas/") #pasta com os .txts (matriz trinária)
load("simulacao_dfs_e_funcao.Rdata") #Rdata com as informações necessárias para rodar as paisagens

#Coutinho, estou tendo um problema com o meu código. Sempre que tento rodar as simulações encontro o seguinte problema:
# Error in dim(r) <- c(N_simul, land_dims) : dims [produto 1234321] não corresponde ao comprimento do objeto [2977]
#pelo o que eu entendi há menos elementos do que deveria ter, correto? Eu já auditei as paisagens (desde os arquivos tifs até a matriz trinária) diversas vezes, 
#acho que o erro não está nelas.

range <- df_dispersal[1,2] * df_simulacao$conversao_1m #crio vetor que será usado no próximo for para indicar qual o valor do range do kernel de dispersao
aviao <- list() #lista que será usada como receptor intermediário para a matriz de SADreplica e para o valor de U_est
aviao <- dinamica_coalescente(U = 2.5e-05, 
                              S = df_simulacao[1,"S_obs"], 
                              N_simul = 1, 
                              seed = 10000, 
                              disp_range = range[1], 
                              disp_kernel = 2, 
                              landscape = df_simulacao[1, "paisagens"])
lista_SAD.sim[[l]] <- aviao$r #matrix com as SAD replicas
df_U.est[l,i+1] <- aviao$U_est #valor da taxa de imigração estimada
lista_range[[i]] <- SAD_sim


### loop completo ### 

for(i in 1:length(lista_dispersal) ) {
  range <- df_dispersal[i,2] * df_simulacao$conversao_1m #crio vetor que será usado no próximo for para indicar qual o valor do range do kernel de dispersao
  aviao <- list() #lista que será usada como receptor intermediário para a matriz de SADreplica e para o valor de U_est
  for(l in 1:dim(df_simulacao)[1]){
    aviao <- dinamica_coalescente(U = 2.5e-05, 
                                  S = df_simulacao[l,"S_obs"], 
                                  N_simul = 200, 
                                  seed = 10000, 
                                  disp_range = range[l], 
                                  disp_kernel = 2, 
                                  landscape = df_simulacao[l, "paisagens"])
    lista_SAD.sim[[l]] <- aviao$r #matrix com as SAD replicas
    df_U.est[l,i+1] <- aviao$U_est #valor da taxa de imigração estimada
  }
  lista_range[[i]] <- SAD_sim
}
