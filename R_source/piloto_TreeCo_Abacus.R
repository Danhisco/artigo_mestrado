## Sites Simulados  
load("/home/danilo/Documents/dissertacao/cluster/estimando_kernel/df_resultados-kernel_estimado.Rdata")
df_resultados$U <- NA

percentil <- df_resultados$kernel_percentil %>% unique
DA_max <- df_resultados$DA %>% max
N_max <- df_resultados$N %>% max
p_max <- df_resultados$p %>% max
S_max <- df_resultados$S.obs %>% max

#DA
df_resultados1 <- df_resultados %>% filter(DA == DA_max, kernel_percentil == 0.01)
df_resultados2 <- df_resultados %>% filter(DA == DA_max, kernel_percentil == percentil[10])
df_resultados3 <- df_resultados %>% filter(DA == DA_max, kernel_percentil == 0.90)
#N
df_resultados4 <- df_resultados %>% filter(N == N_max, kernel_percentil == 0.01)
df_resultados5 <- df_resultados %>% filter(N == N_max, kernel_percentil == percentil[10])
df_resultados6 <- df_resultados %>% filter(N == N_max, kernel_percentil == 0.90)
#p
df_resultados7 <- df_resultados %>% filter(p == p_max, kernel_percentil == 0.01)
df_resultados7 <- df_resultados7[1,]
df_resultados8 <- df_resultados %>% filter(p == p_max, kernel_percentil == percentil[10])
df_resultados8 <- df_resultados8[1,]
df_resultados9 <- df_resultados %>% filter(p == p_max, kernel_percentil == 0.90)
df_resultados9 <- df_resultados9[1,]
#S
df_resultados10 <- df_resultados %>% filter(S.obs == S_max, kernel_percentil == 0.01)
df_resultados11 <- df_resultados %>% filter(S.obs == S_max, kernel_percentil == percentil[10])
df_resultados12 <- df_resultados %>% filter(S.obs == S_max, kernel_percentil == 0.90)

df_piloto_ref <- rbind.fill(df_resultados1, df_resultados2, df_resultados3,df_resultados4, df_resultados5, df_resultados6, df_resultados7, df_resultados8, df_resultados9, df_resultados10, df_resultados11, df_resultados12)
rm(df_resultados1,df_resultados2, df_resultados3,df_resultados4, df_resultados5, df_resultados6, df_resultados7, df_resultados8, df_resultados9, df_resultados10, df_resultados11, df_resultados12)

# lendo os Rdatas gerados
Rdatas <- Sys.glob("*-simulado.Rdata")
for(i in 1:length(Rdatas)) {
  load(file=Rdatas[i])
}

# adicionando o tempo de execução #
df_resultados2$user <- 1375.428
df_resultados2$system <- 0.928
df_resultados2$elapse <- 229.968

df_resultados3$user <- 113.540
df_resultados3$system <- 0.796
df_resultados3$elapse <- 229.968

df_resultados6$user <- 499.960
df_resultados6$system <- 1.572
df_resultados6$elapse <- 182.293

df_resultados8$user <- 198.968
df_resultados8$system <- 0.216
df_resultados8$elapse <- 70.306

df_resultados9$user <- 5.212
df_resultados9$system <- 0.263
df_resultados9$elapse <- 3.606

df_resultados12$user <- 163.064
df_resultados12$system <- 0.436
df_resultados12$elapse <- 56.591

df_piloto <- rbind.fill(df_resultados2,df_resultados3,df_resultados6,df_resultados8,df_resultados9,df_resultados12)
df_piloto[,-12] %>% unique
df_piloto %>% names

# juntando #

df_piloto_ref$user <- NA
df_piloto_ref[2,"user"] <- 1375.428
df_piloto_ref[3,"user"] <- 113.540
df_piloto_ref[6,"user"] <- 499.960
df_piloto_ref[8,"user"] <- 198.968
df_piloto_ref[9,"user"] <- 5.212
df_piloto_ref[12,"user"] <- 163.064

car::scatterplotMatrix(~user + p + S.obs + DA + N + kernel_percentil, df_piloto_ref)




