#library(wooldridge);
library(quantreg);
#library(readxl); # para leer archivos xml, pero vamos a usar el paquete eph
library(eph);
library(dplyr);
library(tidyr)
library(ggplot2)

base2024 <- get_microdata(year = 2024) # para leer usando el paquete eph 
#base2024 <- read_excel('EPH_usu_1er_Trim_2024_xlsx/usu_individual_T124.xlsx')

base2024 <- base2024[base2024$P21>0 &      ## asalariados
                       base2024$CH03 == 01 & ## jefes de hogar
                       base2024$CH04 == 1 &  ## varones
                       25 <= base2024$CH06 & ## edades
                       base2024$CH06 <= 65,]
#1.a
anios_prev <- c(0, 0, 6, 6, 9, 12, 12, 18)
anios_post <- c(0, 6, 9, 12, 12, 15, 18, 22)

base2 <- base2024 %>%
  mutate(
    # Asignar el número de años en base al nivel de educación y condición de finalización
    anios_ed = ifelse(CH13 == 1,
                      anios_post[CH12 + 1],  # +1 para ajustar al índice en R
                      anios_prev[CH12 + 1] + as.numeric(CH14)),
    
    # Calcular el potencial esperado y su cuadrado
    pot_exp = CH06 - anios_ed - 6,
    pot_exp_2 = pot_exp^2
  )

base2 <- base2[complete.cases(base2[, c("pot_exp")]), ] # nos quitamos los na's.

#lm(log(P21) ~ as.factor(CH12)+as.factor(CH07)+as.factor(CH06)+as.factor(AGLOMERADO)-1,data=base2024)
#reg1 <- lm(log(P21) ~ anios_ed+pot_exp+pot_exp_2+as.factor(AGLOMERADO)+as.factor(CH07), data=base2) # 07 estado civil.
#summary(reg1)
#plot(reg1)

regq1 <- rq(log(P21) ~ anios_ed + pot_exp + pot_exp_2 + as.factor(AGLOMERADO) + as.factor(CH07),data=base2,tau =1:9/10,ci = FALSE)
summregq1 <- summary(regq1,se="ker")
plot(summregq1,parm=2)

#regq2 <- rq(log(P21) ~ anios_ed + pot_exp + pot_exp_2, data=base2, tau=1:9/10)
#summregq2 <- summary(regq2,se='ker')
#plot(summregq2)

#regq3 <- rq(log(P21) ~ as.factor(CH12), data=base2,tau=1:9/10) # nivel eduacion CH12
#summregq3 <- summary(regq3,se='ker')
# plot(summregq3)

# plot.summary.rqs 

taus <- 1:9/10

# Correr regresiones cuantil para cada tau
resultados <- lapply(taus, function(tau) {
  rq(log(P21) ~ anios_ed + pot_exp + pot_exp_2 + as.factor(AGLOMERADO) + as.factor(CH07), data = base2, tau = tau)
})

resultados_summary <- lapply(resultados, function(modelo) {
  summary(modelo, se = "boot", R = 10)  # R=1000 usa 1000 réplicas de bootstrap
})

coeficientes <- do.call(rbind, lapply(resultados_summary, function(summ) {
  coef_table <- coef(summ)[c(2,3),c(1,2)]  # Tabla de coeficientes
  cbind(coef_table[, 1],  # Coeficientes
        coef_table[, 1] - 1.96 * coef_table[, 2],  # Límite inferior (95% CI)
        coef_table[, 1] + 1.96 * coef_table[, 2])  # Límite superior (95% CI)
}))

values_anios_ed <- do.call(rbind,lapply(resultados_summary,function(summ){
  coef(summ)[2,1]
}))
anios_ed_inf <- do.call(rbind,lapply(resultados_summary,function(summ){
  coef(summ)[2,1] - 19.6 * coef(summ)[2,2]
}))
anios_ed_sup <- do.call(rbind,lapply(resultados_summary,function(summ){
  coef(summ)[2,1] + 19.6 * coef(summ)[2,2]
}))
df <- data.frame(
  tau = taus,
  value = values_anios_ed,
  v_inf = anios_ed_inf,
  v_sup = anios_ed_sup
)

coef_df <- as.data.frame(Variable = c(rep("anios_ed", 9), rep("pot_exp", 9)))
coef_df <- cbind(coeficientes,rep(1:9, each = 2))
coef_df <- 
  colnames(coef_df) <- c("Coef", "Lower", "Upper")

ggplot(df, aes(x = tau, y = value)) +
  geom_line(color = "blue") +  # Línea de los valores estimados
  geom_ribbon(aes(ymin = v_inf, ymax = v_sup), fill = "lightblue", alpha = 0.4) +  # Intervalos de confianza
  labs(x = "Tau", y = "Valor estimado", 
       title = "Valores estimados con Intervalos de Confianza") +
  ylim(min(df$v_inf) - 0.05, max(df$v_sup) + 0.05)
theme_minimal()

