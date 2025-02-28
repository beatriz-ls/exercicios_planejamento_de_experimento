library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(lmtest)

##### dados --------------------------------------------------------------------

dados <- data.frame(
  racao = rep(c("C0", "C1"), each = 12),
  ambiente = rep(c("L0", "L0", "L0","L0","L0","L0",
                       "L1", "L1", "L1", "L1", "L1", "L1"), times = 2),
  repeticao = rep(c(1,2,3,4,5,6), each = 4),
  ovos = c(60 , 62 , 58 , 64 , 62 , 60, #C1L1
               49 , 52 , 50 , 48 , 46 , 45, #C1L0
               42 , 44 , 46 , 43 , 44 , 45,  #C0L0
               40,  40 , 38 , 39 , 41 , 43)
)

# converter variáveis categóricas para fatores
dados <- dados %>%
  mutate(racao = factor(racao),
         ambiente = factor(ambiente))

summary(dados)

### anova ----------------------------------------------------------------------

modelo <- aov(ovos ~ racao * ambiente,
              data = dados)

summary(modelo)

anova(modelo)

### testar pressupostos --------------------------------------------------------

# 1. normalidade

shapiro.test(resid(modelo)) # é normal

# 2. homocedasticidade

leveneTest(modelo) # é homogeneo

# 3. independencia dos erros

dwtest(modelo) # não parece ser dependente

# Plot de Resíduos vs. Valores Ajustados
plot(modelo, which = 1)

# Gráfico de resíduos vs. ordem das observações
plot(residuals(modelo), type = "o", main = "Resíduos vs Ordem")

### comparações multiplas ------------------------------------------------------

# Realizando o teste de Tukey
tukey_resultado <- TukeyHSD(modelo)

# Visualizando os resultados do teste de Tukey
tukey_resultado

# Plotando os resultados do teste de Tukey
plot(tukey_resultado)

