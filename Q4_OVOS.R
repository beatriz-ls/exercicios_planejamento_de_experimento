library(ggplot2)
library(dplyr)
library(lmtest)
library(car)

### dados ----------------------------------------------------------------------

dados <- data.frame(
  repeticao = rep(c(1,2,3,4,5,6), each = 4),
  racao = rep(c(1,2), each = 2),
  ambiente = rep(c(1,2), times = 2),
  ovos = c(50,49,42,40,
           52,52,44,40,
           48,50,46,38,
           54,48,43,39,
           52,46,44,41,
           50,45,45,43)
)

dados = dados %>%
  mutate(
    racao = factor(racao),
    ambiente = factor(ambiente)
  )

### anova ----------------------------------------------------------------------

modelo <- aov(ovos ~ racao * ambiente + Error(repeticao/(racao*ambiente)),
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

