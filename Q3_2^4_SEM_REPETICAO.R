library(ggplot2)
library(dplyr)
library(lmtest)
library(car)

### Criar o banco de dados------------------------------------------------------
dados <- data.frame(
  ordem = (1:16),
  temperatura = rep(c(-1,1), times = 8),
  catalisador = rep(c(-1,-1,1,1), times = 4),
  concentracao = rep(c(-1,-1,-1,-1,1,1,1,1), times = 2),
  ph = rep(c(-1,1), each = 8),
  rendimento = c(54,85,49,62,64,94,56,70,52,87,49,64,64,94,58,73)
)

dados <- dados %>% 
  mutate(
    temperatura = factor(temperatura),
    catalisador = factor(catalisador),
    concentracao = factor(concentracao),
    ph = factor(ph)
  )

### Ajustando o modelo ANOVA ---------------------------------------------------

modelo <- aov(rendimento ~ temperatura + catalisador + concentracao + ph, data = dados)
summary(modelo)

anova(modelo)

#obs: qunado roda o modelo interativo da problema

# obs: temperatura, catalisado, e concentração modificam o rendimento

### pressupostos ---------------------------------------------------------------

# Teste de normalidade dos resíduos
shapiro.test(resid(modelo))

# obs: como p < 0.5 há evidencias que os residuos sejam normais

# Teste de homocedasticidade (Breusch-Pagan)

bptest(modelo)

# usei esse teste em vez do levene porque precisa de interação

# Gráficos de resíduos
par(mfrow = c(1, 2))

# Histograma dos resíduos
hist(resid(modelo), main = "Histograma dos Resíduos", xlab = "Resíduos",
     breaks = 8, col = "lightblue")

# QQ-plot dos resíduos
qqnorm(resid(modelo))
qqline(resid(modelo), col = "red")

### comparações multiplas ------------------------------------------------------

# Realizando o teste de Tukey
tukey_resultado <- TukeyHSD(modelo)

# obs: temperatura, catalisado, e concentração modificam o rendimento
