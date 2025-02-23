library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(lmtest)

##### dados --------------------------------------------------------------------

dados <- data.frame(
  Antibiótico = rep(c("A0", "A0", "A1", "A1"), times = 3),
  Vitamina_B12 = rep(c("B0", "B1"), times = 6),
  Repetição = rep(1:3, each = 4),
  Peso = c(1.30, 1.26, 1.05, 1.52,
           1.19, 1.21, 1.00, 1.56,
           1.08, 1.19, 1.05, 1.55)
)

# converter variáveis categóricas para fatores
dados <- dados %>%
  mutate(Antibiótico = factor(Antibiótico),
         Vitamina_B12 = factor(Vitamina_B12),
         Repetição = factor(Repetição))


summary(dados)

# Visualização dos dados
ggplot(dados, aes(x = Vitamina_B12,
                  y = Peso,
                  fill = Antibiótico)) +
  geom_boxplot() +
  labs(x = "Tratamentos (Antibiótico x Vitamina B12)", y = "Aumento de Peso (Kg)",
       title = "Efeito do Antibiótico e Vitamina B12 no Aumento de Peso de Suínos") +
  theme_minimal()

##### Análise de variância (ANOVA)----------------------------------------------

modelo <- aov(Peso ~ Antibiótico * Vitamina_B12, data = dados)
summary(modelo) 

# obs: o antibiótico e a vitamina b12 influenciam no peso, sendo que o A diminue 
# o peso sem a vitamina b12 e quando ministrado juntos ambos aumnetam o peso.

##### pressupostos -------------------------------------------------------------

## 1. independencia das observações

# teste durbin watson
dwtest(modelo)

# obs: o teste indica algum nível de dependencia

par(mfrow = c(1, 2))

# Resíduos vs. Índice da observação
plot(resid(modelo), type = "b", pch = 16, col = "blue",
     main = "Resíduos vs. Índice das Observações",
     xlab = "Índice da Observação", ylab = "Resíduo")
abline(h = 0, col = "red", lty = 2)

# Resíduos vs. Valores Ajustados
plot(fitted(modelo), resid(modelo), pch = 16, col = "blue",
     main = "Resíduos vs. Valores Ajustados",
     xlab = "Valores Ajustados", ylab = "Resíduo")
abline(h = 0, col = "red", lty = 2)


## 2. Teste de normalidade dos resíduos

shapiro.test(resid(modelo))

# obs: como p > 0.05 não há evidencias que os resuduos não sejam normais

qqnorm(resid(modelo))

qqline(resid(modelo))

## 3. Verificação da homogeneidade de variâncias

leveneTest(modelo)

# obs: Como p > 0.05, não há evidências de que a variância não é homogenea


