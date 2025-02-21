# Carregar pacotes necessários
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)

# Criar o banco de dados
# dados <- data.frame(
#   Antibiótico = rep(c("A0", "A0", "A1", "A1"), each = 3),
#   Vitamina_B12 = rep(c("B0", "B1"), each = 6),
#   Repetição = rep(1:3, times = 4),
#   Peso = c(1.30, 1.19, 1.08, 
#            1.26, 1.21, 1.19, 
#            1.05, 1.00, 1.05, 
#            1.52, 1.56, 1.55)
# )

# Converter variáveis categóricas para fatores
# dados <- dados %>%
#   mutate(Antibiótico = factor(Antibiótico),
#          Vitamina_B12 = factor(Vitamina_B12),
#          Repetição = factor(Repetição))

Peso = c(1.30, 1.19, 1.08,
         1.26, 1.21, 1.19, 
         1.05, 1.00, 1.05, 
         1.52, 1.56, 1.55)

Antibiótico = rep(c("A0", "A0", "A1", "A1"), times = 3)

Vitamina_B12 = rep(c("B0", "B1"), times = 6)

Antibiótico;Vitamina_B12

# Resumo dos dados
#summary(dados)

# Análise de variância (ANOVA)
modelo <- aov(Peso ~ Antibiótico * Vitamina_B12)
summary(modelo) 

# obs: nem o tipo de antibiótico nem a vitamina b12 influenciam o peso

# Teste de normalidade dos resíduos
shapiro.test(resid(modelo))

# obs: o residuo segue a normalidade

# Verificação da homogeneidade de variâncias
leveneTest(modelo)

# obs: as variãncias são homogeneas

# Visualização dos dados
ggplot(dados, aes(x = interaction(Antibiótico, Vitamina_B12), y = Peso, fill = Antibiótico)) +
  geom_boxplot() +
  labs(x = "Tratamentos (Antibiótico x Vitamina B12)", y = "Aumento de Peso (Kg)",
       title = "Efeito do Antibiótico e Vitamina B12 no Aumento de Peso de Suínos") +
  theme_minimal()

