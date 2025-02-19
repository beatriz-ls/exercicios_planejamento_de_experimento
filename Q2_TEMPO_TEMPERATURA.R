# Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# Criar o banco de dados
dados <- data.frame(
  x1_cod = c(-1, -1, 1, 1, 0, 0, 0, 0, 0),
  x2_cod = c(-1, 1, -1, 1, 0, 0, 0, 0, 0),
  X1 = c(80, 80, 90, 90, 85, 85, 85, 85, 85),
  X2 = c(170, 180, 170, 180, 175, 175, 175, 175, 175),
  Y = c(76.5, 77.0, 78.0, 79.5, 79.9, 80.3, 80.0, 79.7, 79.8)
)

# Ajustar o modelo de regressão linear de primeira ordem
modelo <- lm(Y ~ X1 + X2, data = dados)

# Exibir o resumo do modelo
summary(modelo)

# Tabela ANOVA
anova(modelo)

# Criar superfície de resposta
grid <- expand.grid(
  X1 = seq(80, 90, length.out = 30),
  X2 = seq(170, 180, length.out = 30)
)
grid$Y_pred <- predict(modelo, newdata = grid)

# Plotar a superfície de resposta
ggplot(grid, aes(x = X1, y = X2, fill = Y_pred)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Superfície de Resposta",
       x = "Tempo (X1)",
       y = "Temperatura (X2)",
       fill = "Resposta Y") +
  theme_minimal()