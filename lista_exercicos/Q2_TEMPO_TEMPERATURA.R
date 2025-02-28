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
  X1 = seq(min(dados$X1), max(dados$X1), length.out = 50),
  X2 = seq(min(dados$X2), max(dados$X2), length.out = 50)
)
grid$Y_pred <- predict(modelo, newdata = grid)

# Converter para matriz para o plotly
X1_mat <- matrix(grid$X1, nrow = 30, ncol = 30)
X2_mat <- matrix(grid$X2, nrow = 30, ncol = 30)
Y_mat  <- matrix(grid$Y_pred, nrow = 30, ncol = 30)

# Criar gráfico 3D
fig <- plot_ly(
  x = X1_mat[,1], 
  y = X2_mat[1,], 
  z = Y_mat, 
  type = "surface"
)

# obs: Dúvida de como criar esse gráfico de superficie

# Adicionar pontos dos dados originais
fig <- fig %>%
  add_markers(x = dados$X1, y = dados$X2, z = dados$Y, 
              marker = list(color = 'black', size = 5))

# Exibir gráfico
fig