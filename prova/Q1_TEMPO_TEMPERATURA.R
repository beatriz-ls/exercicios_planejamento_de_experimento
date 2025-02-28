# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(rsm)

# Criar o banco de dados
dados <- data.frame(
  x1_cod = c(-1, -1, 1, 1, 0, 0, 0, 0, 0),
  x2_cod = c(-1, 1, -1, 1, 0, 0, 0, 0, 0),
  X1 = c(80, 80, 90, 90, 85, 85, 85, 85, 85),
  X2 = c(170, 180, 170, 180, 175, 175, 175, 175, 175),
  Y = c(76.5, 77.0, 78.0, 79.5, 79.9, 80.3, 80.0, 79.7, 79.8)
)

# Ajustar o modelo de regressão linear de primeira ordem
modelo <- rsm(Y ~ FO(x1_cod, x2_cod) + TWI(x1_cod, x2_cod), data = dados)

# Exibir o resumo do modelo
modelo

# Tabela ANOVA
anova(modelo)

# gráfico

contour(modelo, ~x1_cod + x2_cod,
        image = TRUE,
        xlabs = c("x1 (Tempo)", "x2 (Temperatura)"))

persp(modelo, ~ x1_cod + x2_cod,
      theta = 30, phi = 20,
      expand = 0.6,
      col = terrain.colors(50), contours = "colors",
      zlab = "Aumento de peso", xlabs = c("x1 (Tempo)", "x2 (Temperatura)"))

