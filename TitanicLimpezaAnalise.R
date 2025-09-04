# Carregar pacotes
library(tidyverse)
library(lemon)
library(ggplot2)

# Ler o CSV
titanic <- read.csv("C:\\Users\\aluno\\Documents\\RStudio\\train.csv")

remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  x[x < lower | x > upper] <- NA  # marca outliers como NA
  return(x)
}

# Aplicar em Age e Fare
titanic$Age  <- remove_outliers(titanic$Age)
titanic$Fare <- remove_outliers(titanic$Fare)

# Substituir NA em Age e Fare pela mediana
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)
titanic$Fare[is.na(titanic$Fare)] <- median(titanic$Fare, na.rm = TRUE)

# Visualizar
View(titanic)

# Ajustar margens do gráfico base
par(mar = c(2, 2, 2, 1))

# Gráfico base R
plot(
  x = titanic$Age,
  y = titanic$Fare,
  main = "Dispersão entre Idade e Tarifa (Titanic)",
  xlab = "Idade",
  ylab = "Tarifa",
  col = "blue",
  pch = 19
)

# ---- OU ---- #
# Versão ggplot2 (mais personalizável)
ggplot(titanic, aes(x = Age, y = Fare)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(
    title = "Dispersão entre Idade e Tarifa (Titanic)",
    x = "Idade",
    y = "Tarifa"
  ) +
  theme_minimal()

# Carregar pacotes
library(ggplot2)
library(dplyr)

# Gráfico de barras empilhadas normalizado a 100%
ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "fill") +  # fill = normaliza para 100%
  scale_y_continuous(labels = scales::percent) +  # eixo Y em porcentagem
  labs(
    title = "Taxa de sobrevivência por Classe (Titanic)",
    x = "Classe do Passageiro",
    y = "Proporção (%)",
    fill = "Sobreviveu"
  ) +
  scale_fill_manual(
    values = c("0" = "red", "1" = "darkgreen"),
    labels = c("Não Sobreviveu", "Sobreviveu")
  ) +
  theme_minimal()



