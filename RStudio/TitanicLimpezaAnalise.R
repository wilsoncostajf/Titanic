# Carregar pacotes
library(tidyverse)
library(lemon)
library(ggplot2)
library(dplyr)

# Ler o arquivo CSV
titanic <- read.csv("C:\\Users\\aluno\\Documents\\RStudio\\train.csv")

# Visualizar
View(titanic)

# Função para remover outliers e substituir por mediana
clean_column <- function(x) {
  if (is.numeric(x)) {
    # calcular quartis e IQR
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    
    # marcar outliers como NA
    x[x < lower | x > upper] <- NA
    
    # substituir NA pela mediana
    x[is.na(x)] <- median(x, na.rm = TRUE)
  }
  return(x)
}


# Ajustar margens do gráfico base
par(mar = c(2, 2, 2, 1))

# 1 Gráficos de Barra Empilhados a 100%

            # 1.1 Taxa de Sobrevivência por Classe
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
                values = c("0" = "darkred", "1" = "darkgreen"),
                labels = c("Não Sobreviveu", "Sobreviveu")
              ) +
              theme_minimal()
            
            # 1.2 Sobrevivência por Faixa Etária
            ggplot(titanic, aes(x = AgeGroup, fill = factor(Survived))) +
              geom_bar(position = "fill") +
              scale_y_continuous(labels = scales::percent) +
              labs(
                title = "Taxa de sobrevivência por faixa etária (10 em 10 anos)",
                x = "Faixa Etária",
                y = "Proporção (%)",
                fill = "Sobreviveu"
              ) +
              scale_fill_manual(
                values = c("0" = "darkred", "1" = "darkgreen"),
                labels = c("Não Sobreviveu", "Sobreviveu")
              ) +
              theme_minimal()
            
            # Criar faixas etárias de 10 em 10 anos
            titanic <- titanic %>%
              mutate(AgeGroup = cut(Age, 
                                    breaks = seq(0, max(Age, na.rm = TRUE) + 10, by = 10),
                                    right = FALSE, 
                                    labels = paste(seq(0, max(Age, na.rm = TRUE), 10),
                                                   seq(10, max(Age, na.rm = TRUE) + 10, 10),
                                                   sep = "-")))
            
            
            # 1.3 Gráfico de barras empilhadas 100% sobrevivência x gênero
            ggplot(titanic, aes(x = Sex, fill = Survived)) +
              geom_bar(position = "fill") +
              scale_y_continuous(labels = scales::percent) +
              labs(
                title = "Proporção de Mortalidade e Sobrevivência por Gênero",
                x = "Gênero",
                y = "Proporção (%)",
                fill = "Sobreviveu"
              ) +
              scale_fill_manual(values = c("0" = "darkred", "1" = "darkgreen"),
                                labels = c("Não", "Sim")) +
              theme_minimal()

# 2 Gráficos de Caixa
  
              # 2.1 Idade por Sobrevivência
              ggplot(titanic, aes(x = factor(Survived), y = Age, fill = factor(Survived))) +
                geom_boxplot() +
                labs(
                  title = "Idade dos Passageiros por Sobrevivência",
                  x = "Sobreviveu (0 = Não, 1 = Sim)",
                  y = "Idade"
                ) +
                scale_fill_manual(values = c("0" = "darkred", "1" = "darkgreen")) +
                theme_minimal()
              
              
              # 2.3 Idade por Gênero
              ggplot(titanic, aes(x = Sex, y = Age, fill = Sex)) +
                geom_boxplot() +
                labs(
                  title = "Idade por Gênero",
                  x = "Gênero",
                  y = "Idade"
                ) +
                scale_fill_manual(values = c("male" = "lightblue", "female" = "pink")) +
                theme_minimal()
              

# 3. Histogramas
  
                  # 3.1 Histograma de idades dos Passageiros
                  ggplot(titanic, aes(x = Age)) +
                    geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
                    labs(
                      title = "Distribuição das Idades dos Passageiros (Titanic)",
                      x = "Idade",
                      y = "Frequência"
                    ) +
                    theme_minimal()
                
                  
                  # 3.2 Histogramas de Idade x Gênero Comparados
                  ggplot(titanic, aes(x = Age, fill = factor(Survived))) +
                    geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) +
                    labs(title = "Distribuição da Idade por Sobrevivência")
                  
                  # Transformar variáveis em fator
                  titanic$Sex <- as.factor(titanic$Sex)
                  titanic$Survived <- as.factor(titanic$Survived)
                  
# 4. Gráficos de Dispersão
                  
                # 4.1 Quantidade de Familiares por Tarifa Paga
                  
                  ggplot(titanic, aes(x = FamilySize, y = Fare, color = factor(Survived))) +
                    geom_point(alpha = 0.7, size = 2) +
                    labs(
                      title = "Número de familiares vs Tarifa paga",
                      x = "Número de familiares a bordo",
                      y = "Tarifa paga",
                      color = "Sobreviveu"
                    ) +
                    theme_minimal()
                  
                  # Criar coluna de total de familiares
                  titanic$FamilySize <- titanic$SibSp + titanic$Parch
                  
                  
                  # 4.2 Idade por Quantidade de Familiares
                  
                  ggplot(titanic, aes(x = Age, y = FamilySize, color = factor(Survived))) +
                    geom_point(alpha = 0.7, size = 2) +
                    labs(
                      title = "Idade vs Número de familiares a bordo",
                      x = "Idade",
                      y = "Número de familiares",
                      color = "Sobreviveu"
                    ) +
                    theme_minimal()
                  

                  
                  
























