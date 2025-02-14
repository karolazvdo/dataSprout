##################################################################
##################################################################
##  Hurdle model - analysis and figures                         ##
##  Karoline Azevedo                                            ##
##################################################################
##################################################################
##
# Install and load packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

p_load(pscl, ggplot2, MuMIn, ggpubr, readxl)


# Import data
data <-read_excel("C:/Users/drive/Downloads/filteredDataV2.xlsx") # if it is a file xlsx [change path]

data <- read.csv("data.csv") # if it is a file csv [change name]

# Construction of the hurdle model with different distributions
modelo_negbin <- hurdle(n.registros ~ Distancia.Ponderada + depth | Distancia.Ponderada + depth, 
                        data = vies.pesq, dist = "negbin", na.action = "na.fail")

modelo_poisson <- hurdle(n.registros ~ Distancia.Ponderada + depth | Distancia.Ponderada + depth, 
                         data = vies.pesq, dist = "poisson", na.action = "na.fail")

# Selecting the best model based on AICc
melhor_modelo <- model.sel(modelo_negbin, modelo_poisson)
print(melhor_modelo)

# Summary of the chosen model (assuming it is the one with the lowest AICc)
summary()

# Multiple model selection with dredge [change for your best model select]
resultado_dd <- dredge(modelo_negbin, evaluate = TRUE, rank = "AICc", m.lim = c(1, NA))
summary(resultado_dd)

# Selecting the most parsimonious model
modelo_selecionado <- get.models(resultado_dd, 1)[[1]]
summary(modelo_selecionado)

# Model averaging (média dos modelos dentro de delta < 4)
resultado_sb <- get.models(resultado_dd, subset = delta < 4)
resultado_avg <- model.avg(resultado_sb)
summary(resultado_avg)

# Creating a table with confidence intervals
Confint_anf <- confint(resultado_avg)
beta_anf <- coefficients(resultado_avg)
table_anf <- merge(Confint_anf, beta_anf, by = "row.names")
colnames(table_anf) <- c("Variável", "Inferior", "Superior", "Beta")

# Coloring coefficients based on significance
table_anf$Cor <- ifelse(table_anf$Inferior < 0 & table_anf$Superior < 0, "red", 
                        ifelse(table_anf$Inferior > 0 & table_anf$Superior > 0, "blue", "grey50"))
col <- setNames(as.character(table_anf$Cor), as.character(table_anf$Cor))

# Separating effects of count and zero-hurdle components [check the rows you are going to use]
efeito_contagem <- table_anf[1:3,]
efeito_zero <- table_anf[5:7,]

efeito_contagem$Variável <- factor(efeito_contagem$Variável, levels = efeito_contagem$Variável)
efeito_zero$Variável <- factor(efeito_zero$Variável, levels = efeito_zero$Variável)

# Creating chart for count component
graf_contagem <- ggplot(efeito_contagem, aes(x = Variável, y = Beta, ymin = Inferior, ymax = Superior, color = Cor)) +
  geom_pointrange(alpha = 0.5, size = 0.8) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  ggtitle("Componente de Contagem") +
  ylab("Coeficiente de Regressão (95% CI)") +
  scale_color_manual(values = col) +
  theme_minimal() +
  theme(legend.position = "none")

graf_contagem

## Creating chart for zero-hurdle component
graf_zero <- ggplot(efeito_zero, aes(x = Variável, y = Beta, ymin = Inferior, ymax = Superior, color = Cor)) +
  geom_pointrange(alpha = 0.5, size = 0.8) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  ggtitle("Componente Zero-hurdle") +
  ylab("Coeficiente de Regressão (95% CI)") +
  scale_color_manual(values = col) +
  theme_minimal() +
  theme(legend.position = "none")

graf_zero

## Join both
graf_hurdle <- ggarrange(graf_contagem, graf_zero, labels = c("A", "B"), common.legend = FALSE)
graf_hurdle