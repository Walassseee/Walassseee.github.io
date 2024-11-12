# lib de dados #################################################################

library(dplyr)
library(readxl)
library(corrplot)
library(car)
library(gt)
library(e1071)
library(tibble)
library(ggplot2)
library(ggtext)
library(gridExtra)

# etl de dados #################################################################

# Depending variable
gdp_percapita <- read.csv2("posts/economic_development/gdp-per-capita-worldbank.csv", sep = ",", dec = ".")

# Independing variable
corruption_index <- read.csv2("posts/economic_development/political-corruption-index.csv", sep = ",", dec = ".")

# Control variables
tax_complexity <- read_excel("posts/economic_development/tax-complexity-index.xlsx", sheet = "Original Data")
control_variables <- read.csv2("posts/economic_development/control-variables-wbd.csv", sep = ",", dec = ".")

year <- 2022

gdp_percapita <- gdp_percapita[c("Entity", "Year", "ny_gdp_pcap_pp_kd")]
corruption_index <- corruption_index[c("Entity", "Year", "corruption_vdem__estimate_best__aggregate_method_average")]
tax_complexity <- tax_complexity[c("Country", "Year", "Tax Complexity Index")]
control_variables <- control_variables[c("Country.Name", "Time",
                                         "Primary.completion.rate..total....of.relevant.age.group...SE.PRM.CMPT.ZS.",
                                         "Life.expectancy.at.birth..total..years...SP.DYN.LE00.IN.")]

data <- inner_join(gdp_percapita, corruption_index, by = "Entity")
data <- inner_join(data, control_variables, by = c("Entity" = "Country.Name"))
data <- inner_join(data, tax_complexity, by = c("Entity" = "Country"))

data <- data |> filter(Year == year)

data <- data[c("Entity", "Year",
               "ny_gdp_pcap_pp_kd",
               "Tax Complexity Index",
               "corruption_vdem__estimate_best__aggregate_method_average",
               "Primary.completion.rate..total....of.relevant.age.group...SE.PRM.CMPT.ZS.",
               "Life.expectancy.at.birth..total..years...SP.DYN.LE00.IN.")]

data <- data |> rename(Growth = ny_gdp_pcap_pp_kd,
                       Tax = `Tax Complexity Index`,
                       Corruption = corruption_vdem__estimate_best__aggregate_method_average,
                       Education = `Primary.completion.rate..total....of.relevant.age.group...SE.PRM.CMPT.ZS.`,
                       `Life expectancy` = `Life.expectancy.at.birth..total..years...SP.DYN.LE00.IN.`)

data <- data |> filter(Education != "..")

data$Education <- as.numeric(data$Education)
data$`Life expectancy` <- as.numeric(data$`Life expectancy`)

# eda de dados #################################################################

length(unique(data$Entity))

table_eda <- gt(data = head(data))
table_eda <- table_eda |> tab_header(
  title = "Dados de Corrupção e Crescimento.",
  subtitle = "Valores para o Ano de 2022, contém 43 países"
) |> tab_footnote(
  footnote = "Fonte: Elaboração do autor."
)
table_eda

summary_table1 <- data %>%
  summarize(
    Máximo = max(Growth, na.rm = TRUE),
    Média = mean(Growth, na.rm = TRUE),
    Mediana = median(Growth, na.rm = TRUE),
    `Desvio Padrão` = sd(Growth, na.rm = TRUE),
    Minímo = min(Growth, na.rm = TRUE),
  )
summary_table1 <- as.data.frame(t(summary_table1))
colnames(summary_table1) <- c("Growth")

summary_table2 <- data %>%
  summarize(
    Máximo = max(Corruption, na.rm = TRUE),
    Média = mean(Corruption, na.rm = TRUE),
    Mediana = median(Corruption, na.rm = TRUE),
    `Desvio Padrão` = sd(Corruption, na.rm = TRUE),
    Minímo = min(Corruption, na.rm = TRUE),
  )
summary_table2 <- as.data.frame(t(summary_table2))
colnames(summary_table2) <- c("Corruption")

summary_table <- cbind(summary_table1, summary_table2)
summary_table$Estatística <- rownames(summary_table)
summary_table <- summary_table[c("Estatística", "Growth", "Corruption")]

table_corrup_grownt <- gt(data = summary_table)
table_corrup_grownt <- table_corrup_grownt |> tab_header(
  title = "Médidas Resumo das Variáveis.",
  subtitle = "Variável dependente e explicativa."
) |> tab_footnote(
  footnote = "Fonte: Elaboração do autor."
)
table_corrup_grownt

skew_growth <- skewness(data$Growth, na.rm = TRUE)
skew_corruption <- skewness(data$Corruption, na.rm = TRUE)
kurt_growth <- kurtosis(data$Growth, na.rm = TRUE)
kurt_corruption <- kurtosis(data$Corruption, na.rm = TRUE)
shapiro_growth <- shapiro.test(data$Growth)
shapiro_corruption <- shapiro.test(data$Corruption)

summary_table <- tibble(
  Variável = c("Crescimento Econômico", "Corrupção"),
  "Assimetria (Skewness)" = c(skew_growth, skew_corruption),
  "Curtose" = c(kurt_growth, kurt_corruption),
  "Shapiro-Wilk p-valor" = c(shapiro_growth$p.value, shapiro_corruption$p.value)
)
summary_table <- gt(summary_table) |> tab_header(
  title = "Medidas e Testes de Normalidade",
  subtitle = "Variável dependente e explicativa."
) |> tab_footnote(
  footnote = "Fonte: Elaboração do autor."
)
summary_table

plot_growth <- ggplot(data, aes(x = Growth)) +
  geom_histogram(bins = 20, fill = "#55b748", color = "black", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "#000000", linewidth = 1) +
  scale_y_continuous(labels = NULL) +
  labs(
    title = "Distribuição do Crescimento Econômico",
    subtitle = "Histograma da variável Crescimento Econômico",
    x = NULL, y = NULL,
    caption = "<b>Fonte</b>: Elaboração do autor."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot"
  )

plot_corruption <- ggplot(data, aes(x = Corruption)) +
  geom_histogram(bins = 20, fill = "#1a2e19", color = "black", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "#000000", linewidth = 1) +
  scale_y_continuous(labels = NULL) +
  labs(
    title = "Distribuição do Índice de Corrupção",
    subtitle = "Histograma da variável Índice de Corrupção",
    x = NULL, y = NULL,
    caption = "<b>Fonte</b>: Elaboração do autor."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot"
  )

grid.arrange(plot_growth, plot_corruption, ncol = 2)

data$Growth_log <- log(data$Growth)

ggplot(data, aes(x = Growth_log)) +
  geom_histogram(bins = 20, fill = "#55b748", color = "black", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "#000000", linewidth = 1) +
  scale_y_continuous(labels = NULL) +
  labs(
    title = "Distribuição Logarítmica do Crescimento Econômico",
    subtitle = "Histograma após transformação logarítmica",
    x = NULL, y = NULL,
    caption = "<b>Fonte</b>: Elaboração do autor."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot"
  )

# reg de dados ################################################################

reg <- lm(Growth_log ~ Corruption + Education + `Life expectancy`, data = data)

data$predicted_values <- predict(reg, newdata = data)

ggplot(data, aes(x = predicted_values, y = Growth_log)) +
  geom_point(color = "#55b748", size = 2.5, shape = 21, fill = "white", stroke = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Verificação das Previsões da Regressão",
    subtitle = "Comparação entre os valores previstos e os valores reais",
    x = "Valores Previstos", 
    y = "Valores Observados",
    caption = "<b>Fonte</b>: Elaboração do autor."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot"
  )

summary(reg)

summary_reg <- summary(reg)

coef_table <- as.data.frame(summary_reg$coefficients)
coef_table <- coef_table[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]
coef_table$Variable <- rownames(coef_table)

coef_table_gt <- coef_table %>%
  gt() %>%
  tab_header(
    title = "Resumo do Modelo de Regressão Linear"
  ) %>%
  cols_label(
    Variable = "Variável",
    Estimate = "Estimativa",
    `Std. Error` = "Erro Padrão",
    `t value` = "Valor t",
    `Pr(>|t|)` = "Valor p"
  ) %>%
  tab_spanner(
    label = "Estimativas e Testes",
    columns = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  ) %>%
  tab_footnote(
    footnote = "Fonte: Elaboração do autor."
  )

coef_table_gt

