library(dplyr)
library(readxl)
library(corrplot)
library(car)

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
                       Life.expectancy = `Life.expectancy.at.birth..total..years...SP.DYN.LE00.IN.`)

data <- data |> filter(Education != "..")

data$Education <- as.numeric(data$Education)
data$Life.expectancy <- as.numeric(data$Life.expectancy)

par(mfrow=c(2,3))

hist(data$Growth, main = "Distribuição do Produto per capita",
     xlab = "Produto per capita", ylab = "")

hist(data$Tax, main = "Distribuição do Índice de Tributação",
     xlab = "Índice de Tributação", ylab = "")

hist(data$Corruption, main = "Distribuição do Índice de Corrupção",
     xlab = "Índice de Corrupção", ylab = "")

hist(data$Education, main = "Distribuição do Grau de Educação",
     xlab = "Grau de Educação", ylab = "")

hist(data$Life.expectancy, main = "Distribuição da Saúde",
     xlab = "Expectativa de Vida", ylab = "")

data$lnGrowth <- log(data$Growth)

par(mfrow=c(1,1))
hist(data$lnGrowth, main = "Distribuição do Ln(Produto per capita)",
     xlab = "ln(Produto per capita)", ylab = "")

par(mfrow=c(2,2))
plot(data$lnGrowth ~ data$Corruption, main = "Correlação Ln(Produto per capita) e Corrupção",
     xlab = "Índice de Corrupção", ylab = "ln(Produto per capita)")
plot(data$lnGrowth ~ data$Tax, main = "Correlação Ln(Produto per capita) e Tributação",
     xlab = "Índice de Tributação", ylab = "ln(Produto per capita)")
plot(data$lnGrowth ~ data$Education, main = "Correlação Ln(Produto per capita) e Grau de Educação",
     xlab = "Grau de Educação", ylab = "ln(Produto per capita)")
plot(data$lnGrowth ~ data$Life.expectancy, main = "Correlação Ln(Produto per capita) e Saúde",
     xlab = "Expectativa de Vida", ylab = "ln(Produto per capita)")

corMatrix <- cor(data[c("lnGrowth", "Corruption", "Tax",
                        "Education", "Life.expectancy")])

par(mfrow=c(1,1))
corrplot(corMatrix, method = "color", type = "lower", addCoef.col = "white", 
         tl.col = "black", tl.srt = 45, number.cex = 0.8)

reg <- lm(lnGrowth ~ Corruption + Tax + Education + Life.expectancy, data = data)
summary(reg)

vif_values <- vif(reg)

barplot(vif_values, main = "Fator de Inflação da Variância", horiz = TRUE, 
        xlab = "Fator de Inflação da Variância", las = 1, cex.names = 0.8)
