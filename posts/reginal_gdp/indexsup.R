library(dplyr)
library(geobr)
library(ggplot2)
library(sf)
library(spdep)

pib <- read.csv2("posts/reginal_gdp/pib.csv", sep = ";", dec = ",")[c("Sigla","Código","Município","pib")] |> 
  filter(Sigla == "PR")
pop <- read.csv2("posts/reginal_gdp/pop.csv", sep = ";", dec = ",")[c("Sigla","Código","Município","pop")] |> 
  filter(Sigla == "PR")
idh <- read.csv2("posts/reginal_gdp/idh.csv", sep = ";", dec = ",")[c("Sigla","Código","Município","idh")] |> 
  filter(Sigla == "PR")


dta <- inner_join(pib, pop[c("Código","pop")], by = "Código")
dta <- inner_join(dta, idh[c("Código","idh")], by = "Código")

mpa <- read_municipality()[c("abbrev_state","code_muni")] |> 
  filter(abbrev_state == "PR")

dta <- inner_join(dta, mpa[c("code_muni")], by = c("Código" = "code_muni"))

dta <- dta |> mutate(pibpc = pib / pop)

colors <- c("#CA5800", "#FDBF11", "#FDD870", "#FFF2CF", "#CFE8F3", "#73BFE2", "#1696D2", "#0a4c6a")

ggplot(data = dta) +
  geom_sf(aes(fill = pibpc, geometry = geom), color = NA) +
  scale_fill_gradientn(colors = colors, name = "PIB per capita") +
  labs(
    title = "PIB per capita por Microrregião do Paraná",
    subtitle = "Dados do PIB e População",
    caption = "Fonte: Dados internos e geobr"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )