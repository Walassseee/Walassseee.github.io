install.packages("tibble")

# LIB ##########################################################################
library(sidrar)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(tibble)
library(gt)

# ETL ##########################################################################

processar_pevs <- function(variavel) {
  
  pevs <- get_sidra(x = 291,
                    geo = "Region",
                    variable = variavel,
                    period = c("last" = 24),
                    classific = "C194",
                    category = list(c(3455, 3456, 3457, 3460))) |>
    select(`Grande Região`, Ano, Valor, `Tipo de produto da silvicultura`) |>
    
    mutate(`Tipo de produto da silvicultura` = case_when(
      `Tipo de produto da silvicultura` == "1.1 - Carvão vegetal" ~ "Carvão vegetal",
      `Tipo de produto da silvicultura` == "1.2 - Lenha" ~ "Lenha",
      `Tipo de produto da silvicultura` == "1.3 - Madeira em tora" ~ "Madeira em tora",
      `Tipo de produto da silvicultura` == "2 - Outros produtos" ~ "Outros produtos",
      TRUE ~ `Tipo de produto da silvicultura`
    )) |>
    
    pivot_wider(
      names_from = `Tipo de produto da silvicultura`,
      values_from = Valor,
      values_fill = 0
    ) |>
    
    mutate(across(c(`Carvão vegetal`, `Lenha`, `Madeira em tora`, `Outros produtos`), 
                  ~ replace_na(as.numeric(.x), 0))) |>
    
    mutate(across(c(`Carvão vegetal`, `Lenha`, `Madeira em tora`, `Outros produtos`), as.numeric)) |>
    
    select(`Grande Região`, Ano, `Carvão vegetal`, `Lenha`, `Madeira em tora`, `Outros produtos`)
  
  return(pevs)
}

pevs_quant <- processar_pevs(142)
pevs_valor <- processar_pevs(143)

# EDA ##########################################################################
