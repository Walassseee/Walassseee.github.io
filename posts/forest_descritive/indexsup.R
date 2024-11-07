install.packages("urbnthemes")

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

pevs_valor <- processar_pevs(143)

# EDA ##########################################################################

#| echo: false

library(sidrar)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

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

pevs_valor <- processar_pevs(143)

pevs_filtrado <- pevs_valor %>%
  select(-`Grande Região`) %>%
  filter(Ano %in% c(2022, 2023)) %>%
  pivot_longer(cols = c(`Carvão vegetal`, `Lenha`, `Madeira em tora`, `Outros produtos`), 
               names_to = "Produto", values_to = "Valor") %>%
  group_by(Produto, Ano) %>%
  summarise(Total = sum(Valor), .groups = "drop")

pevs_chart <- ggplot(pevs_filtrado, aes(x = Produto, y = Total, fill = as.factor(Ano))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("R$%.2f Mi", Total / 1e6)), position = position_dodge(width = 0.8), vjust = -0.7, size = 3.2) +
  geom_hline(yintercept = 0, color = "#000000", size = 1) +
  scale_y_continuous(labels = NULL) +
  scale_fill_manual(values = c("2022" = "#000000", "2023" = "#46ABDB")) +
  labs(title = "Valor Total da Produção Floresta", subtitle = "Comparação entre 2022 e 2023, R$ Milhões", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.size = unit(0.7, "lines"),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "plot"
  )

print(pevs_chart)


