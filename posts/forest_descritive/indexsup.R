#install.packages("ggtext")

# LIB ##########################################################################
library(sidrar)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(geobr)
library(ggtext)

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

## PEVS POR PRODUTO

pevs_filtrado <- pevs_valor |>
  select(-`Grande Região`) |>
  filter(Ano %in% c(2022, 2023)) |>
  pivot_longer(cols = c(`Carvão vegetal`, `Lenha`, `Madeira em tora`, `Outros produtos`), 
               names_to = "Produto", values_to = "Valor") |>
  group_by(Produto, Ano) |>
  summarise(Total = sum(Valor), .groups = "drop")

ggplot(pevs_filtrado, aes(x = Produto, y = Total, fill = as.factor(Ano))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("R$%.1f Bi", Total / 1e6)), position = position_dodge(width = 0.8), vjust = -0.7, size = 3) +
  geom_hline(yintercept = 0, color = "#000000", linewidth = 1) +
  scale_y_continuous(labels = NULL) +
  scale_fill_manual(values = c("2022" = "#1a2e19", "2023" = "#55b748")) +
  labs(title = "Valor Total da Produção Floresta", subtitle = "Comparação entre 2022 e 2023, R$ Bilhões", x = NULL, y = NULL, caption = "<b>Fonte</b>: Produção da Extração Vegetal e da Silvicultura (PEVS)") +
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
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot"
  )


## PEVS NO TEMPO

pevs_filtrado <- pevs_valor |>
  select(-`Grande Região`) |>
  mutate(Ano = as.numeric(Ano)) |>
  group_by(Ano) |>
  summarise(
    `Carvão vegetal` = sum(`Carvão vegetal`, na.rm = TRUE),
    `Lenha` = sum(`Lenha`, na.rm = TRUE),
    `Madeira em tora` = sum(`Madeira em tora`, na.rm = TRUE),
    `Outros produtos` = sum(`Outros produtos`, na.rm = TRUE)
  ) |>
  pivot_longer(cols = c(`Carvão vegetal`, `Lenha`, `Madeira em tora`, `Outros produtos`),
               names_to = "Produto", values_to = "Valor Total")

ggplot(pevs_filtrado, aes(x = Ano, y = `Valor Total`, color = Produto, group = Produto)) +
  geom_line(linewidth = 1.1) +
  geom_point(shape = 21, fill = "white", size = 2.5) +
  geom_hline(yintercept = 0, color = "#000000", linewidth = 1) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, prefix = "R$", suffix = " Bi")) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020, 2023)) +
  scale_color_manual(values = c("Carvão vegetal" = "#fdbf11", "Lenha" = "#1696d2", "Madeira em tora" = "#55b748", "Outros produtos" = "#000000")) +
  labs(
    title = "Produção Florestal nos Últimos Anos",
    subtitle = "Valor de produção entre 2000 e 2024, R$ Bilhões",
    x = NULL,
    y = NULL,
    caption = "<b>Fonte</b>: Produção da Extração Vegetal e da Silvicultura (PEVS)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.7, "lines"),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

## PEVS NO BRASIL

pevs_filtrado <- pevs_valor |>
  filter(Ano == 2023) |>
  mutate(`Grande Região` = case_when(
    `Grande Região` == "Centro-Oeste" ~ "Centro Oeste",
    TRUE ~ `Grande Região`
  )) |>
  group_by(`Grande Região`) |>
  summarise(`Valor Total` = sum(`Carvão vegetal`, `Lenha`, `Madeira em tora`, `Outros produtos`, na.rm = TRUE), .groups = "drop") |>
  mutate(`Valor Total` = `Valor Total` / 1e6)

regioes <- geobr::read_region(year = 2019)

map_data <- regioes |>
  left_join(pevs_filtrado, by = c("name_region" = "Grande Região"))

ggplot(map_data) +
  geom_sf(aes(fill = `Valor Total`), color = "white") +
  scale_fill_gradient(low = "#e5f5e0", high = "#006d2c", 
                      labels = label_number(prefix = "R$", suffix = " Bi"),
                      name = "Valor Total") +
  labs(
    title = "Produção Florestal Brasileira",
    subtitle = "Grandes Regiões em 2023, R$ Bilhões",
    caption = "<b>Fonte</b>: Produção da Extração Vegetal e da Silvicultura (PEVS)",
    x = NULL, y = NULL
  ) +
  geom_sf_label(aes(label = name_region), 
                size = 3, color = "black", 
                fontface = "bold", 
                label.padding = unit(0.3, "lines"), 
                fill = "white") +
  geom_sf_text(aes(label = sprintf("R$%.1f Bi", `Valor Total`)),
               size = 3, color = "black", 
               fontface = "italic", 
               vjust = 2.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    legend.title = element_blank(),
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot"
  )

