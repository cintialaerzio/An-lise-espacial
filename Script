 ### Pacotes ----
library(tidyverse)    # inclui ggplot2, dplyr, readr etc.
library(sp)
library(sf)
library(ggspatial)
library(RColorBrewer)
library(viridis)
library(rio)
library(openxlsx) 

### Diretório de trabalho ----
getwd()  # confira seu diretório atual, mantenha aqui só se precisar

### Importação e processamento dos dados ----

# Importa dados de casos por município
VIORO <- import("VIORO_limpa.xlsx")
table(VIORO$nome_municipio)

contagem_municipio <- VIORO %>%
  group_by(nome_municipio) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  mutate(proporcao = total / sum(total)) %>%
  rename(name_mn = nome_municipio)

c("São Felipe", "Itapuã") %in% unique(VIORO$nome_municipio)

# Importa população dos municípios (IBGE)
POPRO <- import("ro_censo_2024.xlsx")

# Junta casos e população para cálculo da incidência
incidencia_ro <- left_join(contagem_municipio, POPRO, by = "name_mn") %>%
  mutate(
    taxa_incidencia = round((total / Pop2024) * 100000, 2),
    alerta = if_else(taxa_incidencia >= 300, "sim", "não")
  )

# Salva resultado (opcional)
write.xlsx(incidencia_ro, "incidencia_ro.xlsx")

### Importação e tratamento do shapefile ----

shp_br <- st_read("shapefile_ibge.shp")

# Filtra Rondônia
shp_ro <- shp_br %>%
  filter(nam_stt == "Rondônia") %>%
  mutate(
    name_mn = str_squish(name_mn),
    name_mn = str_to_upper(name_mn)
  )

# Ajusta nome nos dados de incidência para combinar
incidencia_ro <- incidencia_ro %>%
  mutate(
    name_mn = str_squish(name_mn),
    name_mn = str_to_upper(name_mn)
  )

# Faz o join para juntar shapefile com incidência
shp_ro <- shp_ro %>%
  left_join(incidencia_ro %>% select(name_mn, Pop2024, taxa_incidencia), by = "name_mn")

# Salva shapefile com incidência (opcional)
output_path <- "D:/Analise_SUS_tj/shp_ro_incidence.shp"
st_write(shp_ro, output_path, delete_layer = TRUE)

### Plotagem do mapa ----

library(ggplot2)
library(RColorBrewer)
library(sf)
library(ggspatial)
library(grid)  # para unit()


ggplot() +
  geom_sf(data = shp_ro, aes(fill = taxa_incidencia),
          color = "black", size = 0.1) +
  scale_fill_gradientn(
    colours = brewer.pal(name = "Spectral", n = 11),
    na.value = "grey",
    name = "Taxa\n(100k hab.)"
  ) +
  theme_bw() +
  labs(
    title = "Taxa de Incidência por Município — Rondônia",
    subtitle = "DATASUS; Censo 2024 — IBGE",
    caption = " "
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    height = unit(4, "cm"),
    width = unit(4, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  annotation_scale(
    location = "bl",
    text_cex = 1.5,
    line_width = 1.0,
    height = unit(1, "cm")
  ) +
  theme(
    legend.position = 'bottom',
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.key.size = unit(1.5, 'cm'),
    axis.text.x = element_text(size = 14),  # tamanho menor para longitude (W)
    axis.text.y = element_text(size = 14),  # tamanho menor para latitude (S)
    plot.title = element_text(size = 40, hjust = 0.5),
    plot.subtitle = element_text(size = 35, hjust = 0.5)
  )


# Salva imagem do mapa
ggsave(filename = 'VD_casos_mapa.jpeg', width = 15, height = 15, units = 'in', dpi = 600)
