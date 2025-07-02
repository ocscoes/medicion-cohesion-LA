library(pacman)
p_load(tidyverse)

load(file="input/data/cohesion_merge.RData")

otros <- datos %>% select(orgullonacional,
                          igualdad_ingresos_lapop,
                          trabajo_duro,
                          percepcion_corrupcion,
                          interes_politica,
                          sub_prosocial)

datos <- datos %>% select(it1, aoj11, vic1ext,
                           confianza_congreso,
                           confianza_part_politicos_lapop,
                           confianza_poder_judicial,
                           apoyo_democracia,
                           satisfaccion_democracia,
                           justicia_distribucion,
                           sub_confianza,
                           sub_seguridad,
                           sub_confianza_inst,
                           sub_democracia,
                           cohesion_horizontal,
                           cohesion_vertical,
                           cohesion_general)

datos <- datos %>% filter(wave!=2020 & wave!=2021)

# Exportar

load(file="input/data/variables_macro.RData")

country_vars <- var_macro


# df <- merge(datos,
#             otros,
#             by = c("pais","wave"),
#             all.x = TRUE)



# prom_wave_wvs <- prom_wave_wvs %>%
#   mutate(pais = droplevels(pais)) %>%
#   as_tibble()%>%
#   complete(wave,pais)
df <- datos %>%
  select(Ola = wave,
         País = pais,
         "Confianza Interpersonal"= sub_confianza,
         "Seguridad Pública"= sub_seguridad,
         "Confianza en las Instituciones"= sub_confianza_inst,
         "Actitudes a la Democracia"= sub_democracia,
         "Justicia Redistributiva"= justicia_distribucion,
         "Cohesión horizontal" = cohesion_horizontal,
         "Cohesión vertical" = cohesion_vertical,
         "Cohesión general"= cohesion_general,
         # "Orgullo Nacional"= orgullonacional,
         # "Importancia Igualdad de Ingresos"= igualdad_ingresos_lapop,
         # "Importancia Trabajo Duro"= trabajo_duro,
         # "Percepción de Corrupción"= percepcion_corrupcion,
         # "Interés en la Política"= interes_politica,
         # "Comportamiento Prosocial"= sub_prosocial
         ) %>% 
  pivot_longer(cols = 3:10,names_to = "Variable",values_to = "Valor", values_drop_na = FALSE)

save(df,file="input/data/base_shiny_minima.rdata")

# Otros indicadores

df <- merge(datos,
            otros,
            by = c("pais","wave"),
            all.x = TRUE)

df <- df %>%
  select(Ola = wave,
         País = pais,
         "Confianza Interpersonal"= sub_confianza,
         "Seguridad Pública"= sub_seguridad,
         "Confianza en las Instituciones"= sub_confianza_inst,
         "Actitudes a la Democracia"= sub_democracia,
         "Justicia Redistributiva"= justicia_distribucion,
         "Cohesión horizontal" = cohesion_horizontal,
         "Cohesión vertical" = cohesion_vertical,
         "Cohesión general"= cohesion_general,
         "Orgullo Nacional"= orgullonacional,
         "Importancia Igualdad de Ingresos"= igualdad_ingresos_lapop,
         "Importancia Trabajo Duro"= trabajo_duro,
         "Percepción de Corrupción"= percepcion_corrupcion,
         "Interés en la Política"= interes_politica,
         "Comportamiento Prosocial"= sub_prosocial
  ) %>% 
  pivot_longer(cols = 3:16,names_to = "Variable",values_to = "Valor", values_drop_na = FALSE)

save(df,file="input/data/base_shiny_otros.rdata")

 ### Procesar base macro
library(stringi)
library(dplyr)

paises_estandar <- c(
  "Argentina", "Bahamas", "Belize", "Bolivia", "Brazil",
  "Canada", "Chile", "Colombia", "Costa Rica", "Dominican Republic",
  "Ecuador", "El Salvador", "Grenada", "Guatemala", "Guyana",
  "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
  "Panama", "Paraguay", "Peru", "Puerto Rico", "Suriname",
  "Trinidad & Tobago", "United States", "Uruguay", "Venezuela"
)


# Función para normalizar texto
normalizar <- function(x) {
  stri_trans_general(x, "Latin-ASCII") |> tolower()
}

# Ahora hacemos la clasificación
country_vars <- country_vars %>%
  mutate(pais_normalizado = normalizar(nombre_pais),
         País = sapply(pais_normalizado, function(x) {
           match <- NA
           for (p in paises_estandar) {
             if (grepl(normalizar(p), x, ignore.case = TRUE)) {
               match <- p
               break
             }
           }
           match
         })) 

unique(country_vars$País)

country_vars <- country_vars %>%
  mutate(País= case_when(id_pais=="TTO" ~ "Trinidad & Tobago",
                         id_pais=="USA" ~ "United States",
                         id_pais=="DOM" ~ "Dominican Republic",
                         id_pais=="BLZ" ~ "Belize",
                         id_pais=="GRD" ~ "Grenada",
                         id_pais=="BRA" ~ "Brazil",
                         .default= País)) %>%
  select("Ola"= año,
         País,
         "Variable"= indicador,
         "Valor"= valor)

unique(country_vars$País)

df <- rbind(df, country_vars)


##################


df <- df %>%
  rbind(df %>%
          group_by(Ola, Variable) %>%
          summarise(
            Valor = if (all(is.na(Valor))) NA_real_ else mean(Valor, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(País = "Promedio")) %>%
                     ungroup() %>%
                     select(Ola, País, Variable, Valor) %>%
  filter(Ola!=2021)

save(df,file = "input/data/base_shiny.rdata")
# load(file = "data/base_shiny.rdata")
# Ola, País, Variable, Valor (incluye NA) # 2000 filas.


### Datos para el maapa


load(file = "input/data/world_data.rdata")
load(file="input/data/country_vars.RData")

country_vars <- country_vars %>%
  rename("wave" = "date")

df <- merge(datos,
            country_vars,
            by = c("pais","wave"),
            all.x = TRUE) %>%
  select(-id)

df <- df %>%
  select(iso3 = country,
         Ola = wave,
         País = pais,
         "Confianza Interpersonal"= sub_confianza,
         "Seguridad Pública"= sub_seguridad,
         "Confianza en las Instituciones"= sub_confianza_inst,
         "Actitudes a la Democracia"= sub_democracia,
         "Justicia Redistributiva"= justicia_distribucion,
         "Cohesión horizontal" = cohesion_horizontal,
         "Cohesión vertical" = cohesion_vertical,
         "Cohesión general"= cohesion_general
         # "Coef. Gini" = gini,
         # "PIB per capita (miles)" = gdp
  ) %>%
  filter(Ola!=2021 & Ola!=2020)

world <- world_data %>% select(iso3,geometry)
world <- world[!duplicated(world),]

world_data <- merge(world,df, by = c("iso3"), all.y = TRUE)
world_data <- world_data %>% filter(!is.na(iso3))



save(world_data, file = "input/data/world_data.rdata")


