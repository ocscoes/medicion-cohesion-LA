library(pacman)
p_load(tidyverse)

load(file="input/data/cohesion_merge.RData")

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

# Calidad subdimensiones

calidad_year <- datos %>%
  as.data.frame %>%
  select(-c(pais, wave)) %>%
  mutate(across(everything(), ~ !is.na(.))) %>%
  as.data.frame() %>%
  mutate(
    sub_confianza   = ifelse(rowSums(cbind(it1)) > 0, FALSE, TRUE),
    sub_seguridad  = ifelse(rowSums(cbind(aoj11, vic1ext)) > 1, FALSE, TRUE),
    sub_confianza_inst  = ifelse(rowSums(cbind(confianza_congreso,
                                                        confianza_part_politicos_lapop,
                                                        confianza_poder_judicial)) > 1, FALSE, TRUE),
    sub_democracia = ifelse(rowSums(cbind(apoyo_democracia, satisfaccion_democracia)) > 1, FALSE, TRUE),
    justicia_distribucion = ifelse(rowSums(cbind(justicia_distribucion)) > 0, FALSE, TRUE),
  ) %>%
  select(c("sub_confianza",
           "sub_seguridad",
           "sub_confianza_inst",
           "sub_democracia",
           "justicia_distribucion")) %>%
  mutate(
    pais = datos$pais,
    wave = datos$wave
  ) %>%
  select(pais,wave,everything())


for(i in 1:nrow(datos)){
  # print(i)
  for(x in names(calidad_year)[3:7]){
    if(calidad_year[i,x]){
      datos[i,x] <- NA
    }
  }
}

# Calidad dimensiones

calidad_year <- datos %>%
  as.data.frame %>%
  select(-c(pais, wave)) %>%
  mutate(across(everything(), ~ !is.na(.))) %>%
  as.data.frame() %>%
  mutate(
    cohesion_horizontal   = ifelse(rowSums(cbind(sub_confianza, sub_seguridad)) > 1, FALSE, TRUE),
    cohesion_vertical = ifelse(rowSums(cbind(sub_confianza_inst, sub_democracia, justicia_distribucion)) > 1, FALSE, TRUE)) %>%
  select(c(cohesion_horizontal,
           cohesion_vertical)) %>%
  mutate(
    pais = datos$pais,
    wave = datos$wave
  ) %>%
  select(pais,wave,everything())


for(i in 1:nrow(datos)){
  # print(i)
  for(x in names(calidad_year)[3:4]){
    if(calidad_year[i,x]){
      datos[i,x] <- NA
    }
  }
}

# Calidad general

calidad_year <- datos %>%
  as.data.frame %>%
  select(-c(pais, wave)) %>%
  mutate(across(everything(), ~ !is.na(.))) %>%
  as.data.frame() %>%
  mutate(
    cohesion_general   = ifelse(rowSums(cbind(cohesion_vertical,
                                              cohesion_horizontal)) > 1, FALSE, TRUE)) %>%
  select(c(cohesion_general)) %>%
  mutate(
    pais = datos$pais,
    wave = datos$wave
  ) %>%
  select(pais,wave,everything())

columnas_objetivo <- c("cohesion_general")  # Agrega más si lo deseas
for(i in 1:nrow(datos)) {
  if (isTRUE(calidad_year$cohesion_general[i])) {
    datos[i, columnas_objetivo] <- NA
  }
}

datos <- datos %>% filter(wave!=2020 & wave!=2021)

# Exportar

load(file = "input/data/country_vars.rdata")

country_vars <- country_vars %>%
  rename("wave" = "date")

df <- merge(datos,
            country_vars,
            by = c("pais","wave"),
            all.x = TRUE) %>%
  select(-id,-country)



# prom_wave_wvs <- prom_wave_wvs %>%
#   mutate(pais = droplevels(pais)) %>%
#   as_tibble()%>%
#   complete(wave,pais)
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
         "Coef. Gini" = gini,
         "PIB per capita (miles)" = gdp
  ) %>% 
  pivot_longer(cols = 3:12,names_to = "Variable",values_to = "Valor", values_drop_na = FALSE)

df <- df %>% rbind(df %>%
                     group_by(Ola, Variable) %>%
                     summarise(Valor = mean(Valor, na.rm = TRUE)) %>%
                     mutate(País = "Promedio") %>%
                     ungroup() %>%
                     select(Ola, País, Variable, Valor)) %>%
  filter(Ola!=2021)

save(df,file = "input/data/base_shiny.rdata")
# load(file = "data/base_shiny.rdata")
# Ola, País, Variable, Valor (incluye NA) # 2000 filas.


load(file = "input/data/world_data.rdata")

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


