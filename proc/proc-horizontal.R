# Carga de librerías ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,sjlabelled,countrycode,haven,questionr,wbstats,zoo,
               sjPlot,sjmisc,dplyr,summarytools,flextable,data.table,
               PerformanceAnalytics,plotly,countrycode,rnaturalearth,sf,leaflet,
               shiny, kableExtra,DiagrammeR,survey,srvyr)
library(DiagrammeR)


# Funciones ----

# 1. Función de estandarización de las escalas.
## Descripción:
## Esta función reescala las variables numéricas a un rango 0-10.
## Excluye automáticamente variables que no sean de caracter numérico.
# Argumentos:
## df: Espera un dataframe.
## exclude_cols: Se espera un vector con el nombre (character) de las variables que no se procesarán.

standarize_data <- function(df, exclude_cols = NULL) {
  cols <- setdiff(names(df)[sapply(df, is.numeric)], exclude_cols)
  for(col in cols) {
    old_min <- min(df[[col]], na.rm = TRUE)
    old_max <- max(df[[col]], na.rm = TRUE)
    new_min <- 0
    new_max <- 10
    df[[col]] <- ifelse(!is.na(df[[col]]), ((df[[col]] - old_min) / (old_max - old_min)) * 
                          (new_max - new_min) + new_min, NA)
  }
  return(df)
}



# 3. Funcióm media sin NAs
## Descripción:
## Simple función que omite el agumento de rm.na=TRUE.
# Argumentos:
## ... = Valores a promediar.
mean_na <- function(...) {
  mean_result <- mean(c(...), na.rm = TRUE)
  if (is.nan(mean_result)) {
    mean_result <- NA
  }
  return(mean_result)
}

# Datos LAPOP ----

# Se almacenó el resultado en "input/data/Lapop_data_proms.RDS"

# Se crean dos vectores con el nombre de variables LAPOP. Se distingue si apuntan
# a indicadores horizontales o verticales. Posteriormente se utilizan para crear
# el vector general usado para el subset.

vars_hor <- c("b4", "b6", "aoj11",
              "sd2new2",
              "sd3new2",
              "sd6new2",
              "vic1ext",
              "sd5new2",
              "cp6",
              "cp7",
              "cp8",
              "it1")

# vars_ver <- c("b21a","b21","b18","b23","b10a","b39n","b13",
#               "pn4","b4","nicfear","prot3",
#               "pol1","vb2",
#               "movus1", "exc7", "ros4")

vars <- c("wave","year","pais","wt",vars_hor)

# A continuación se realiza la carga de archivos LAPOP.
load(file = "input/data/GrandMerge.RData") 
load(file="input/data/LAPOP_2004-2008.RData") # datos0418
dta <- list.files(path = "input/data/lapop-faltantes/", pattern = ".dta")
dta <- paste0(file = "input/data/lapop-faltantes/",dta)
merge_faltante <- lapply(dta, function(archivo){
  print(archivo)
  df <- read_dta(archivo)
  df <- df %>% select_if(names(df) %in% vars) %>% mutate(
    wave = substr(archivo,nchar(archivo)-7,nchar(archivo)-4)
  ) 
  return(df)
})
rm(dta)
merge_faltante <- bind_rows(merge_faltante)

dta<- list.files(path = "input/data/LAPOP2021/", pattern = ".dta")
dta <- paste0(file = "input/data/LAPOP2021/",dta)

dta_2023<- list.files(path = "input/data/LAPOP2023/", pattern = ".dta")
dta_2023 <- paste0(file = "input/data/LAPOP2023/", dta_2023)

data2021 <- lapply(dta, function(archivo){
  print(archivo)
  df <- read_dta(archivo)
  df <- df %>% select_if(names(df) %in% vars)
  return(df)
})
data2021 <- bind_rows(data2021) ; rm(dta)

data2023 <- lapply(dta_2023, function(archivo){
  print(archivo)
  df <- read_dta(archivo)
  df <- df %>% select_if(names(df) %in% vars)
  return(df)
})
data2023 <- bind_rows(data2023) ; rm(dta_2023)

datos0418 <- datos0418 %>% select_if(names(datos0418) %in% vars)
datos1618 <-  datos0418  %>% filter(wave==2016 | wave==2018)
datos1618$wave <- as.numeric(datos1618$wave)
datos$wave <- NA
datosselc <- datos %>% select_if(names(datos) %in% vars)

# Los casos se recodifican de tal modo wave (olas): 
# 2004(2004), 2006(2006 y 2007),  2008(2008 y 2009), 2010(2010), 2012(2012), 2014(2014), 2016(16,17), 2018(18,19)
datosselc$wave <- ifelse(datosselc$year==2006 | datosselc$year==2007, 2006, NA)
datosselc$wave <- ifelse(datosselc$year==2004 , 2004, datosselc$wave)
datosselc$wave <- ifelse(datosselc$year==2008 | datosselc$year==2009, 2008, datosselc$wave)
datosselc$wave <- ifelse(datosselc$year==2010, 2010, datosselc$wave)
datosselc$wave <- ifelse(datosselc$year==2012, 2012, datosselc$wave)
datosselc$wave <- ifelse(datosselc$year==2014, 2014, datosselc$wave)

datosselc <- remove_all_labels(datosselc) # Las etiquetas no son compatibles
merge_faltante <- mutate_all(merge_faltante, as.numeric)
merge_faltante$year <- merge_faltante$wave

data2021 <- mutate_all(data2021, as.numeric)
data2021$year <- data2021$wave

data2023 <- data2023 %>% 
  mutate_all(as.numeric) %>%
  mutate(wave= 2022)
data2023$year <- data2023$wave

# Merge 2004-2014 + 2018 + faltantes varias del grand merge + 2021
datos <- bind_rows(datosselc,datos1618,merge_faltante,data2021, data2023) # logra una base longitudinal con la pr4 incluida. 
datos <- copy_labels(datos, datos0418)
# rm(list = setdiff(ls(),c("datos","vars","vars_hor","vars_ver","mean_result")))
datos_label <- to_label(datos)
datos$pais<-datos_label$pais

# # Se incorporan etiquetas faltantes
# datos$movus1 <- set_label(datos$movus1, "Movilidad económica")
# datos$jamb18a <- set_label(datos$jamb18a, "Confianza en la policía del barrio")
# datos$b39n <- set_label(datos$b39n, "¿Hasta qué punto tiene usted confianza en el sector privado?")

# Se invierte sentido de respuestas para ajustar escalas (valores mayores ~ mayor cohesión).
datos <- datos %>%
  mutate(cp8 = 5 - cp8,
         cp6 = 5 - cp6,
         cp7 = 5 - cp7,
         it1 = 5 - it1,
#          prot3 = 3 -prot3,
         aoj11 = 5 - aoj11,
         sd2new2 = 5 - sd2new2,
         sd3new2 = 5 - sd3new2,
         sd6new2 = 5 - sd6new2,
         sd5new2 = 5 - sd5new2,
#          pn4 = 5 -pn4,
#          vb2 = 3-vb2,
#          pol1 = 5-pol1,
#          b23 = set_na(b23,na=8),
#          nicfear = 3 - nicfear
  )

# Especifica las columnas para excluir
exclude_cols = c("wave", "pais","wt")

# Estandariza los datos
prom_wave <- standarize_data(datos, exclude_cols) %>% select(-year)


###########

vars_mod <- names(prom_wave)[c(3:15)]

prom_wave <- svydesign(ids = ~1,
                       data = prom_wave,
                       weights = ~wt) %>%
  as_survey() %>%
  group_by(pais, wave) %>%
  summarize_all(survey_mean, na.rm = TRUE) %>%
  select(c("pais","wave"),vars_mod)

# Supongamos que tu dataframe se llama test
# Excluimos la columna "pais" del proceso
columnas_a_modificar <- setdiff(names(prom_wave), "pais")

# Aplicamos la función replace solo a las columnas seleccionadas
prom_wave[columnas_a_modificar] <- sapply(prom_wave[columnas_a_modificar], function(x) replace(x, x == 0, NA))

# test <- prom_wave %>%
#   group_by(pais,wave) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) %>%
#   ungroup() %>%
#   select(-wt)

# datos_sin_agrupar <- ungroup(prom_wave)
# Convertir a data.frame
# prom_wave <- as.data.frame(datos_sin_agrupar)
prom_wave <- copy_labels(prom_wave, datos_label)

# prom_wave$movus1 <- set_label(prom_wave$movus1, "Movilidad económica")
# prom_wave$jamb18a <- set_label(prom_wave$jamb18a, "Confianza en la policía del barrio")
# prom_wave$b39n <- set_label(prom_wave$b39n, "¿Hasta qué punto tiene usted confianza en el sector privado?")
prom_wave <- prom_wave %>% ungroup()

# Se almacena la data.
write_rds(prom_wave,"input/data/lapop_proc.RDS")


# WVS ----

rm(list=setdiff(ls(),c("prop_casos_perdidos","standarize_data",
                       "impute_data","mean_na")))

# Los datos están contenidos en un ar
load("input/data/WVS/WVS_TimeSeries_4_0.rdata")
WVS <- data1 ; rm(data1)
columnas <- names(label_to_colnames(WVS))
data <- WVS %>% select(COW_NUM, S020,S017,E012, G006,
                       F114A,
                       F115,
                       F116,
                       F117,
                       G007_18_B,
                       A165)

# data <- WVS[c(6,23,
#               # 78,80,94:111,115,116,194,
#               407,
#               # 533,
#               804,914)]
respuestas <- to_label(data)
data$COW_NUM<-respuestas$COW_NUM


data <- data %>%
  setNames(c("pais","wave","wt", "voluntad_luchar_pais", "orgullonacional",
             "recibir_beneficios", "evadir_transporte",
             "evadir_impuestos", "aceptar_soborno", "confiar_vecinos",
             "confianza_generalizada"))

data$orgullonacional[data$orgullonacional<1] <- NA
data$orgullonacional <- 5-data$orgullonacional
data$voluntad_luchar_pais[data$voluntad_luchar_pais<0] <- NA
# data$voluntad_luchar_pais <- 2-data$voluntad_luchar_pais

# data$igualdadingresos[data$igualdadingresos<1] <- NA
# data$igualdadingresos <- 11-data$igualdadingresos
# data$seguridadbarrio[data$seguridadbarrio<1] <- NA
# data$seguridadbarrio <- 5-data$seguridadbarrio
# data$importanciademocracia[data$importanciademocracia<1] <- NA
# 
# data$firmapeticion_wvs[data$firmapeticion_wvs<1] <- NA
# data$firmapeticion_wvs <- 4-data$firmapeticion_wvs
# 
# 
# data$boycotts[data$boycotts<1] <- NA
# data$boycotts <- 4-data$boycotts
# 
# data$protestaspacificas[data$protestaspacificas<1] <- NA
# data$protestaspacificas <- 4-data$protestaspacificas
# 
# data$huelgas[data$huelgas<1] <- NA
# data$huelgas <- 4-data$huelgas
# 
# data$trabajoduro[data$trabajoduro<1] <- NA
# data$trabajoduro <- 11-data$trabajoduro


data$recibir_beneficios[data$recibir_beneficios<1] <- NA
data$recibir_beneficios <- 11-data$recibir_beneficios

data$evadir_transporte[data$evadir_transporte<1] <- NA
data$evadir_transporte <- 11-data$evadir_transporte

data$evadir_impuestos[data$evadir_impuestos<1] <- NA
data$evadir_impuestos <- 11-data$evadir_impuestos

data$aceptar_soborno[data$aceptar_soborno<1] <- NA
data$aceptar_soborno <- 11-data$aceptar_soborno

data$confiar_vecinos[data$confiar_vecinos<1] <- NA
data$confiar_vecinos <- 5-data$confiar_vecinos

 
data$confianza_generalizada[data$confianza_generalizada<1] <- NA
data$confianza_generalizada <- 3-data$confianza_generalizada

# Filter país
data <- data %>%
  filter(pais %in% c("Argentina", "Bolivia", "Brazil", "Canada",
                     "Chile", "Colombia", "Dominican Republic",
                     "Ecuador","El Salvador", "Guatemala", "Haiti",
                     "Mexico", "Nicaragua","Trinidad and Tobago",
                     "Peru","Puerto Rico", "United States of America",
                     "Uruguay","Venezuela")) %>%
  filter(wave > 2003)

# Especifica las columnas para excluir
exclude_cols = c("wave", "pais","wt")
# Estandariza los datos
data <- standarize_data(data, exclude_cols)

# data$wave[data$wave==2022] <- 2020

vars_mod <- names(data)[c(4:11)]

prom_wave <- svydesign(ids = ~1,
                       data = data,
                       weights = ~wt) %>%
  as_survey() %>%
  group_by(pais, wave) %>%
  summarize_all(survey_mean, na.rm = TRUE) %>%
  select(c("pais","wave"),vars_mod)

# Supongamos que tu dataframe se llama test
# Excluimos la columna "pais" del proceso
columnas_a_modificar <- setdiff(names(prom_wave), "pais")

# Aplicamos la función replace solo a las columnas seleccionadas
prom_wave[columnas_a_modificar] <- sapply(prom_wave[columnas_a_modificar], function(x) replace(x, x == 0, NA))


prom_wave$wave <- ifelse(prom_wave$wave %% 2 == 1,prom_wave$wave-1,prom_wave$wave)

prom_wave <- prom_wave %>%
  # select(-year) %>%
  group_by(wave,pais) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

# Obtén la lista de países
paises <- unique(prom_wave$pais)

# Crea un dataframe con wave == 2008 para cada país
nuevas_filas <- data.frame(pais = paises,
                           wave = 2008)

# Añade columnas adicionales con NA
otros_campos <- setdiff(names(prom_wave), c("pais", "wave"))
for (campo in otros_campos) {
  nuevas_filas[[campo]] <- NA
}

# Une con el dataframe original
prom_wave <- bind_rows(prom_wave, nuevas_filas)

# Ordena si lo deseas
prom_wave <- prom_wave %>% arrange(pais, wave)

write_rds(prom_wave,"input/data/wvs_proc.RDS")

# Merge ----

# Se limpia el espacio de trabajo
rm(list=setdiff(ls(),"mean_na"))
# Comienza la carga de datos
data_lapop <- readRDS("input/data/lapop_proc.rds")
# data_latinobarometro <- readRDS("data/data_latinobarometro_2023.rds")
data_wvs <- readRDS("input/data/wvs_proc.rds")

# Ediciones de formato
# data_latinobarometro$pais <- as.character(data_latinobarometro$pais)
data_lapop$pais <- as.character(data_lapop$pais)
data_wvs$pais <- as.character(data_wvs$pais)

# Recodificación para estandarizar
# data_latinobarometro$pais[data_latinobarometro$pais =='Dominican Rep.'] <- "Dominican Republic" 
data_wvs$pais[data_wvs$pais =='Trinidad and Tobago'] <- "Trinidad & Tobago" 
data_wvs$pais[data_wvs$pais =='United States of America'] <- "United States"

# Merge
datos <- merge(data_lapop,data_wvs,by=c("pais","wave"),all= TRUE)

# Imputación ----

# 2. Función de imputación de datos por olas.
## Descripción:
## Esta función aisla los datos de cada país por ola, identifica los casos
# perdidos e imputa sus valores hasta 3 olas atrás y en caso de no encontrarse
# hasta tres olas en el futuro.
# Argumentos:
## df: Espera un dataframe.
## var: Indica la variable a imputar.
impute_data <- function(df, var) {
  # Contar NAs antes de la imputación
  na_count_before <- sum(is.na(df[[var]]))
  
  df %>% 
    group_by(pais) %>% 
    mutate(
      temp = ifelse(is.na(!!sym(var)), lag(!!sym(var), 1), !!sym(var)), 
      temp = ifelse(is.na(temp), lag(temp, 1), temp),
      temp = ifelse(is.na(temp), lag(temp, 1), temp),
      temp = ifelse(is.na(temp), lead(temp, 1), temp),
      temp = ifelse(is.na(temp), lead(temp, 1), temp),
      temp = ifelse(is.na(temp), lead(temp, 1), temp),
      !!var := ifelse(is.na(!!sym(var)), temp, !!sym(var))) %>% 
    select(-temp) -> df
  
  # Contar NAs después de la imputación
  na_count_after <- sum(is.na(df[[var]]))
  
  # Calcular el número de imputaciones
  imputations <- na_count_before - na_count_after
  
  return(list(df = df, imputations = imputations))
}

numeric_vars <- names(datos)[sapply(datos, is.numeric)]
numeric_vars <- setdiff(numeric_vars, "wave")


# Calcula el número total de filas en el dataframe
total_rows <- nrow(datos)

# Inicializa una lista para almacenar los resultados
imputation_results <- list()

# Ejecuta la imputación y almacena los resultados
for(var in numeric_vars) {
  result <- impute_data(datos, var)
  datos <- result$df
  n_imputations <- result$imputations
  percentage <- round((n_imputations / total_rows) * 100,2)
  imputation_results[[var]] <- c(n = n_imputations, percentage = percentage)
}

# Convierte la lista de resultados en un dataframe
imputation_summary <- as.data.frame(do.call(rbind, imputation_results))
rownames(imputation_summary) <- numeric_vars
# imputation_summary
rm(numeric_vars,var)

save(datos, file="input/data/datos_horizontal.RData")

