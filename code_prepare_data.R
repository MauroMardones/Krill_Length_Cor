# Cargar librerías --------------------------------------------------------

# Manipulación de datos
library(tidyverse)
library(readr)
library(data.table)
library(patchwork)
library(kableExtra)
library(here)

# Análisis estadístico
library(easystats)
library(see)
library(ggcorrplot)
library(ggpubr)  # Para stat_cor()

# Visualización
library(ggplot2)
library(ggridges)
library(egg)
library(GGally)

# Datos espaciales y raster
library(ncdf4)
library(raster)
library(sf)
library(CCAMLRGIS)
library(tmaptools)

# Optimización
library(parallel)

# Cargar datos espaciales -------------------------------------------------

# Línea de costa
coast <- load_Coastline() %>% st_as_sf() %>%
  st_transform("+proj=latlong +ellps=WGS84")

# Áreas estadísticas con foco en 48.1
suba <- load_ASDs() %>% subset((3)) %>% 
  st_as_sf() %>% 
  st_transform("+proj=latlong +ellps=WGS84")

# Cargar y transformar estratos
strata <- st_read("~/DOCAS/Mapas/Antarctic_SHPfiles/Strata.shp", quiet = TRUE) %>%
  st_transform("+proj=latlong +ellps=WGS84") %>%
  st_make_valid()

# Cargar datos climáticos -------------------------------------------------

load("~/DOCAS/Data/KrillLows/datchl.RData")
load("~/DOCAS/Data/KrillLows/datgeo5.RData")
load("~/DOCAS/Data/KrillLows/dattsm.RData")

# Transformar datos climáticos
datchl1 <- datchl %>%
  mutate(variable = "Chla") %>%
  rename(value = CHLa) %>%
  group_by(ANO, lat, lon, variable) %>%
  summarise(value1 = mean(value))

dattsm1 <- dattsm %>%
  mutate(variable = "tsm") %>%
  rename(value = TSM)

datseai <- datgeo5 %>%
  mutate(variable = "seaice") %>%
  rename(value = SEAICE, lat = latitud, lon = longitud)

# Unir datos ambientales
allenv <- bind_rows(datchl1, dattsm1, datseai) %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=latlong +ellps=WGS84")

# Asociar datos ambientales con estratos
envstr <- st_join(strata, allenv)

# Guardar datos ambientales con estratos
saveRDS(envstr, "env_strata.RData")

# Resumir datos ambientales
datasum <- envstr %>%
  select(ID, ANO, value, variable) %>%
  filter(ID != "Outer") %>%
  group_by(ID, ANO, variable, geometry) %>%
  summarise(meanvalue = mean(value))

# Cargar y procesar datos biológicos --------------------------------------

# Cargar archivos en entornos separados
env_metadata <- new.env()
env_metadata2 <- new.env()

load("~/DOCAS/Data/565_C1_KRI_2021-10-01/DATA_PRODUCT_565.RData", envir = env_metadata)

# Extraer datos biológicos
ohbio2 <- left_join(env_metadata$C1, env_metadata$OBS_HAUL_BIOLOGY, by = "obs_haul_id") %>%
  filter(asd_code == "481") %>%
  select(vessel_nationality_code, asd_code, datetime_haul_start, latitude_haul_start,
         longitude_haul_start, maturity_stage, length_total_cm, krill_greenweight_kg) %>%
  rename(lat = latitude_haul_start, lon = longitude_haul_start) %>%
  mutate(ANO = year(datetime_haul_start), MES = month(datetime_haul_start)) %>%
  drop_na()

# Convertir datos biológicos a sf y unir con estratos
ohbiostr <- strata %>%
  st_join(st_as_sf(ohbio2, coords = c("lon", "lat"), crs = "+proj=latlong +ellps=WGS84"))

# Resumir datos biológicos
ohbiostr2 <- ohbiostr %>%
  select(ID, ANO, maturity_stage, length_total_cm, krill_greenweight_kg, geometry) %>%
  group_by(ID, ANO, geometry) %>%
  summarise(
    MAT = mean(maturity_stage),
    LENGTH = mean(length_total_cm),
    LENGTH_P75 = quantile(length_total_cm, probs = 0.75, na.rm = TRUE),
    CATCH = sum(krill_greenweight_kg)
  )

# Cargar y procesar datos de SPR ------------------------------------------

SPR_Total <- read_csv("~/DOCAS/LBSPR_Krill/LBSPR_Krill/SPR_Total.csv")

SPR_long <- SPR_Total %>%
  pivot_longer(-Year, names_to = "ID", values_to = "Value") %>%
  mutate(Value = str_remove_all(Value, "[()]")) %>%
  separate(Value, into = c("SPR", "SE SPR"), sep = " ", fill = "right", convert = TRUE) %>%
  rename(ANO = Year)



# Cargar datos de Biomasa AMlR

biomass <- read_csv("~/DOCAS/Data/Biomass_Krill_Trend/biomassurvey.csv") %>% 
  rename(ID = Strata,
         ANO = Year) %>% 
  mutate(ID =  recode(ID, 
                      "West" = "GS",
                      "Joinville" = "SSWI",
                      "Elephant" = "EI",
                      "Bransfield" = "BS",
                      "SouthJoin-With_AP_is_WESJ" = "JOIN")) %>% 
  filter(ID != "WestElephant(AP)")
# Unir bases de datos -----------------------------------------------------

datasum1 <- datasum %>%
  pivot_wider(names_from = variable, values_from = meanvalue)
merged_data <- full_join(dplyr::select(as_tibble(datasum1), -geometry) %>% mutate(ANO = as.integer(ANO)),
                          dplyr::select(as_tibble(ohbiostr2), -geometry) %>% mutate(ANO = as.integer(ANO)),
                          by = c("ID", "ANO")) %>%
  mutate(ID = str_replace_all(ID, c("Extra" = "GS", "SSIW" = "SSWI"))) %>%
  filter(ID != "Outer")

merged_data_all <- full_join(merged_data, SPR_long, by = c("ID", "ANO"))


# Unio con base de AMLR biomass

data_large2<- full_join(merged_data_all, biomass, by = c("ID", "ANO"))

## Guardo la data

write_csv(data_large2, "datapost_LBSPR.csv")

##