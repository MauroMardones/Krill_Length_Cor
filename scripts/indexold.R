#' ---
#' title: "Supporting Information 2"
#' subtitle: "Spatial and temporal variability in the intrinsic productivity of Antarctic krill (Euphausia superba) along the Western Antarctic Peninsula under environmental and life history scenarios"
#' author: "Mardones, M; Jarvis Mason, E.T.;  Santa Cruz, F.; Watters, G.; Cárdenas, C.A"
#' date:  "`r format(Sys.time(), '%d %B, %Y')`"
#' bibliography: seaice.bib
#' csl: apa.csl
#' link-citations: yes
#' linkcolor: blue
#' output:
#'   bookdown::html_document2:
#'     fig_caption: yes
#'     keep_md: true
#'     toc: true
#'     toc_deep: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: false
#'     theme: cosmo
#'     fontsize: 0.9em
#'     linestretch: 1.7
#'     html-math-method: katex
#'     self-contained: true
#'     code-tools: true
#'     number_sections: false
#' editor_options: 
#'   markdown: 
#'     wrap: 72
#' ---
#' 
#' 
## ----setup1-----------------------------------------------------------------------------------------------
rm(list = ls())
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      fig.align = 'center',
                      dev = 'jpeg',
                      dpi = 300)
#XQuartz is a mess, put this in your onload to default to cairo instead
options(bitmapType = "cairo") 
# (https://github.com/tidyverse/ggplot2/issues/2655)
# Lo mapas se hacen mas rapido

#' 
#' 
#' 
## ----lib, message=F, echo= TRUE---------------------------------------------------------------------------
library(here)
#analisis
library(ggsignif)
library(ggrepel)
library(ggpubr)
#library(inlmisc)
library(nortest) #para testear distribucion
library(skimr) #provides a frictionless approach to summary statistics 
library(lubridate)
library(easystats) # multiples unciones analiticas
library(lme4)
library(skimr)
library(readxl)
library(fitdistrplus)
# vizualizacion
library(ggridges)
library(sf)
library(GGally)
library(tidyverse, quietly = TRUE)
library(knitr, quietly = TRUE)
library(kableExtra)
library(raster)
library(egg)
library(car) #Variance inflation Factor
library(ggthemes)
library(sjPlot)
library(GGally)
library(CCAMLRGIS)
library(modelsummary)
library(tinytable)

#' 
#' # Background
#' 
#' The following document intends to carry out a complementary
#' methodological analysis to correlate environmental variables with the
#' population dynamics of krill (*Euphausia superba*), in this case, in spatial complexity and with a biological component like lengths from fishery monitoring.
#' 
#' # Hypothesis
#' 
#' The primary inquiry pertains to the environmental and  effects of distinct
#' physical and oceanographic factors in the Southern Ocean on the krill
#' population. Our aim is to examine the population structure via krill
#' length component from fishery and determine whether any changes were driven by
#' environmental factors across any of its dimensions.
#' 
#' # Objective
#' 
#' Once the correlation and effects on the population and/or fishing
#' indicators on krill have been verified, this analysis aims to have a
#' time series of the environmental variable to incorporate into the stock
#' assessment process. Similar work in @Wang2021 but with a longest fishery
#' history.
#' 
#' 
#' # Methodology
#' 
#' ## Spatial heterogeneity 
#' 
#' Figure \@ref(fig:Figure1) (S2 Fig 1 now) illustrates the spatial heterogeneity of key environmental and population variables across different management units (BS, EI, GS, JOIN, SSWI) in the Antarctic Peninsula, where krill populations are distributed. Biomass and catch trends show substantial variability between strata, with BS and GS exhibiting the highest biomass estimates, whereas JOIN and SSWI display comparatively lower values. Catch levels also differ significantly, with BS and GS experiencing the most intensive exploitation, while SSWI and JOIN have minimal catch records.  Environmental variables further highlight this heterogeneity. Sea surface temperature (SST) trends vary among strata, with GS and JOIN exhibiting a slight warming trend over time, while BS and EI remain relatively stable. Sea ice cover differs substantially, with GS showing consistently high coverage, whereas JOIN presents greater fluctuations. Chlorophyll-a (Chl-a) levels, a proxy for primary productivity, also vary across regions, with BS and GS showing declining trends, while EI and SSWI remain relatively stable at lower concentrations.  Given these spatial differences in both krill population metrics and environmental conditions, it is essential to analyze and estimate SPR at this local scale. The observed heterogeneity supports the need for spatially explicit management, as krill population dynamics are likely influenced by regional environmental drivers. By incorporating SPR analysis at this resolution, we can provide a spatial explicit framework for sustainable krill management, ensuring that conservation efforts align with local population and ecosystem characteristics.
#' 
## ----Figure1, echo=FALSE, out.width='80%', fig.cap="Spatial heterogeneity of krill biomass, catch, and environmental variables across management units in the Antarctic Peninsula (BS, EI, GS, JOIN, SSWI)"----
knitr::include_graphics('index_files/figure-html/var_env.png')

#' 
#' 
#' ## Length structure krill 48 Statistical Subarea
#' 
#' Another important piece of information for a stock evaluation refers to
#' the biological components such as average sizes and weights across areas
#' and years. To do this, we will explore the biological data and prepare
#' the output to add it into stock assessment integrate model
#' [@Methot2013].
#' 
#' 
#' ## Models
#' 
#' 
#' To evaluate the spatial and temporal variability of krill length and its relationship with environmental covariates, we applied a series of regression models, including generalized linear models (GLMs) and linear mixed-effects models (LMMs). Initial GLMs were constructed to assess the fixed effects of year (ANO), spatial strata (ID), sea ice concentration, chlorophyll-a (Chla), and sea surface temperature (TSM) on krill length. To account for the hierarchical structure of the data, we incorporated random intercepts for year (ANO) and, in some models, for spatial strata (ID) using the `lme4` package in R. Interactions between ID and ANO were tested to explore whether temporal trends in krill length differed across spatial units. Model selection was based on the Akaike Information Criterion (AIC) and residual deviance, with lower values indicating better model fit. Variance inflation factors (VIF) were calculated to check for multicollinearity among environmental predictors, and when necessary, covariates were standardized or imputed to handle missing data. The inclusion of random effects allowed us to capture unexplained annual variability while accounting for spatial heterogeneity in krill growth patterns. Visualization of model predictions and marginal effects was conducted using the `ggeffects` and `sjPlot` packages, providing insights into how krill length dynamics respond to environmental and spatial drivers over time.  
## ---------------------------------------------------------------------------------------------------------
#Load data procesed
datapost_LBSPR <- read_csv("data/datapost_LBSPR.csv")

#' 
## ---------------------------------------------------------------------------------------------------------
glimpse(datapost_LBSPR)

#' 
#' 
## ---------------------------------------------------------------------------------------------------------
# Filtrar datos para evitar problemas con NA
data_filtered <- data_large2 %>% filter(!is.na(LENGTH), !is.na(LENGTH_P75))

# Scatter plot para LENGTH con tendencia por ID
length <- ggplot(data_filtered, aes(x = ANO, y = LENGTH, color = ID)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # Línea de tendencia por ID
  labs(title = "",
       x = "",
       y = "Mean Krill Length (cm)") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_color_viridis_d(option="F")+
  ylim(2,6)

# Scatter plot para LENGTH_P75 con tendencia por ID
length75 <- ggplot(data_filtered, aes(x = ANO, y = LENGTH_P75, color = ID)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # Línea de tendencia por ID
  labs(title = "",
       x = "Year",
       y = "75th Percentile Krill Length") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_color_viridis_d(option="F")+
  ylim(2,6)

ggarrange(length,
          length75,
          ncol=2,
          common.legend = TRUE)

#' 
#' ## Maps length data
#' 
#' First thing is get different rater layer to join krill data length
#' according different porpoises.
#' 
## ----raster-----------------------------------------------------------------------------------------------
# Cargo linea de costa
coast <- load_Coastline()
coast1<- st_as_sf(coast) 
coast2 = st_transform(coast1, "+proj=latlong +ellps=WGS84")
# con SSMU
ssmu <- load_SSMUs()
ssmu481 <- subset(ssmu[c(2,3,4,5,6,7,17),])
ssmu481a <- st_as_sf(ssmu481) 
ssmu481aa = st_transform(ssmu481a, "+proj=latlong +ellps=WGS84")

# con Statistical Areas con foco en 48.1
suba <- load_ASDs()
suba1 <- subset(suba[(3),])
suba1a<- st_as_sf(suba1) 
suba1aa = st_transform(suba1a, "+proj=latlong +ellps=WGS84")


# Uso las agrupaciones de DMP1
dmp1 <- st_read("~/DOCAS/Mapas/Antarctic_SHPfiles/D1MPA-model-2019.shp",
                quiet=T)
dmp1=st_transform(dmp1, 6932)

# Uso las agrupaciones de Strata
strata <- st_read("~/DOCAS/Mapas/Antarctic_SHPfiles/Strata.shp",
                quiet=T)
strata=st_transform(strata,
                    "+proj=latlong +ellps=WGS84")

# Uso las agrupaciones de Strata Lucas HW
strataclip <- st_read("~/DOCAS/Data/WAP-Heatwaves/peninsula_correct.shp",
                quiet=T)
strataclip=st_transform(strata, 
                        "+proj=latlong +ellps=WGS84")
library(sf)
library(dplyr)

# Unir las bases por ID
data_merged <- left_join(st_as_sf(strata), data_large2, by = "ID")

library(ggplot2)

ggplot() +
  geom_point(data = data_merged, aes(x = Labx, y = Laby, color = LENGTH), size = 3) +
  geom_sf(data = strata %>%
           filter(ID != "Outer"), 
          aes(fill=ID, 
              alpha=0.3))+
  geom_sf(data = coast2, colour="black", fill=NA)+
  scale_color_viridis_c(option = "plasma", na.value = "gray") +  # Escala de color para LENGTH
  labs(title = "Spatial Distribution of LENGTH",
       x = "Longitude",
       y = "Latitude",
       color = "LENGTH") +
  theme_minimal()+
  ylim(230000, 2220000)+
  xlim(-3095349 , -1858911)+
  # coord_sf(crs = 32610)+ #sistema de prpyecccion para campos completos
  coord_sf(crs = 6932)


#' 
#' 
#' 
#' ## Grouping Length data into Grid
#' 
#' ### SSMU 48.1 CCAMLR
#' 
## ----ssmu1------------------------------------------------------------------------------------------------
names(ohbio4)
ohbio5 <- ohbio4 %>% 
  filter(asd_code==481) %>% 
  dplyr::select(6, 7, 8, 9, 10, 11, 12) 
ohbio6 <- st_as_sf(ohbio5 %>% 
                     drop_na(latitude_set_end), 
                   coords = c("longitude_set_end", 
                              "latitude_set_end"),  
                  crs = "+proj=latlong +ellps=WGS84")

#' 
#' This grid has the same characteristics as the environmental data grids
#' that will be called up later. This grid is 1x0.5 degrees which allows a
#' clear visualization of the processes, whether biological and/or
#' environmental.
#' 
## ----gri--------------------------------------------------------------------------------------------------

Grid<- suba1aa  %>% #pm481 es el plot base original linea 481
  sf::st_make_grid(cellsize = c(1,0.5)) %>% # para que quede cuadrada
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_sf()  %>%  # objeto en spatial feature
  dplyr::mutate(cellid = row_number()) 

# Clean the input data by removing duplicate vertices and making the object topologically valid
grid3 <- st_make_valid(Grid)

# Corto la grilla dentro de las SSMU
gridcrop1 <- tmaptools::crop_shape(grid3, 
                                   suba1aa,
                                   polygon = TRUE)

# the first object drives the output geometry
grilen <- grid3 %>%
  st_join(ohbio6) %>% 
  group_by(cellid)

names(grilen)

#' 
#' Test lentgh mean map by Year
#' 
## ----plogri-----------------------------------------------------------------------------------------------
grilen3 <- grilen %>%
  drop_na(length_total_cm) %>% 
  dplyr::group_by(cellid,Year) %>% 
  dplyr::summarise(lepro=mean(length_total_cm))

#' 
#' 
#' solo la grilla
#' 
## ---------------------------------------------------------------------------------------------------------
glvoid <- ggplot() +
  geom_sf(data = coast2, colour="black", fill=NA)+
  geom_sf(data = gridcrop1, colour="black", fill=NA)+
  geom_sf(data= suba1aa, fill=NA, col="red")+
  theme(panel.background = element_rect(fill = 'aliceblue'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
   ylim(230000, 2220000)+
  xlim(-3095349 , -1858911)+
  # coord_sf(crs = 32610)+ #sistema de prpyecccion para campos completos
  coord_sf(crs = 6932)+
  scale_alpha(guide="none")+
  theme_bw()
glvoid

#' 
#' 
## ----plogri2----------------------------------------------------------------------------------------------
gl <- ggplot() +
   geom_sf(data=grilen3,
           aes(fill = lepro), 
           alpha=0.7, color=NA) +
  scale_fill_viridis_b(option="G",
                       name="Krill Length Fishery (cm.)",
                       breaks = seq(0,8,1),
                       direction=-1)+
  geom_sf(data = suba1aa, fill=NA, col=2)+
  geom_sf(data = coast2, colour="black", fill="grey")+
  facet_wrap(~Year, ncol=6)+
  theme(panel.background = element_rect(fill = 'aliceblue'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  # ylim(-70, -60)+
  # xlim(-70, -50)
  # another kind projection
  #coord_sf(crs = 32610)+ #sistema de prpyecccion para campos completos
  ylim(230000, 2220000)+
  xlim(-3095349 , -1858911)+
  
  coord_sf(crs = 6932)
gl

#' 
#' 
#' ###  STRATA
#' 
#' Length composition by Strata CCAMLR to visualization first. First step is group data into to poligons strata.
#' 
## ----wranlingdata-----------------------------------------------------------------------------------------
class(ohbio6)
dim(ohbio6)
glimpse(ohbio6)

class(strata)
glimpse(strata)
dim(strata)


conteo <- sf4 %>% 
  group_by(ID) %>% 
  count()

ohbiopro <- ohbio6 %>%
  dplyr::group_by(gear_type, Year, Month, geometry) %>% 
  dplyr::summarize(tapro = mean(length_total_cm))

# comoprobar si tengo datos duplicados
strata2 <- st_make_valid(strata)
ohbio62 <- st_make_valid(ohbio6)



sf3 <- st_join(strata2, ohbiopro)
sf4 <- st_join(strata2, ohbio6)
dim(sf4)
dim(sf3)

#' 
#' 
## ----saveda, eval=FALSE-----------------------------------------------------------------------------------
# save("sf4", file = "sf4.RData")
# save("sf3", file = "sf3.RData")
# load("~/DOCAS/Data/Krill_Length_Cor/sf4.RData")

#' 
#' 
#' scattter plot lenght data
## ----warning=F--------------------------------------------------------------------------------------------
lentraplot <- ggplot(sf4 %>% 
                   filter(Year>2000,
                      ID !="Outer"), 
               aes(Year, tapro,
               fill=Month))+
    geom_point(alpha=0.3, 
               shape=21, 
               show.legend = T) +
    scale_fill_viridis_c(option="G")+
    geom_hline(yintercept = 4.16,
               color = "black",
               linetype  = 2)+
    stat_smooth(method = "loess",
                col="red")+
    theme_bw()+ 
    facet_wrap(.~ID)+
    theme(axis.text.x = element_text(angle = 90, hjust = 2))+
    guides(fill = guide_legend(reverse=F))+
    
    ylab("") +
    xlab("") 
lentraplot


#' histogram length data
#' 
#' 
## ---------------------------------------------------------------------------------------------------------
jzstrata <- ggplot(sf4 %>% 
               filter(Year>2000,
                      ID !="Outer"),
             aes(x=length_total_cm, 
                 y = as.factor(Year), 
                 fill=ID))+
  geom_density_ridges(stat = "binline", bins = 30, 
                      scale = 1.9, 
                      draw_baseline = FALSE,
                      alpha=0.9)+
  facet_wrap(.~ID, ncol=7) +   
  geom_vline(xintercept = 3.6, color = "red")+
  scale_x_continuous(breaks = seq(from = 1, to = 10, 
                                  by = 2))+
  scale_y_discrete(breaks = seq(from = 2000, 
                                to = 2020, by = 1))+
  scale_fill_viridis_d(name="Strata",
                       option="F")+
  theme_minimal()+
  xlab("Length (cm.)")+
  ylab("")
jzstrata

#' 
#' 
#' 
#' maps length
#' 
## ---------------------------------------------------------------------------------------------------------
glstrata <- ggplot() +
   geom_sf(data=sf3%>% 
             drop_na(tapro) %>% 
               filter(Year>1999,
                      ID !="Outer"),
           aes(fill = tapro), 
           color=NA) +
  scale_fill_viridis_b(option="C",
                       name="cm.",
                       breaks = seq(0,6,0.5),
                       direction=-1)+
  geom_sf(data = strata, fill=NA, col=2)+
  geom_sf(data = coast2, colour="black", fill="grey")+
  facet_wrap(~Year, ncol=6)+
  theme(panel.background = element_rect(fill = 'aliceblue'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  # ylim(-70, -60)+
  # xlim(-70, -50)
  # another kind projection
  ylim(230000, 2220000)+
  xlim(-3095349 , -1858911)+
  #coord_sf(crs = 32610)+ #sistema de prpyecccion para campos completos
  coord_sf(crs = 6932)+
  labs(title = "Mean Length Krill by Strata and Year")
glstrata

#' 
#' Prepare template length data by strata for posterior analysis like "Intrinsic Productivity". We can found this in this [link](https://github.com/MauroMardones/LBSPR_Krill)
#' 
#' #### Grouping by strata
#' 
## ----eval=FALSE, echo=TRUE--------------------------------------------------------------------------------
# # Maybe this could be a loop , but i didnt find the way...
# # Elephan Island
# ei <- sf4 %>%
#   drop_na(length_total_cm) %>%
#   filter(ID== "EI")
# # cut in order
# ei$catlon <- cut(x = ei$length_total_cm,
#                  breaks = seq(0,7,0.2),
#                  labels = seq(0,6.8,0.2),
#                  right = FALSE)
# 
# eit <- table(ei$Year, ei$catlon)
# eit1 <-t(as.data.frame.matrix(eit)); class(eit1)
# 
# 
# # SSIW
# ssiw <- sf4 %>%
#   drop_na(length_total_cm) %>%
#   filter(ID== "SSIW")
# # cut in order
# ssiw$catlon <- cut(x = ssiw$length_total_cm,
#                  breaks = seq(0,7,0.2),
#                  labels = seq(0,6.8,0.2),
#                  right = FALSE)
# 
# ssiwt <- table(ssiw$Year, ssiw$catlon)
# ssiwt1 <-t(as.data.frame.matrix(ssiwt)); class(ssiwt1)
# 
# # BS
# bs <- sf4 %>%
#   drop_na(length_total_cm) %>%
#   filter(ID== "BS")
# # cut in order
# bs$catlon <- cut(x = bs$length_total_cm,
#                  breaks = seq(0,7,0.2),
#                  labels = seq(0,6.8,0.2),
#                  right = FALSE)
# 
# bst <- table(bs$Year, bs$catlon)
# bst1 <-t(as.data.frame.matrix(bst)); class(bst1)
# 
# # JOIN  (very poor data, just a couple years)
# join <- sf4 %>%
#   drop_na(length_total_cm) %>%
#   filter(ID== "JOIN")
# # cut in order
# join$catlon <- cut(x = join$length_total_cm,
#                  breaks = seq(0,7,0.2),
#                  labels = seq(0,6.8,0.2),
#                  right = FALSE)
# 
# joint <- table(join$Year, join$catlon)
# joint1 <-t(as.data.frame.matrix(joint)); class(joint1)
# 
# # Extra
# extra <- sf4 %>%
#   drop_na(length_total_cm) %>%
#   filter(ID== "Extra")
# # cut in order
# extra$catlon <- cut(x = extra$length_total_cm,
#                  breaks = seq(0,7,0.2),
#                  labels = seq(0,6.8,0.2),
#                  right = FALSE)
# 
# extrat <- table(extra$Year, extra$catlon)
# extrat1 <-t(as.data.frame.matrix(extrat)); class(extrat1)

#' 
#' Generate the files to use in intrinsic productivity analysis.
#' 
## ----eval=FALSE, echo=TRUE, warning=FALSE-----------------------------------------------------------------
# # A su vez puedo generar el archivo por separado
# write.csv(eit1, "lenghtEI.csv", sep = ",", row.names = TRUE)
# write.csv(ssiwt1, "lenghtSSIW.csv", sep = ",", row.names = TRUE)
# write.csv(bst1, "lenghtBS.csv", sep = ",", row.names = TRUE)
# write.csv(joint1, "lenghtJOIN.csv", sep = ",", row.names = TRUE)
# write.csv(extrat1, "lenghtExtra.csv", sep = ",", row.names = TRUE)
# 

#' 
#' 
#' ## Environmental data
#' 
#' Load environmental data to merge with length.
#' 
#' 
## ----echo=FALSE-------------------------------------------------------------------------------------------
load("~/DOCAS/Data/KrillLows/DataEnvKrill.RData")

#' 
#' Get data specific object
#' 
## ----include=FALSE----------------------------------------------------------------------------------------
ls()
get("dataenvf")

#' 
#' Join length and environental data by `cellid` and `Year` with a reduce DB.
#' 
## ---------------------------------------------------------------------------------------------------------
class(dataenvf)

envgru <- dataenvf %>%
  dplyr::select(1, 2, 3, 6, 9) %>% 
  drop_na(ANO.x) %>% 
  dplyr::rename(Year=ANO.x) %>% 
  dplyr::group_by(cellid,Year) %>% 
  dplyr::summarise(meansic2=mean(meansic),
                   meanchl2=mean(meanchl),
                   meantsm2=mean(meantsm))
dim(envgru)
summary(envgru)

# Test
envgru2 <- dataenvf %>%
  dplyr::select(1, 2, 3, 6, 9, 10) %>% 
  drop_na(ANO.x) %>% 
  dplyr::rename(Year=ANO.x) %>% 
  dplyr::group_by(cellid,Year, geometry) %>% 
  dplyr::summarise(meansic2=mean(meansic),
                   meanchl2=mean(meanchl),
                   meantsm2=mean(meantsm))

#' 
#' Join merge object in a new data base.
#' 
## ---------------------------------------------------------------------------------------------------------
names(grilen3)
names(envgru)
envlen <- merge(grilen3, envgru, 
                 by = c("cellid", "Year"))
# change names

envname <- envlen %>% 
  dplyr::rename(Length=lepro,
         SIC=meansic2,
         CHla=meanchl2,
         SSM=meantsm2)
head(envname)

#' 
#' # Results
#' 
#' ## GLM Length-Environmental
#' 
#' Debo trabajar ciertos componentes de los datos para poder hacer el
#' analisis bajo supuestos estructurales.
#' 
#' -   Comprobar distribución de Longitudes medias de krill
#' -   Cambiar a factor `Year` y `cellid`
#' -   Filtrar datos `NA`
#' -   Filtrar outliers en datos ambientales
#' 
#' ### Test Correlation
#' 
#' Test de comportamiento de la variable de longitudes `lenpro` mediante 2
#' test; 1) Shapiro-Wilk `shapiro.test()` y Kolmogorov-Smirnov `ks.test()`
#' 
## ---------------------------------------------------------------------------------------------------------
shapiro.test(envname$Length)
ks.test(envname$Length, "pnorm", mean(envname$Length), sd(envname$Length))
tal<-ggplot(envname, aes(x=Length)) + 
  geom_histogram(color="red", fill="white")+
  theme_minimal()+
  labs(x="Length Mean Krill (cm.)",
       y="Frecuency")
tal

#' 
#' ### Cambiar a factor `Year` y `cellid`
#' 
## ---------------------------------------------------------------------------------------------------------
envname$Year<-as.factor(envname$Year)
envname$cellid<-as.factor(envname$cellid)
glimpse(envname)

#' 
#' ### Filtrar datos `NA` if we have. Test
#' 
## ---------------------------------------------------------------------------------------------------------
# Are there missing values?
colSums(is.na(envname))

#' 
#' ### Filter outliers environmental and biological data
#' 
#' Test trought plot all variables in `envlen` object
#' 
## ---------------------------------------------------------------------------------------------------------
boxsic <- ggplot(envname)+
  geom_boxplot(aes(Year, SIC),
               outlier.colour = "red")+
  ylim(0,50)+
  theme_minimal()
boxtsm <- ggplot(envname)+
  geom_boxplot(aes(Year, SSM-270),
               outlier.colour = "red")+
  labs(y="TSM")+
  theme_minimal()
boxchl <- ggplot(envname)+
  geom_boxplot(aes(Year, CHla),
               outlier.colour = "red")+
  theme_minimal()
ggarrange(boxsic, boxtsm, boxchl, nrow = 3)


#' 
#' is necessary get off outliers from Sea ice index. Get environmental
#' outliers data and again...
#' 
## ----warning=F, message=FALSE-----------------------------------------------------------------------------
envlen2 <- envlen %>% 
  filter(meansic2<100)

#plot 2
         
boxsic2 <- ggplot(envlen2)+
  geom_boxplot(aes(Year, meansic2),
               outlier.colour = "red")+
  theme_minimal()
boxtsm2 <- ggplot(envlen2)+
  geom_boxplot(aes(Year, meantsm2),
               outlier.colour = "red")+
  theme_minimal()
boxchl2 <- ggplot(envlen2)+
  geom_boxplot(aes(Year, meanchl2),
               outlier.colour = "red")+
  theme_minimal()

p<-ggplot(envlen2, aes(x=meansic2)) + 
  geom_histogram(color="black", fill="white")+
  theme_minimal()

q<-ggplot(envlen2, aes(x=meantsm2)) + 
  geom_histogram(color="black", fill="white")+
  theme_minimal()

t<-ggplot(envlen2, aes(x=meanchl2)) + 
  geom_histogram(color="black", fill="white")+
  theme_minimal()

ggarrange(boxsic2, boxtsm2, boxchl2, p, q, t, nrow = 2, ncol=3)

#' 
#' ## Correlation Analysis
#' 
#' ## Simple correlation identification toget information about variables
#' 
#' Firts filter in our dataframe `envlen3`.
#' 
#' Regarding difference between `Spearman` and `pearson`, both, the
#' Spearman test and the Pearson test are statistical methods used to
#' assess the correlation between two variables. The main difference
#' between them is that the Pearson test evaluates the linear correlation
#' between two continuous variables, while the Spearman test evaluates the
#' monotonic correlation between two continuous or order variables.
#' 
#' In the case of the Pearson test, the degree of association between two
#' continuous variables is measured through a correlation coefficient that
#' varies between -1 and 1. A value of 1 indicates a perfectly positive
#' correlation, a value of -1 indicates a perfectly negative correlation,
#' and a value of 0 indicates no correlation between the two variables.
#' 
#' On the other hand, the Spearman test is based on the range of the
#' variables, instead of the actual values. In other words, this test
#' evaluates the correlation between two order variables, where the values
#' of each variable are ranked from lowest to highest, and ranges are used
#' instead of actual values. Spearman's correlation coefficient also varies
#' between -1 and 1, but it measures the monotonic correlation between two
#' variables, that is, if one variable increases, the other variable also
#' increases or decreases[@McCulloch2001].
#' 
#' In summary, the main difference between the Spearman test and the
#' Pearson test is that the former is used to assess the monotonic
#' correlation between two order variables, while the latter is used to
#' assess the linear correlation between two continuous variables.
#' 
#' -   Spearman's Rank Correlation Coefficient
#' 
#' This coefficient is used to see if there is any significant relationship
#' between the two datasets, and operates under the assumption that the
#' data being used is ordinal, which here means that the numbers do not
#' indicate quantity, but rather they signify a position of place of the
#' subject's standing (e.g. 1st, 2nd, 3rd, etc.)
#' 
#' $\begin{aligned} r_s = \frac{\sum_{i=1}^n (x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum_{i=1}^n (x_i - \bar{x})^2}\sqrt{\sum_{i=1}^n (y_i - \bar{y})^2}} \end{aligned}$
#' 
#' -   Pearson Product-Moment Coefficient
#' 
#' This is the most widely used correlation analysis formula, which
#' measures the strength of the 'linear' relationships between the raw data
#' from both variables, rather than their ranks. This is an dimensionless
#' coefficient, meaning that there are no data-related boundaries to be
#' considered when conducting analyses with this formula, which is a reason
#' why this coefficient is the first formula researchers try.
#' 
#' $\begin{aligned} r = 1- \frac{6\sum_{i=1}^n D_{i}^n}{n (n^2 - 1)}\end{aligned}$
#' 
#' (Is posible report outpus by @Report2023)
#' 
## ---------------------------------------------------------------------------------------------------------
# change to data frame
envlen3 <- as.data.frame(envlen2)
envlen4 <- envlen3 %>% 
  dplyr::select(3, 4, 5, 6)

#' 
#' Plot to show correlation trough different ways. First, the matri
#' correlation
#' 
## ---------------------------------------------------------------------------------------------------------
kableExtra::kable(cor(envlen4)) %>% 
  kable_styling()
# covariance matrix
kableExtra::kable(cov(envlen4)) %>% 
  kable_styling()

#' 
#' 
## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------
pairs.panels(envlen4 %>% 
               dplyr::rename(Length=lepro,
         SIC=meansic2,
         CHla=meanchl2,
         SSM=meantsm2),
             smooth = TRUE,      
             scale = FALSE,      
             density = TRUE,     
             ellipses = TRUE,  
             method = "pearson",
             pch = 21,           
             lm = FALSE,        
             cor = TRUE,         
             jiggle = FALSE,     
             factor = 2,        
             hist.col = 4,       # Color de los histogramas
             stars = TRUE)

#' 
## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------
resu <- correlation(envlen4 %>%
               dplyr::rename(Length=lepro,
         SIC=meansic2,
         CHla=meanchl2,
         SSM=meantsm2))
resu
summary(resu, redundant=TRUE)


#' 
#' We can identify through a correlation matrix the data of our set,
#' whether it is positive or negative. See Figure \@ref(fig:coorplot).
#' 
## ----corrplot, fig.cap="Correlation Matrix", message=FALSE, warning=FALSE---------------------------------
resu %>%
  summary(redundant = FALSE) %>%
  plot()

#' 
#' Here we can identify the correlation that exists between krill length
#' and chlorophyla (-) and krill length and SST (+)
#' 
#' ## GLM models
#' 
#' Come back with `envlen2` object.
#' 
## ---------------------------------------------------------------------------------------------------------
names(envlen2)
# [1] "cellid"   "Year"     "lepro"    "meansic2" "meanchl2"
# [6] "meantsm2" "geometry"
envlen2r <- envlen2 %>% 
  rename(SIC = meansic2,
         Chl = meanchl2,
         SST = meantsm2,
         Length = lepro)

env1 <- glm(Length ~ Year,
            data = envlen2r)
env2 <- glm(Length ~ Year + 
              SIC,
            data = envlen2r)
env3 <- glm(Length ~ Year + 
              SIC +
             Chl ,
            data = envlen2r)
env4 <- glm(Length ~ Year + 
              SIC +
              Chl + 
              SST ,
            data = envlen2r)
#spatial component
env5 <- glm(Length ~ Year + 
              SIC +
              Chl +
              SST +
              cellid,
            data = envlen2r)

#' 
#' test with @Ludecke2021
#' 
#' Table with significance level (***).
## ---------------------------------------------------------------------------------------------------------
#summary(env4)
tab_model(env1, 
          env2,
          env3, 
          env4, p.style = "stars")


#' 
#' 
#' Table comparing performance model. 
## ----warning=F--------------------------------------------------------------------------------------------
compare_performance(env1, 
                    env2,
                    env3,
                    env4,
                    env5,
                    env1m,
                    rank = TRUE, verbose = FALSE)

#' 
#' Regarding this result, we can select **best model** and check some assumtion like:
#' 
#' - Colinealidad
#' - Data points influyentes
#' - Homoscedasticidad
#' - Normalidad de los residuos 
#' - Independencia
#' 
#' In this case, is `env4`
#' 
## ----fig.height=3, fig.width=3,  warning=F----------------------------------------------------------------
check_model(env1)

#' 
## ---------------------------------------------------------------------------------------------------------
plot(compare_performance(env1, 
                         env2, 
                         env3,
                         env4,
                         rank = TRUE),
     name="LM to length krill data vs Env")

#' 
#' Comparative table acrosss models.
#' 
## ---------------------------------------------------------------------------------------------------------
compare_performance(env1, 
                    env2, 
                    env3,
                    env4, 
                    env5)

report(env4)

#' 
#' 
## ---------------------------------------------------------------------------------------------------------
AIC(env1)
AIC(env2)
AIC(env3)
AIC(env4)
deviance(env1)
deviance(env2)
deviance(env3)
deviance(env4)

library(lmtest)
lrtest0<-lrtest(env1,env2,env3,env4)
lrtest0
plot_model(env4)+
  theme_bw()

#' 
#' ## GLMM models
#' 
#' Random effects are a way to model variability in data that comes from
#' factors that cannot be directly measured or controlled. In the context
#' of statistical models, random effects refer to variables that are
#' assumed to have an unknown probability distribution, and are included in
#' the model to explain some of the variation in the data.
#' 
#' For example, in a study comparing the test scores of students at
#' different schools, random effects refer to differences between schools
#' that cannot be explained by variables measured in the study, such as
#' students' socioeconomic status. These differences may be caused by
#' factors such as the quality of teaching, school culture, or geographic
#' location.
#' 
#' Random effects are often modeled by using mixed effects models, which
#' combine random and fixed effects in the same model. Fixed effects are
#' those that are assumed to be constant for all study units and are
#' directly measured, while random effects are those that are assumed to
#' vary randomly across study units and cannot be directly measured.
#' 
#' In short, random effects are a way of modeling variability in data that
#' cannot be directly explained by the variables measured in the study, and
#' are included in the model to improve the precision of the estimates and
#' reduce the potential for bias [@McCulloch2001].
#' 
#' In this cae we try test spatial componenten in `cellid` variable like
#' random effects with `lme4` package [@Bates2015].
#' 
## ---------------------------------------------------------------------------------------------------------
env1m <- lmer(Length ~ Year + 
            Chl + 
             SIC + 
             SST + (1|cellid), 
           data = envlen2r)

env2m <- lmer(Length ~ Year + 
            Chl + 
             SIC + 
             SST + (1|Year), 
           data = envlen2r)

#' 
#' 
## ---------------------------------------------------------------------------------------------------------
# graficar efectos aleatorios
pre <- plot_model(env1m,type = "re", 
                facet.grid=FALSE, 
                free.scale = FALSE, 
                title = NULL, 
                vline.color = "darkgrey",
                sort.est = TRUE,
                colors = "Set1",
                show.data = TRUE,
                jitter = 0.2,
                terms = "Days")+
    theme_few()

pest <- plot_model(env1m,type = "est", 
                facet.grid=FALSE, 
                free.scale = FALSE, 
                title = NULL, 
                vline.color = "darkgrey",
                sort.est = TRUE,
                colors = "Set2",
                show.data = TRUE,
                jitter = 0.2)+
  theme_few()

#' 
#' 
## ----warning=FALSE, message=FALSE, include=FAlSE----------------------------------------------------------
ggarrange(pre, pest, ncol = 2)

#' 
#' 
#' 
## ---------------------------------------------------------------------------------------------------------
env_ranef <- ranef(env1m)
summary(env_ranef)
mean(env_ranef$cellid[, 1])

report(env1m)

#' 
#' 
## ---------------------------------------------------------------------------------------------------------
library(lme4)
library(sjPlot)

env1m_Chla <- lmer(LENGTH ~ ANO + Chla + (1|ID), data = data_large2)
env1m_tsm  <- lmer(LENGTH ~ ANO + tsm + (1|ID), data = data_large2)

env1 <- glm(LENGTH ~ ANO,
            data = data_large2)
env2 <- glm(LENGTH ~ ANO+
               seaice,
             data = data_large2)
env3 <- glm(LENGTH ~ ANO+
               seaice+
               tsm,
             data = data_large2)
env4 <- glm(LENGTH ~ ID + 
              seaice +
              Chla + 
              tsm ,
            data = data_large2)
#spatial component
env5 <- glm(lepro ~ Year + 
              meansic2 +
              meanchl2 + 
              meantsm2 +
              cellid,
            data = envlen2)

env1m <- lmer(LENGTH ~ ID + 
                Chla + 
                seaice + 
                 (1|ANO), 
              data = data_large2)

pre <- plot_model(env1m,type = "re", 
                  facet.grid=TRUE, 
                  free.scale = FALSE, 
                  title = NULL, 
                  vline.color = "darkgrey",
                  sort.est = TRUE,
                  colors = "Set1",
                  show.data = TRUE,
                  jitter = 0.2,
                  terms = "Days")+
  theme_minimal()

pest <- plot_model(env1m,type = "est", 
                   facet.grid=FALSE, 
                   free.scale = FALSE, 
                   title = NULL, 
                   vline.color = "darkgrey",
                   sort.est = TRUE,
                   colors = "Set2",
                   show.data = TRUE,
                   jitter = 0.2)+
  theme_minimal()

ggarrange(pre, pest, ncol = 2)


data_large4<- na.omit(data_large2)
data_large4 <- data_large4 %>%
  mutate(across(c(ANO, Chla, seaice, tsm), scale))

library(lme4)
model1 <- lmer(LENGTH ~ ID + ANO + Chla + seaice + tsm + (1|ANO), data = data_large4)
summary(model1)


model2 <- lmer(LENGTH ~ ANO + Chla + seaice + tsm + (1|ID) + (1|ANO), data = data_large4)
summary(model2)


tab_model(model1, show.re.var = TRUE)

library(ggplot2)
library(ggeffects)

# Efecto de Año
plot(ggpredict(model1, terms = "ANO"))

# Efecto de ID
plot(ggpredict(model1, terms = "ID"))

# Efecto de Chla
plot(ggpredict(model1, terms = "Chla"))

model_null <- lm(LENGTH ~ ANO + Chla + seaice + tsm, data = data_large4) # sin efectos aleatorios
anova(model1) # Comparación de modelos

plot(model1)  # Residuos vs valores ajustados
qqnorm(resid(model2))
qqline(resid(model2))  # Normalidad de residuos


model3 <- lmer(LENGTH_P75 ~ ID + ANO + Chla * tsm + seaice + (1|ANO), data = data_large4)
summary(model3)

pest <- plot_model(model3,type = "est", 
                   facet.grid=FALSE, 
                   free.scale = FALSE, 
                   title = NULL, 
                   vline.color = "darkgrey",
                   sort.est = TRUE,
                   colors = "Set2",
                   show.data = TRUE,
                   jitter = 0.2)+
  theme_minimal()



#' 
#' 
#' # Conclusion
#' 
#' -   On the one hand, the models with mixed effects serve to verify the
#'     influence of the spatial component, in this case each cell y in
#'     which the data of the dependent variable (krill sizes) and the
#'     independent variable (environmental variables) were considered.
#' 
#' -   The influence of environmental variables on the sizes of the krill
#'     fishery is corroborated. The environmental variable with the
#'     greatest impact on krill sizes is Chlorophyll in negative terms. In
#'     other words, the more chlorophyll in the environment, the sizes
#'     decrease because there is greater recruitment due to the abundance
#'     of substrate for the krill population.
#' 
#' -   In a way, it is proof that the krill population structure is
#'     influenced not only by fishing pressure, but also by environmental
#'     conditions.
#' 
#' -   With these results, the environmental component is solidly
#'     incorporated into the krill stock assessment model in the Antarctic
#'     Peninsula, specifically in Subarea 48.1.
#' 
#' # References
#' 
#' 
