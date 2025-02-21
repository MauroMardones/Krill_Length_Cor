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



# carga data

data_large2 <- read_csv("datapost_LBSPR.csv")

## Description

# To analyze the relationship between spawning potential ratio (SPR) and 
# various environmental and fishery-related variables, a series of scatter 
# plots were generated using the ggplot2 package in R. First, the SPR values were 
# lagged by one year (SPR_lag) to account for potential delayed effects of fishing
# pressure and environmental conditions. Scatter plots were then created to visualize 
# the relationships between SPR_lag and catch, biomass, sea surface temperature (SST), 
# and chlorophyll-a (Chl-a), using geom_point() for data representation 
# and geom_smooth(method = "lm") to fit linear regression models. 
# To assess correlation strength, the Pearson correlation coefficient 
# (stat_cor(method = "pearson")) was displayed within each plot.
# Additionally, a facet-wrapping approach (facet_wrap(~ID)) was implemented to 
# compare trends across different spatial strata (ID). The same methodology was
# applied to analyze the relationship between the 75th percentile of krill length
# (LENGTH_P75) and environmental variables. This visualization approach facilitates 
# the identification of significant correlations and spatial patterns in the data,
# providing insights into the potential drivers of krill population dynamics.



# Visualización -----------------------------------------------------------

# cobn lag
# Crear una nueva variable con SPR retrasado un año
data_large3 <- data_large2 %>%
  arrange(ID, ANO) %>%  # Asegurar que los datos están ordenados
  group_by(ID) %>%
  mutate(SPR_lag = lag(SPR)) %>%
  ungroup()

# Gráfico de dispersión con SPR retrasado
spr2 <- ggplot(data_large3, aes(y = SPR_lag, x = CATCH/10000, label = ANO)) + 
  geom_point(size = 1.5, alpha = 0.7) +  
  geom_smooth(method = "lm", aes(group = ID), 
              col="black", se = FALSE) +  
  stat_cor(method = "pearson", label.x.npc = "left", col="blue") +
  facet_wrap(~ID, ncol = 5, scales = "free_x") +  
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=6),
        axis.text.y  = element_text(size=6),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_line(colour = "white", size = 0.1),
        legend.position = "none")+
  scale_color_viridis_d() + 
  ylim(0, 0.9) +  
  geom_text(aes(label = ANO), size = 2, vjust = -1, hjust = 0.5) +
  labs(y = "SPR (Lag)", x = "Catch (t)")

# Gráfico de dispersión con SPR retrasado para bio
spr3 <- ggplot(data_large3, aes(y = SPR_lag, x = biot/1000, label = ANO)) + 
  geom_point(size = 1.5, alpha = 0.7) +  
  geom_smooth(method = "lm", aes(group = ID), 
              col="black", se = FALSE) +  
  stat_cor(method = "pearson", label.x.npc = "left", col="blue") +
  facet_wrap(~ID, ncol = 5, scales = "free_x") +  
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=6),
        axis.text.y  = element_text(size=6),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_line(colour = "white", size = 0.1),
        legend.position = "none")+
  scale_color_viridis_d() + 
  #ylim(0, 0.9) +  
  geom_text(aes(label = ANO), size = 2, vjust = -1, hjust = 0.5) +
  labs(y = "SPR (Lag)", x = "Survey AMLR Biomass")


# Gráfico de dispersión con SPR retrasado para bio
spr4 <- ggplot(data_large2, aes(y = LENGTH_P75, 
                                x = tsm-270, 
                                label = ANO)) + 
  geom_point(size = 1.5, alpha = 0.7) +  
  geom_smooth(method = "lm", aes(group = ID), 
              col="black", se = FALSE) +  
  stat_cor(method = "pearson", label.x.npc = "left", col="blue") +
  facet_wrap(~ID, ncol = 5, scales = "free_x") +  
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=6),
        axis.text.y  = element_text(size=6),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_line(colour = "white", size = 0.1),
        legend.position = "none")+
  scale_color_viridis_d() + 
  #ylim(0, 0.9) +  
  geom_text(aes(label = ANO), size = 2, vjust = -1, hjust = 0.5) +
  labs(y = "75th Percentile of Length", x = "SST")

# Gráfico de dispersión con SPR retrasado para 
spr5 <- ggplot(data_large2, aes(y = LENGTH_P75, 
                                x = Chla, 
                                label = ANO)) + 
  geom_point(size = 1.5, alpha = 0.7) +  
  geom_smooth(method = "lm", aes(group = ID), 
              col="black", se = FALSE) +  
  stat_cor(method = "pearson", label.x.npc = "left", col="blue") +
  facet_wrap(~ID, ncol = 5, scales = "free_x") +  
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=6),
        axis.text.y  = element_text(size=6),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_line(colour = "white", size = 0.1),
        legend.position = "none")+
  scale_color_viridis_d() + 
  #ylim(0, 0.9) +  
  geom_text(aes(label = ANO), size = 2, vjust = -1, hjust = 0.5) +
  labs(y = "75th Percentile of Length", x = "Chl-a")

# Gráfico de dispersión con SPR retrasado para 
spr6 <- ggplot(data_large3, aes(y = SPR_lag, 
                                x = Chla, 
                                label = ANO)) + 
  geom_point(size = 1.5, alpha = 0.7) +  
  geom_smooth(method = "lm", aes(group = ID), 
              col="black", se = FALSE) +  
  stat_cor(method = "pearson", label.x.npc = "left", col="blue") +
  facet_wrap(~ID, ncol = 5, scales = "free_x") +  
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=6),
        axis.text.y  = element_text(size=6),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_line(colour = "white", size = 0.1),
        legend.position = "none")+ 
  scale_color_viridis_d() + 
  #ylim(0, 0.9) +  
  geom_text(aes(label = ANO), size = 2, vjust = -1, hjust = 0.5) +
  labs(y = "SPR (Lag)", x = "Chl-a")


## todos los plot

ggarrange(spr2,
          spr3,
          spr6,
          spr4,
          spr5,
        ncol = 1)



# Ahora Plot de variables
biomassplot <- ggplot(data_large2%>% 
                        filter(ANO >1999), aes(x = ANO, 
                                   y = biot,
                  fill=ID)) +
  geom_col(position = "dodge") +  # Barras lado a lado por ID
  theme_bw() +
  facet_wrap(~ID, ncol = 5) +  
  labs(title = "",
       x = "",
       y = "Biomass (t)") +
  scale_fill_viridis_d(option="F") +
  scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 4))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=6),
        axis.text.y  = element_text(size=6),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_line(colour = "white", size = 0.1),
        legend.position = "none")
# columna

landings <- ggplot(data_large2%>% 
                     filter(ANO >1999), aes(x = ANO, 
                                    y = CATCH/10000,
                                    fill=ID)) +
  geom_col(position = "dodge") +  # Barras lado a lado por ID
  theme_bw() +
  facet_wrap(~ID, ncol = 5) +  
  labs(title = "",
       x = "",
       y = "Catch (t)")  +
  scale_fill_viridis_d(option="F") +
  scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 4))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=6),
        axis.text.y  = element_text(size=6),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_line(colour = "white", size = 0.1),
        legend.position = "none")


sst <- ggplot(data_large2 %>% 
                filter(ANO >1999), aes(ANO, tsm-270,
                               color=ID))+ 
  geom_line()+
  #geom_point()+
  #geom_smooth(method = "lm", colour='grey', fill='grey', alpha=.3)+
  theme_bw() +
  facet_wrap(.~ID,
             ncol=5)+
  labs(title = "",
       x = "",
       y = "SST")  +
  scale_color_viridis_d(option="F")+
  scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 4))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=6),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_line(colour = "white", size = 0.1),
        legend.position = "none")

chl <- ggplot(data_large2 %>% 
                filter(ANO >1999), aes(ANO, Chla,
                                       color=ID))+ 
  geom_line()+
  #geom_point()+
  #geom_smooth(method = "lm", colour='grey', fill='grey', alpha=.3)+
  theme_bw() +
  facet_wrap(.~ID,
             ncol=5)+
  scale_color_viridis_d(option="F")+
  labs(title = "",
       x = "",
       y = "Chl-a")  +
  scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 4))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=6),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_line(colour = "white", size = 0.1),
        legend.position = "none")

seai <- ggplot(data_large2 %>% 
                filter(ANO >1999), aes(ANO, seaice,
                                       color=ID))+ 
  geom_line()+
  #geom_point()+
  #geom_smooth(method = "lm", colour='grey', fill='grey', alpha=.3)+
  theme_bw() +
  facet_wrap(.~ID,
             ncol=5)+
  labs(title = "",
       x = "",
       y = "Sea Ice Cover")  +
  scale_color_viridis_d(option="F")+
  scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 4))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=6),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_line(colour = "white", size = 0.1),
        legend.position = "none")

### todos los plor

ggarrange(biomassplot,
          landings,
          sst,
          seai,
          chl,
          ncol = 1)

data_corr <- data_large2 %>% 
  dplyr::select(-ID, -ANO) %>%  # Elimina las variables categóricas
  cor(use = "complete.obs")  # Calcula la correlación excluyendo NA

ggcorrplot(data_corr, method = "circle", type = "lower", 
           lab = TRUE, outline.col = "white", ggtheme = theme_minimal())

biot <- ggplot(biomasto %>% 
                 group_by(Year) %>% 
  summarise(biototal =sum(biot)),
  aes(Year, biototal))+
  geom_point(shape=21,
             size=3,)+
  geom_text_repel(aes(label = biototal),
                  size = 3,
                  color="black")+
  geom_smooth(method="lm",
              color="red")+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=9),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.grid.minor = element_line(colour = "white", size = 0.1))+
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 1))+
  labs(y="Biomass Survey (t)",
       x="")
biot


ggplot(data_large2, aes(x = CATCH, y = SPR, color = as.factor(ANO))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~ID, scales = "free", ncol=5) +
  theme_minimal() +
  scale_color_viridis_d() +
  labs(title = "Relación entre CATCH y SPR",
       x = "Captura (CATCH)",
       y = "SPR",
       color = "Año")





ggplot(data_large2, aes(x = as.factor(ANO), y = LENGTH, fill = ID)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ID, scales = "free") +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = "Distribución de LENGTH por ID y Año",
       x = "Año",
       y = "LENGTH")

ggplot(data_large2, aes(x = tsm, y = SPR, color = ID)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ID, scales = "free") +
  theme_minimal() +
  labs(title = "Relación entre TSM y Biomasa",
       x = "Temperatura Superficial del Mar (tsm)",
       y = "Biomasa (biot)")



ggplot(data_large2, aes(x = biot, y = SPR)) +
  geom_point(size = 3, alpha = 0.7, aes(color = as.factor(ANO))) +  
  geom_smooth(method = "lm", se = TRUE, color = "black") +  
  stat_cor(method = "pearson", label.x.npc = "left") +  # Añade R en cada facet
  facet_wrap(~ID, scales = "free", ncol=5) +  
  theme_minimal() +  
  scale_color_viridis_d(option="F") +  
  labs(title = "Relación entre CATCH y SPR con R en cada ID",
       x = "CATCH",
       y = "SPR",
       color = "Año")
# Correlaciones por año
cor_by_year <- data_large %>%
  group_by(ANO) %>%
  summarise(
    corr_spr_chla = cor(SPR, Chla, use = "complete.obs"),
    corr_spr_tsm = cor(SPR, tsm, use = "complete.obs"),
    corr_spr_seaice = cor(SPR, seaice, use = "complete.obs")
  )

print(cor_by_year)

# Relación entre SPR y Chla por ID
ggplot(data_large, aes(x = SPR, y = Chla, color = ID)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = ID), se = FALSE) +
  facet_wrap(~ID) +
  theme_minimal() +
  labs(title = "Relación entre SPR y Chla por ID")

