---
title: "Supporting Information 2"
subtitle: "Spatial and temporal variability in the intrinsic productivity of Antarctic krill (Euphausia superba) along the Western Antarctic Peninsula under environmental and life history scenarios"
author: "Mardones, M; Jarvis Mason, E.T.;  Santa Cruz, F.; Watters, G.; Cárdenas, C.A"
date:  "21 February, 2025"
bibliography: seaice.bib
csl: apa.csl
link-citations: yes
linkcolor: blue
output:
  bookdown::html_document2:
    fig_caption: yes
    keep_md: true
    toc: true
    toc_deep: 3
    toc_float:
      collapsed: false
      smooth_scroll: false
    theme: cosmo
    fontsize: 0.9em
    linestretch: 1.7
    html-math-method: katex
    self-contained: true
    code-tools: true
    number_sections: false
editor_options: 
  markdown: 
    wrap: 72
---



``` r
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
```




``` r
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
library(ggcorrplot)
```

# Background

The following document intends to carry out a complementary
methodological analysis to correlate environmental variables with the
population dynamics of krill (*Euphausia superba*), in this case, in spatial complexity and with a biological component like lengths from fishery monitoring.

# Hypothesis

The primary inquiry pertains to the environmental and  effects of distinct
physical and oceanographic factors in the Southern Ocean on the krill
population. Our aim is to examine the population structure via krill
length component from fishery and determine whether any changes were driven by
environmental factors across any of its dimensions.

# Objective

The main idea of this analysis is to empirically verify the influence on the population dynamics of krill measured through length variability, which gives indications of growth variations on the temporal and spatial scale.
Also, once the correlation and effects on the population and/or fishing
indicators on krill have been verified, this analysis aims to have a
time series of the environmental variable to incorporate into the stock
assessment process. Similar work in @Wang2021 but with a longest fishery
history.


# Methodology

## Spatial heterogeneity 

Figure \@ref(fig:Figure1) (S2 Fig 1 now) illustrates the spatial heterogeneity of key environmental and population variables across different management units (BS, EI, GS, JOIN, SSWI) in the Antarctic Peninsula, where krill populations are distributed. Biomass and catch trends show substantial variability between strata, with BS and GS exhibiting the highest biomass estimates, whereas JOIN and SSWI display comparatively lower values. Catch levels also differ significantly, with BS and GS experiencing the most intensive exploitation, while SSWI and JOIN have minimal catch records.  Environmental variables further highlight this heterogeneity. Sea surface temperature (SST) trends vary among strata, with GS and JOIN exhibiting a slight warming trend over time, while BS and EI remain relatively stable. Sea ice cover differs substantially, with GS showing consistently high coverage, whereas JOIN presents greater fluctuations. Chlorophyll-a (Chl-a) levels, a proxy for primary productivity, also vary across regions, with BS and GS showing declining trends, while EI and SSWI remain relatively stable at lower concentrations.  Given these spatial differences in both krill population metrics and environmental conditions, it is essential to analyze and estimate SPR at this local scale. The observed heterogeneity supports the need for spatially explicit management, as krill population dynamics are likely influenced by regional environmental drivers. By incorporating SPR analysis at this resolution, we can provide a spatial explicit framework for sustainable krill management, ensuring that conservation efforts align with local population and ecosystem characteristics.

<div class="figure" style="text-align: center">
<img src="index_files/figure-html/var_env.png" alt="Spatial heterogeneity of krill biomass, catch, and environmental variables across management units in the Antarctic Peninsula (BS, EI, GS, JOIN, SSWI)" width="90%" />
<p class="caption">(\#fig:Figure1)Spatial heterogeneity of krill biomass, catch, and environmental variables across management units in the Antarctic Peninsula (BS, EI, GS, JOIN, SSWI)</p>
</div>

## Length composition data as indicator of krill dynamics


``` r
#Load data procesed
data_large2 <- read_csv("data/datapost_LBSPR.csv")
```


``` r
glimpse(data_large2)
```

```
## Rows: 215
## Columns: 13
## $ ID         <chr> "BS", "BS", "BS", "BS", "BS", "BS", "BS", "BS", "BS", "BS",…
## $ ANO        <dbl> 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1989,…
## $ seaice     <dbl> 152.1639, 155.5738, 152.4918, 152.4918, 165.3443, 143.3770,…
## $ tsm        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 272.2446, 272.2…
## $ Chla       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ MAT        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ LENGTH     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ LENGTH_P75 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ CATCH      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ SPR        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ `SE SPR`   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ biot       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ cvto       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
```

Figure \@ref(fig:Figure2) consists of two side-by-side scatterplots, each displaying trends in krill length over time (years) for different spatial strata (ID).


``` r
data_filtered <- data_large2 %>%
  filter(!is.na(LENGTH), 
         !is.na(LENGTH_P75))

length <- ggplot(data_filtered, 
                 aes(x = ANO, y = LENGTH, color = ID)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "",
       x = "",
       y = "Mean Krill Length (cm)") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_color_viridis_d(option="F")+
  ylim(2,6)

length75 <- ggplot(data_filtered, 
                   aes(x = ANO, y = LENGTH_P75, color = ID)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "",
       x = "Year",
       y = "75th Percentile Krill Length") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_color_viridis_d(option="F")+
  ylim(2,6)
```



``` r
ggarrange(length,
          length75,
          ncol=2)
```

<div class="figure" style="text-align: center">
<img src="index_files/figure-html/Figure2-1.jpeg" alt="Lenght mean and 75th percentile between year and strata." width="100%" />
<p class="caption">(\#fig:Figure2)Lenght mean and 75th percentile between year and strata.</p>
</div>


There is spatial variation in krill length trends across different strata, with some areas showing an increase in krill length over time, while others remain stable or decrease. The differences between the mean and the 75th percentile length suggest potential changes in krill population structure across strata. This visualization helps assess how krill size dynamics vary over time and across different spatial regions.



## Test Correlation

The Pearson test are statistical methods used to assess the correlation between two variables. The Pearson test evaluates the linear correlation between two continuous variables. In the case of the Pearson test, the degree of association between two continuous variables is measured through a correlation coefficient that
varies between -1 and 1. A value of 1 indicates a perfectly positive
correlation, a value of -1 indicates a perfectly negative correlation,
and a value of 0 indicates no correlation between the two variables [@McCulloch2001].

-   Pearson Product-Moment Coefficient

This is the most widely used correlation analysis formula, which
measures the strength of the 'linear' relationships between the raw data
from both variables, rather than their ranks. This is an dimensionless
coefficient, meaning that there are no data-related boundaries to be
considered when conducting analyses with this formula, which is a reason
why this coefficient is the first formula researchers try.

$\begin{aligned} r = 1- \frac{6\sum_{i=1}^n D_{i}^n}{n (n^2 - 1)}\end{aligned}$



``` r
data_corr <- data_large2 %>% 
  dplyr::select(-ID, -ANO, - SPR, -`SE SPR`) %>%  # Elimina las variables categóricas
  cor(method = "pearson", 
                 use = "complete.obs")  # Calcula la correlación excluyendo NA
```



``` r
ggcorrplot(data_corr, method = "circle", 
           type = "lower", 
           colors = c("green", "white", "yellow"),
           lab = TRUE, outline.col = "white", 
           ggtheme = theme_minimal())
```

<div class="figure" style="text-align: center">
<img src="index_files/figure-html/Figure3-1.jpeg" alt="Coorrelation plot to different variables." width="80%" />
<p class="caption">(\#fig:Figure3)Coorrelation plot to different variables.</p>
</div>


The correlation matrix provides valuable insights into the relationships among the variables (Figure \@ref(fig:Figure3)):  

**Sea ice (seaice)** is **negatively correlated with LENGTH (-0.339)** and **LENGTH_P75 (-0.329)**, suggesting that higher sea ice levels might be associated with smaller organism sizes.  **TSM** has a **moderate positive correlation with LENGTH (0.330) and LENGTH_P75 (0.298)**, indicating that turbidity may be linked to larger body sizes. **Chla** is **negatively correlated with LENGTH (-0.421) and LENGTH_P75 (-0.393)**, implying that areas with higher chlorophyll-a concentrations may have smaller individuals.   Based on the correlation matrix provided, the relationship between **Chla and TSM** is strong and negative (**r = -0.82**). This indicates that as **TSM increases, Chla tends to decrease**, suggesting a potential inverse relationship between these two environmental variables. Given this strong correlation, it will be necessary to account for this relationship in the GLMM models, either by:  

1. Incorporating both variables separately, treating them as independent predictors.  
2. Modeling their correlation explicitly, such as including an interaction term (**TSM and Chla**) to assess their combined effect.  
3. Testing alternative models to evaluate whether including both variables together introduces multicollinearity issues.  

By considering these approaches, we aim to capture the effects of environmental conditions on LENGTH and LENGTH_P75 while ensuring statistical robustness. These insights will help refine the **GLMM** models to better capture the environmental effects on population structure while maintaining statistical reliability. 


## Variable Distribution

To explore the distribution of numerical variables in our dataset, we conducted a histogram analysis (Figure \@ref(fig:Figure4). First, we excluded non-numeric variables (`ID`, `ANO`, `SE SPR`, and `cvto`) to focus on the remaining quantitative data. The results from this exploratory analysis provided essential insights into the characteristics of our dataset, helping to guide subsequent modeling and statistical analyses.


``` r
data_filtered2 <- data_large2 %>%
  dplyr::select(-ID, -ANO, -`SE SPR`, -cvto) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "Valor")

ggplot(data_filtered2, aes(x = Valor)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Variable distribution",
       x = "",
       y = "Frecuency")
```

<div class="figure" style="text-align: center">
<img src="index_files/figure-html/Figure4-1.jpeg" alt="Distribution of numerical variables to test assumtion of regresion models" width="80%" />
<p class="caption">(\#fig:Figure4)Distribution of numerical variables to test assumtion of regresion models</p>
</div>
To ensure independence from the assumption of normality in variable behavior, we will use **Generalized Linear Mixed Models (GLMMs)**. This approach is appropriate because GLMMs allow for **non-normal distributions** of response variables and can account for **hierarchical or grouped data structures** by incorporating random effects. In our case, GLMMs help model variations in `LENGTH` and `LENGTH_P75` while considering **both fixed effects (categorical factors like `ID`) and random effects (environmental covariates and `ANO`)**. This methodology provides **greater flexibility** compared to standard linear models, making it suitable for ecological and fisheries research where data often exhibit heteroscedasticity and non-normal distributions.

## Models

To evaluate the spatial and temporal variability of krill length from fishery and its relationship with environmental covariates, we applied a series of regression models, including generalized linear models (GLMs) and general linear mixed-effects models (GLMMs). Initial GLMs were constructed to assess the fixed effects of year (ANO), spatial strata (ID), sea ice concentration, chlorophyll-a (Chla), and sea surface temperature (TSM) on krill length. To account for the hierarchical structure of the data, we incorporated random intercepts for year (ANO) and, in some models, for spatial strata (ID) using the `lme4` package in R. Interactions between ID and ANO were tested to explore whether temporal trends in krill length differed across spatial units. Model selection was based on the Akaike Information Criterion (AIC) and residual deviance, with lower values indicating better model fit. Variance inflation factors (VIF) were calculated to check for multicollinearity among environmental predictors, and when necessary, covariates were standardized or imputed to handle missing data. The inclusion of random effects allowed us to capture unexplained annual variability while accounting for spatial heterogeneity in krill growth patterns. Visualization of model predictions and marginal effects was conducted using the `ggeffects` and `sjPlot` packages [@Ludecke2024], providing insights into how krill length dynamics respond to environmental and spatial drivers over time.  

### GLMM models

Random effects are a way to model variability in data that comes from
factors that cannot be directly measured or controlled. In the context
of statistical models, random effects refer to variables that are
assumed to have an unknown probability distribution, and are included in
the model to explain some of the variation in the data. Random effects are often modeled by using mixed effects models, which combine random and fixed effects in the same model. Fixed effects are
those that are assumed to be constant for all study units and are
directly measured, while random effects are those that are assumed to
vary randomly across study units and cannot be directly measured.

In short, random effects are a way of modeling variability in data that
cannot be directly explained by the variables measured in the study, and
are included in the model to improve the precision of the estimates and
reduce the potential for bias [@McCulloch2001; @Bates2015].

In our models, **ID represents the stratum** within the study area, and it is treated as a fixed effect. This approach assumes that each stratum has a specific and constant influence on the response variable (*LENGTH* or *LENGTH_P75*), allowing for direct estimation and comparison of its effects. By treating ID as a fixed effect, we acknowledge that each stratum possesses unique environmental and oceanographic characteristics that may impact the response variable differently. This approach contrasts with a random effects model, which would assume that the observed strata are a random sample from a larger population, estimating only the variance between them rather than their specific effects. Given that the number of strata is finite and known, and our interest lies in explicitly assessing differences among them rather than making inferences beyond the observed data, treating ID as a fixed effect is the most appropriate choice.

In our models, **ANO represents the temporal component** and is treated as a random effect. This approach assumes that annual variations influence the response variable (*LENGTH* or *LENGTH_P75*), but rather than estimating specific effects for each year, we model their variance as a way to account for unobserved temporal fluctuations. By treating ANO as a random effect, we acknowledge that interannual variability may be influenced by unmeasured environmental or ecological factors, reducing the risk of overfitting and allowing for more generalizable inferences. This also improves model parsimony by capturing temporal correlations without requiring individual year-specific coefficients. Since our interest lies in understanding general temporal patterns rather than estimating the fixed influence of each year, treating ANO as a random effect is the most appropriate methodological choice.

\[
\text{Mod 1:} \quad LENGTH_{i} = \beta_0 + \beta_1 ID_{i} + (1 | ANO_i) + \epsilon_i
\]

\[
\text{Mod 2:} \quad LENGTH_{i} = \beta_0 + \beta_1 ID_{i} + \beta_2 Chla_{i} + (1 | ANO_i) + \epsilon_i
\]

\[
\text{Mod 3:} \quad LENGTH_{i} = \beta_0 + \beta_1 ID_{i} + \beta_2 Chla_{i} + \beta_3 tsm_{i} + (1 | ANO_i) + \epsilon_i
\]

\[
\text{Mod 4:} \quad LENGTH_{i} = \beta_0 + \beta_1 ID_{i} + \beta_2 Chla_{i} + \beta_3 tsm_{i} + \beta_4 seaice_{i} + (1 | ANO_i) + \epsilon_i
\]

\[
\text{Mod 5:} \quad LENGTH_{i} = \beta_0 + \beta_1 ID_{i} + \beta_2 seaice_{i} + \beta_3 tsm_{i} + \beta_4 Chla_{i} + \beta_5 (tsm \times Chla)_{i} + (1 | ANO_i) + \epsilon_i
\]

\[
\text{Mod 5\_P75:} \quad LENGTH\_P75_{i} = \beta_0 + \beta_1 ID_{i} + \beta_2 seaice_{i} + \beta_3 tsm_{i} + \beta_4 Chla_{i} + \beta_5 (tsm \times Chla)_{i} + (1 | ANO_i) + \epsilon_i
\]

This represents each model in mathematical terms, where \(\beta\) are the coefficients, \( (1 | ANO_i) \) represents the random effect of ANO, and \(\epsilon_i\) is the error term.



``` r
data_large3 <- data_large2 %>% 
  drop_na(LENGTH, LENGTH_P75) %>% 
  filter(ANO > 1999) %>%
  mutate(tsm = scale(tsm),
         Chla = scale(Chla))
# Modelo 1: Solo ID como efecto fijo
mod1_L <- lmer(LENGTH ~ ID + (1 | ANO), 
               data = data_large3, 
               REML = FALSE)
# Modelo 2: Agregando Sea Ice
mod2_L <- lmer(LENGTH ~ ID + Chla + (1 | ANO), 
               data = data_large3, 
               REML = FALSE)
# Modelo 3: Sumando TSM
mod3_L <- lmer(LENGTH ~ ID + Chla+ tsm + (1 | ANO), 
               data = data_large3, REML = FALSE)
# Modelo 4: Sumando Chla
mod4_L <- lmer(LENGTH ~ ID + Chla + tsm + seaice + (1 | ANO), 
               data = data_large3, REML = FALSE)
# Modelo 5: Interacción TSM y Chla
mod5_L <- lmer(LENGTH ~ ID + seaice + tsm * Chla + (1 | ANO), 
               data = data_large3, REML = FALSE)
# Modelo 5_P75: Interacción TSM y Chla
mod5_P75 <- lmer(LENGTH_P75 ~ ID + seaice + tsm * Chla + (1 | ANO),
                 data = data_large3, REML = FALSE)
```

## Results




``` r
model_comparison <- compare_performance(mod1_L, 
                                        mod2_L, 
                                        mod3_L,
                                        mod4_L, 
                                        mod5_L, 
                                        mod5_P75,
                                        rank = TRUE,
                                        verbose = FALSE)

model_comparison %>%
  kable(format = "html", 
        digits = 3, 
        align = "c", 
        caption = "Model Performance Comparison") %>%
  kable_styling(full_width = TRUE, 
                bootstrap_options = c("striped", 
                                      "hover", 
                                      "condensed",
                                      "responsive")) %>%
  scroll_box(width = "100%", height = "400px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:400px; overflow-x: scroll; width:100%; "><table class="table table-striped table-hover table-condensed table-responsive" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-6)Model Performance Comparison</caption>
 <thead>
  <tr>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> Name </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> Model </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> R2_conditional </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> R2_marginal </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> ICC </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> RMSE </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> Sigma </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> AIC_wt </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> AICc_wt </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> BIC_wt </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> Performance_Score </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> mod5_L </td>
   <td style="text-align:center;"> lmerMod </td>
   <td style="text-align:center;"> 0.815 </td>
   <td style="text-align:center;"> 0.409 </td>
   <td style="text-align:center;"> 0.687 </td>
   <td style="text-align:center;"> 0.180 </td>
   <td style="text-align:center;"> 0.208 </td>
   <td style="text-align:center;"> 0.246 </td>
   <td style="text-align:center;"> 0.080 </td>
   <td style="text-align:center;"> 0.008 </td>
   <td style="text-align:center;"> 0.721 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mod1_L </td>
   <td style="text-align:center;"> lmerMod </td>
   <td style="text-align:center;"> 0.770 </td>
   <td style="text-align:center;"> 0.380 </td>
   <td style="text-align:center;"> 0.629 </td>
   <td style="text-align:center;"> 0.198 </td>
   <td style="text-align:center;"> 0.227 </td>
   <td style="text-align:center;"> 0.358 </td>
   <td style="text-align:center;"> 0.578 </td>
   <td style="text-align:center;"> 0.825 </td>
   <td style="text-align:center;"> 0.629 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mod4_L </td>
   <td style="text-align:center;"> lmerMod </td>
   <td style="text-align:center;"> 0.809 </td>
   <td style="text-align:center;"> 0.393 </td>
   <td style="text-align:center;"> 0.686 </td>
   <td style="text-align:center;"> 0.185 </td>
   <td style="text-align:center;"> 0.214 </td>
   <td style="text-align:center;"> 0.118 </td>
   <td style="text-align:center;"> 0.062 </td>
   <td style="text-align:center;"> 0.011 </td>
   <td style="text-align:center;"> 0.559 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mod3_L </td>
   <td style="text-align:center;"> lmerMod </td>
   <td style="text-align:center;"> 0.811 </td>
   <td style="text-align:center;"> 0.376 </td>
   <td style="text-align:center;"> 0.697 </td>
   <td style="text-align:center;"> 0.186 </td>
   <td style="text-align:center;"> 0.216 </td>
   <td style="text-align:center;"> 0.121 </td>
   <td style="text-align:center;"> 0.098 </td>
   <td style="text-align:center;"> 0.033 </td>
   <td style="text-align:center;"> 0.507 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mod2_L </td>
   <td style="text-align:center;"> lmerMod </td>
   <td style="text-align:center;"> 0.773 </td>
   <td style="text-align:center;"> 0.383 </td>
   <td style="text-align:center;"> 0.633 </td>
   <td style="text-align:center;"> 0.197 </td>
   <td style="text-align:center;"> 0.226 </td>
   <td style="text-align:center;"> 0.154 </td>
   <td style="text-align:center;"> 0.181 </td>
   <td style="text-align:center;"> 0.123 </td>
   <td style="text-align:center;"> 0.390 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mod5_P75 </td>
   <td style="text-align:center;"> lmerMod </td>
   <td style="text-align:center;"> 0.722 </td>
   <td style="text-align:center;"> 0.390 </td>
   <td style="text-align:center;"> 0.544 </td>
   <td style="text-align:center;"> 0.212 </td>
   <td style="text-align:center;"> 0.241 </td>
   <td style="text-align:center;"> 0.002 </td>
   <td style="text-align:center;"> 0.001 </td>
   <td style="text-align:center;"> 0.000 </td>
   <td style="text-align:center;"> 0.052 </td>
  </tr>
</tbody>
</table></div>
The Figure \@ref(fig:Figure5) show graphically the performance in the models.


``` r
plot(compare_performance(mod1_L, 
                    mod2_L, 
                    mod3_L,
                    mod4_L, 
                    mod5_L, 
                    mod5_P75,
                    rank=TRUE,
                    verbose = FALSE))
```

<div class="figure" style="text-align: center">
<img src="index_files/figure-html/Figure5-1.jpeg" alt="Comparision the performance and quality of several models" width="80%" />
<p class="caption">(\#fig:Figure5)Comparision the performance and quality of several models</p>
</div>

In our *best model* `mod5_L` we fitted a linear mixed model (estimated using ML and nloptwrap optimizer) to
predict `LENGTH` with `ID`, `seaice`, `tsm` and `Chla` (formula: LENGTH ~ ID + seaice + tsm * Chla). The model included ANO as random effect (formula: ~1 | ANO). The model's total explanatory power is substantial (conditional R2 = 0.82) and the part related to the fixed effects alone (marginal R2) is of 0.41. The model's intercept, corresponding to ID = BS, seaice = 0, tsm = 0 and Chla = 0, is at 5.02
(95% CI [3.65, 6.39], t(51) = 7.36, p < .001). about colinearity, the effect of tsm × Chla is statistically non-significant and negative (beta = -0.11, 95% CI [-0.23, 7.00e-03], t(51) = -1.89, p = 0.064; Std. beta = -0.25, 95% CI [-0.52, 0.02])


Figure \@ref(fig:Figure6) presents two panels, each illustrating different aspects of a mixed-effects model analyzing krill *LENGTH*. The left panel displays the random effects associated with interannual variability, where the x-axis represents the magnitude of the random effects, with a vertical reference line at zero indicating no effect, and the y-axis corresponds to different years from 2001 to 2020. Each point represents the estimated random effect for a given year, with horizontal error bars indicating confidence intervals. Blue points denote positive effects, while red points signify negative ones. Notably, some years, such as 2008, 2012, and 2004, show more pronounced negative effects, whereas other years exhibit relatively small variations around zero.  

The right panel visualizes the estimated fixed effects of different spatial strata (ID [SSWI], ID [EI], ID [JOIN], ID [GS]) and chlorophyll-a concentration (*Chla*) on krill length. The x-axis represents the estimated effect size, with a vertical line at zero serving as a reference, while the y-axis lists the fixed effects included in the model. Each point represents the estimated coefficient for a given variable, with horizontal error bars showing confidence intervals. The results indicate that all fixed effects have a positive influence on krill length, with *Chla* and certain spatial strata showing stronger effects.  

Overall, the plot suggests that krill length is influenced by both interannual variability and spatial-environmental factors. The random effects panel highlights fluctuations in krill size over time, while the fixed effects panel confirms that spatial structure and chlorophyll-a concentration play significant roles in shaping krill growth patterns. This analysis provides valuable insights into how environmental and regional factors contribute to krill population dynamics.


``` r
pre <- plot_model(mod5_L, type = "re",  
                  facet.grid = FALSE,  
                  free.scale = FALSE,  
                  title = NULL,  
                  vline.color = "darkgrey",  
                  sort.est = TRUE,  
                  colors = "Set1",  
                  show.data = TRUE,  
                  jitter = 0.2) +  
       theme_few()

pest <- plot_model(mod2_L, type = "est",  
                   facet.grid = FALSE,  
                   free.scale = FALSE,  
                   title = NULL,  
                   vline.color = "darkgrey",  
                   sort.est = TRUE,  
                   colors = "Set2",  
                   show.data = TRUE,  
                   jitter = 0.2) +  
        theme_few()

ggarrange(pre,
          pest,
          ncol=2)
```

<div class="figure" style="text-align: center">
<img src="index_files/figure-html/Figure6-1.jpeg" alt="Comparision the performance and quality of several models" width="80%" />
<p class="caption">(\#fig:Figure6)Comparision the performance and quality of several models</p>
</div>

# Conclusion


Our analysis aimed to explore the spatial variability in krill *LENGTH* and the 75th percentile of length (*LENGTH_P75*) while incorporating key environmental covariates, such as chlorophyll-a concentration (Chla), sea surface temperature (TSM), and sea ice cover. Using mixed-effects models, we evaluated the influence of these environmental factors on krill growth across different spatial strata (ID) while accounting for temporal variability by including year (ANO) as a random effect. The inclusion of interactions between TSM and Chla allowed us to capture potential synergistic or antagonistic effects of these variables, providing insights into how environmental variability drives size distribution across regions.  

Among the tested models, **Model 5**—which includes spatial strata (ID), sea ice, and the interaction between TSM and Chla—performed best based on AIC selection criteria. This model suggests that krill growth is influenced not only by individual environmental factors but also by their interactions, which vary across spatial strata. These findings provide a solid foundation for testing different growth scenarios within the LBSPR model, allowing for a more nuanced understanding of how environmental heterogeneity shapes krill population dynamics.  

On one hand, the mixed-effects models confirm the influence of spatial structure, where each stratum represents a distinct environmental context affecting both the dependent variable (krill sizes) and independent variables (environmental factors). This highlights the importance of considering spatial heterogeneity in growth studies.  

Additionally, our results corroborate the influence of environmental variables on krill size variability. Among them, chlorophyll-a emerged as the most significant factor, with a negative effect on krill size. This suggests that higher chlorophyll concentrations lead to increased recruitment, as the greater availability of phytoplankton provides a more abundant substrate for the krill population, thereby shifting the size structure toward smaller individuals.  

Overall, these findings demonstrate that krill population structure is shaped not only by fishing pressure but also by environmental conditions. By incorporating these environmental components into stock assessment models, we enhance our understanding of krill population dynamics in the Antarctic Peninsula, particularly in Subarea 48.1, and improve the ecological realism of predictive models such as LBSPR.

# References


