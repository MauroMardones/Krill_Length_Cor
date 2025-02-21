# **Krill Length Analysis**  

## **Overview**  
This repository contains an analysis of the spatial and environmental variability in krill (*Euphausia superba*) length. The study evaluates different mixed-effects models incorporating spatial strata (ID) as a fixed effect and year (ANO) as a random effect. Environmental covariates such as chlorophyll-a (Chla), sea surface temperature (TSM), and sea ice cover are included to assess their influence on krill growth dynamics.  

## **Analysis Goals**  

- Investigate spatial variability in krill length (*LENGTH*) and the 75th percentile length (*LENGTH_P75*).  
- Test different statistical models to determine how environmental factors influence krill growth across different strata.  
- Identify the best-fitting model to support krill growth scenario testing within the LBSPR framework.  

## **Results**  

The full results, including model comparisons, visualizations, and conclusions, are available in the HTML report:  

[View the Results](https://mauromardones.github.io/Krill_Length_Cor/)

## **Reproducibility**  

This analysis was conducted in **R**, using the `lme4` package for mixed-effects modeling and `performance` for model comparison. The R Markdown file (`index.Rmd`) compiles the entire workflow and generates the HTML report.  

## **Repository Structure**  
```
├── data/               # Raw and processed data files  
├── scripts/            # R scripts for data processing and modeling  
├── index_files/        # figures and Outputs from the analysis  
├── index.Rmd           # R Markdown file for the report  
├── README.md           # Project description and instructions  
```

## **How to Cite**  
If you use or reference this analysis, please cite appropriately:  
Mardones, M. (2025). *Krill Length Analysis: Environmental and Spatial Influences*.  

