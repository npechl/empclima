# Microbial co-occurrence network reveals climate and geographic patterns for soil diversity on the planet

This repository contains all software scripts (`.R`) and analysis files produced within the project.

## Data retrieval
The Earth Microbiome Project - EMP - has produced a variety of microbiome subsets (subset of 2k samples, 5k samples, and 10k samples) that equally reflect various settings (EMPO level 3) and investigations conducted. These subsets are based on sequence data from the EMP database that have been corrected for errors and trimmed using Deblur in Qiime2. The complete workflow for soil collection, metadata curation, DNA extraction, sequencing, and sequence preparation, as well as the various subsets, can be retrieved from the EMP website (or FTP site). The 90-bp BIOM table of 5k samples was used for the purposes of this study. Additionally, only samples that fit the EMPO3 ontology "Soil (non-saline)" were selected for analysis due to their simplicity. These samples were classified and labelled with climatic zones in line with the Köppen-Geiger climate classification system, based on the geographic coordinates of each location.

## Software description
Within the `R` folder, you will find a comprehensive collection of scripts utilized for the data analysis. Each script is sequentially numbered, designed to facilitate smooth execution in the appropriate order.

## Getting help, contributing
If you require assistance with executing the pipeline or have any questions or suggestions regarding the analysis workflow, please don't hesitate to open an issue. 
We welcome your feedback and are ready to provide any support needed. Feel free to reach out!
