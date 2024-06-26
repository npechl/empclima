# Microbial co-occurrence network reveals climate and geographic patterns for soil diversity on the planet

This repository contains all software scripts (`.R`) and analysis files produced within the project.

## Data retrieval
The Earth Microbiome Project has produced a variety of microbiome datasets that equally reflect various settings (EMPO level 3) and investigations conducted. These datasets are based on sequence data from the EMP database that have been corrected for errors and trimmed using Deblur in Qiime1. The complete workflow for soil collection, metadata curation, DNA extraction, sequencing, and sequence preparation, as well as the various subsets, can be retrieved from the EMP website (or FTP site). The complete quality filtered 90-bp BIOM table of release1 samples was used for the purposes of this study. Additionally, only samples that fit the EMPO3 ontology "Soil (non-saline)" were selected for analysis due to their simplicity. These samples were classified and labelled with climatic zones in line with the Köppen-Geiger climate classification system, based on the geographic coordinates of each location.

## Software description
Within the `R` folder, you will find a comprehensive collection of scripts utilized for the data analysis. Each script is sequentially numbered, designed to facilitate smooth execution in the appropriate order.

### Visualization
The R folder contains all the scripts necessary for generating the visualizations in this project (`[visualization]_*.R`).

## Analysis folders
We used the complete quality filtered 90-bp BIOM table of release1 coming from the Earth Microbiome Project database. To prevent the introduction of noise in the data and avoid false positive observations, ESVs with a relative abundance of less than 0.01% and present in fewer than 30% of samples were removed (`emp-soil-analysis-clean-release1`). Additionally, a more lenient filtering threshold of 0.001% was employed to assess its impact on the analysis (`emp-soil-analysis-clean-release1-v2`). Since Archaea were underrepresented, only Bacteria were included in the analysis. Finally, to ensure sufficient sample representation for each climatic zone, a filter was applied to retain only climate zones with at least three samples.

Each folder contains:
- Sample metadata (`sample-metadata.Soil (non-saline).txt`) and ESV taxonomy (`taxonomy-table.Soil (non-saline).txt`) information
- Diversity analysis results - alpha diversity (`diversity1.csv`) and beta diversity (`diversity2.csv`)
- Microbial co-occurrence network (`*.graphml`, `SpiecEasi-elapsedTime (min).txt`)
- Climate related subnetworks global properties (`globalProperties.csv`)
- Climate related hub nodes (`hubs.csv`)
- Identified microbial links (`links1.csv`, `links2.csv`) and significant links (`hypergeometric1.csv`)
- Climate related subnetworks centralities (`centralities1.csv`, `centralities2.csv`)

## Getting help, contributing
If you require assistance with executing the pipeline or have any questions or suggestions regarding the analysis workflow, please don't hesitate to open an issue. 
We welcome your feedback and are ready to provide any support needed. Feel free to reach out!
