
rm(list = ls())
gc()

# load libraries and inputs ------------

library(data.table)
library(stringr)

sample_metadata <- "emp-soil-analysis-clean-sub10k/sample-metadata.Soil (non-saline).txt"
abundance_table <- "emp-soil-analysis-clean-sub10k/abundance-table.Soil (non-saline).txt"
workdir         <- dirname(sample_metadata)


# import BIOM file ------------------------

obs0 <- fread(abundance_table)
sam0 <- fread(sample_metadata)

    
    sam0 = sam0[which(ClimateZone != "" & !is.na(ClimateZone)), ]
    
    
    index = colSums(obs0[, sam0$`#SampleID`, with = FALSE])

    drop = names(which(index == 0))
    keep = names(which(index != 0))
    
    sam0 = sam0[which(sam0$`#SampleID` %in% keep), ]
    
    
    # Filter 2; at least 3 samples per climate zones ----------------
    
    sam0[, by = ClimateZone, N_climatezone := .N]
    
    sam0 = sam0[which(N_climatezone >= 3), ]
    
    sam0$N_climatezone = NULL
    
    # creating mapping files ----------------------

    sam_mapping <- data.table(
        "SampleIDabv" = paste0("S", 1:nrow(sam0)),
        "SampleID"    = sam0$`#SampleID`
    )


    sam0 = merge(sam_mapping, sam0, by.x = "SampleID", by.y = "#SampleID")
    sam0 = sam0[str_order(sam0$SampleIDabv, numeric = TRUE), ]
    
    obs0 = obs0[, c("TaxaIDabv", sam0$SampleID), with = FALSE]
    
    colnames(obs0) = c("TaxaIDabv", sam0$SampleIDabv)


fwrite(
    sam0, sample_metadata,
    row.names = FALSE, quote = FALSE, sep = "\t"
)

fwrite(
    obs0, abundance_table,
    row.names = FALSE, quote = FALSE, sep = "\t"
)
