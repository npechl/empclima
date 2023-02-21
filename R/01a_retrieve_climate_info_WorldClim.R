



rm(list = ls())
gc()

# load libraries and inputs ------------

library(data.table)
library(stringr)

library(geodata)

sample_metadata <- "data/sample-metadata/emp_qiime_mapping_subset_5k.tsv"
geo_path        <- "WorldClim/"
resolution      <- 2.5
workdir         <- "emp-soil-analysis-clean-sub5k"


# create working folder ------------------------


if(dir.exists(workdir)) {
    
    unlink(workdir, recursive = TRUE)
    dir.create(workdir, showWarnings = FALSE)
    
} else {
    
    dir.create(workdir, showWarnings = FALSE)
    
}

# import sample metadata ------------------------

sam <- fread(sample_metadata)

# download Worldclim data -----------------

options(timeout = 50000)
getOption('timeout')

bioclim <- worldclim_global(var = 'bio', path = geo_path, res = resolution)
bioclim <- extract(
    x  = bioclim, 
    y  = sam[, c("longitude_deg", "latitude_deg"), with = FALSE], 
    xy = TRUE
)


bioclim <- setDT(bioclim)
bioclim <- bioclim[, 2:(ncol(bioclim) - 2)]

colnames(bioclim) <- paste0("BIO", 1:19)

sam <- cbind(sam, bioclim)

fwrite(
    sam, paste0(workdir, "/sample-metadata.tsv"),
    row.names = FALSE, quote = FALSE, sep = "\t"
)


