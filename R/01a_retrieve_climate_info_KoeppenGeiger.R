


rm(list = ls())
gc()

# load libraries and inputs ------------

library(data.table)
library(stringr)

library(kgc)

sample_metadata     <- "data-raw-test/emp_qiime_mapping_subset_2k.tsv"
koeppen_geiger_info <- "climate-classification-info.csv"
workdir             <- "test"

if(dir.exists(workdir)) {
    
    unlink(workdir, recursive = TRUE)
    dir.create(workdir, showWarnings = FALSE)
    
} else {
    
    dir.create(workdir, showWarnings = FALSE)
    
}


climate_info <- fread(koeppen_geiger_info)

climate_info$`Level of Heat` <- ifelse(
    climate_info$`Level of Heat` == "",
    NA, climate_info$`Level of Heat`
)

# import sample metadata ------------------------

sam <- fread(sample_metadata)


data <- data.frame(
    "Longitude" = sam$longitude_deg,
    "Latitude"  = sam$latitude_deg
)

data <- data.frame(
    data,
    rndCoord.lon = RoundCoordinates(data$Longitude),
    rndCoord.lat = RoundCoordinates(data$Latitude)
)

data <- data.frame(data, ClimateZ = LookupCZ(data))


any(sam$longitude_deg != data$Longitude, na.rm = TRUE)
any(sam$latitude_deg != data$Latitude, na.rm = TRUE)

sam$ClimateZone <- data$ClimateZ

sam[which(sam$ClimateZone == "Climate Zone info missing"), ]$ClimateZone <- NA
sam[which(sam$ClimateZone == ""), ]$ClimateZone <- NA


sam <- merge(
    sam, climate_info, 
    by.x = "ClimateZone", by.y = "Code", all.x = TRUE
)

fwrite(
    sam, paste0(workdir, "/", basename(sample_metadata)),
    row.names = FALSE, quote = FALSE, sep = "\t"
)
