
rm(list = ls())
gc()

# load libraries and inputs ------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggh4x)

library(extrafont)

library(raster)

# library(rnaturalearth)
# library(rgeos)
# library(sf)

sample_metadata        <- "emp-soil-analysis-clean-sub5k/sample-metadata.Soil (non-saline).txt"
climate_classification <- "Beck_KG_V1/Beck_KG_V1_present_0p083.tif"
climate_info           <- "Beck_KG_V1/classification.txt"
workdir         <- dirname(sample_metadata)


# import BIOM file ------------------------

df    <- fread(sample_metadata)
# world <- ne_countries(scale = "medium", returnclass = "sf")

x  = raster(climate_classification)
sf = setDT(as.data.frame(x, xy = TRUE))
colnames(sf) = c("x", "y", "code")

legend = fread(climate_info)

sf$ClimateZone = legend[match(sf$code, legend$indexCode), ]$ClimateZone

my_col = str_remove_all(legend$rgbCode, "\\[|\\]")
my_col = str_split(my_col, "\\ ")
my_col = lapply(my_col, as.numeric)
my_col = lapply(my_col, function(x) {
    rgb(red = x[1], green = x[2], blue = x[3], maxColorValue = 255)
})

my_col = unlist(my_col)
names(my_col) = legend$ClimateZone


# clusters = data.table(
#     "ClimateZone" = c(
#         "Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", "Csa", "Csb", "Csc", 
#         "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc", "Dsa", "Dsb", "Dsc", "Dsd", 
#         "Dwa", "Dwb", "Dwc", "Dwd", "Dfa", "Dfb", "Dfc", "Dfd", "ET",  "EF"
#     ),
#     
#     "Cluster" = c(
#         3, 2, 2, 0, 0, 0, 1, 1, 3, 0,
#         0, 0, 0, 3, 3, 0, 0, 0, 0, 0,
#         0, 2, 2, 0, 3, 3, 0, 0, 1, 1
#     )
# )

# sf$cluster = clusters[match(sf$ClimateZone, clusters$ClimateZone), ]$Cluster
# sf[which(sf$cluster == 0), ]$cluster = NA
# 
# clusters = clusters[, by = Cluster, .(
#     label = paste0(paste(sort(ClimateZone), collapse = ", "))
# )]
# 
# clusters = clusters[which(Cluster != 0), ]
# 
# index = match(sf$cluster, clusters$Cluster)
# sf$cluster_label = clusters[index, ]$label
# 
# sf = sf[which(!is.na(sf$cluster_label)), ]

gr = ggplot() +
    
    geom_tile(
        data = sf, aes(x = x, y = y, fill = ClimateZone)
    ) +
    
    geom_point(
        data = df, aes(x = longitude_deg, y = latitude_deg),
        shape = 21, stroke = .25, size = 1.5,
        fill = "black", color = "white"
    ) +
    
    scale_fill_manual(
        values = my_col,
        
        na.value = "white"
    ) +
    
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    coord_sf() +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        panel.grid = element_blank(),
        
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        
        panel.background = element_rect(fill = "white", color = NA),
        # panel.border = element_rect(fill = NA, linewidth = .3),
        
        strip.text = element_text(face = "bold")
    ) +
    
    guides(
        fill = guide_legend(
            nrow = 3, 
            override.aes = list(size = 1), 
            label.theme = element_text(size = 1)
        )
    )



# ggsave(
#     plot = gr, paste0(workdir, "/samplingWorld.pdf"),
#     width = 8, height = 4, units = "in"
# )

ggsave(
    plot = gr, paste0(workdir, "/samplingWorld.jpeg"),
    width = 12, height = 4, units = "in"
)


# ggsave(
#     plot = gr, paste0(workdir, "/samplingWorld.svg"),
#     width = 8, height = 4, units = "in"
# )











