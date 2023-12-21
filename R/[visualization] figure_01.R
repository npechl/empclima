

rm(list = ls())
gc()

# Fig. 1 A =============================================


library(data.table)
library(stringr)

library(ggplot2)
library(extrafont)
library(rbiom)

df = fread("emp-soil-analysis-clean-release1-v2/prevalence.csv")

s  = fread("emp-soil-analysis-clean-release1-v2/emp_qiime_mapping_qc_filtered.tsv")
m  = read.biom(src = "data-raw/emp_deblur_90bp.qc_filtered.biom", tree = FALSE)

s = s[which(empo_3 == "Soil (non-saline)")]

subset <- rbiom::select(m, samples = s$`#SampleID`)

m = counts(subset)

gr1 = ggplot(data = df, aes(x = `No. of Samples`, y = `No. of ESVs`)) +
    
    geom_point(shape = 21, fill = "grey25", color = "black", 
               size = 1.5, stroke = .1) +
    
    geom_smooth() +
    
    scale_y_continuous(
        trans = "log10",
        expand = c(0, 0),
        breaks = c(1, 10, 100, 1000, 10000, 30000),
        labels = function(x) {
            
            return(
                paste0(
                    format(
                        x, big.mark = " ", decimal.mark = ".", 
                        scientific = FALSE
                    ), 
                    "\n",
                    "(", round(100 * x / nrow(m), digits = 2), "%)"
                )
            )
        }
        
        # function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)
    ) +
    
    scale_x_continuous(
        expand = c(0, 0), limits = c(0, max(df$`No. of Samples`)),
        labels = function(x) {
            return(
                paste0(
                    x, "\n",
                    "(", round(
                        100 * x / (ncol(m) - 1), digits = 2
                    ),
                    "%)"
                )
            )
        }
    ) +
    
    coord_cartesian(expand = TRUE, clip = "on") +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "none",
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed"),
        panel.grid.minor = element_blank(), # element_line(linewidth = .3, linetype = "dashed"),
        
        axis.ticks = element_line(linewidth = .3),
        
        plot.subtitle = element_text(margin = margin(b = 10)),
        plot.title.position = "plot",
        
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        
        panel.border = element_rect(fill = NA, linewidth = .3)
    ) +
    
    labs(x = "No. of samples", y = "No. of ESVs")

# Fig. 1 B =============================================

df = fread("emp-soil-analysis-clean-release1-v2/ESV_distribution.csv")

# s = fread("draft/Supplementary Table 1.csv")
# 
# 
# df = merge(df, s, by.x = "ClimateZone", by.y = "Code")


library(ggrepel)
library(ggforce)
library(paletteer)
library(extrafont)

gr2 = ggplot(data = df, aes(x = `No of Samples`, y = `No of ESVs`)) +
    
    geom_point(
        aes(size = Ratio, fill = Group),
        shape = 21, color = "grey10", stroke = .25
    ) +
    
    geom_label_repel(
        aes(label = ClimateZone),
        box.padding = .25,
        
        label.size = NA, fill = alpha("white", alpha = .5),
        
        fontface = "bold", family = "Calibri"
    ) +
    
    scale_x_continuous(
        trans = "log2"
    ) +
    
    scale_fill_manual(
        values = paletteer_d("ggthemes::Color_Blind"), # paletteer_d("ggthemes::Color_Blind"),
        guide = guide_legend(override.aes = list(size = 3))
    ) +
    
    scale_size_continuous(range = c(2, 6)) +
    
    coord_cartesian(expand = TRUE, clip = "on") +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "right",
        
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed"),
        
        panel.border = element_rect(fill = NA, linewidth = .3),
        axis.ticks = element_line(linewidth = .3)
    ) + 
    
    labs(size = "No. of ESVs / No. of Samples")
    
    

# Fig. 1 C =============================================

# rm(list = ls())
# gc()

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

sample_metadata        <- "emp-soil-analysis-clean-release1-v2/sample-metadata.Soil (non-saline).txt"
climate_classification <- "Beck_KG_V1/Beck_KG_V1_present_0p5.tif"
climate_info           <- "Beck_KG_V1/classification.txt"
workdir                <- dirname(sample_metadata)


# import BIOM file ------------------------

df    <- fread(sample_metadata)

x            = raster(climate_classification)
sf           = setDT(as.data.frame(x, xy = TRUE))
colnames(sf) = c("x", "y", "code")

legend = fread(climate_info)

sf$ClimateZone = legend[match(sf$code, legend$indexCode), ]$ClimateZone

my_col = str_remove_all(legend$rgbCode, "\\[|\\]")
my_col = str_split(my_col, "\\ ")
my_col = lapply(my_col, as.numeric)
my_col = lapply(my_col, function(x) {
    rgb(red = x[1], green = x[2], blue = x[3], maxColorValue = 255)
})

my_col        = unlist(my_col)
names(my_col) = legend$ClimateZone


gr3 = ggplot() +
    
    geom_tile(
        data = sf, aes(x = x, y = y, fill = ClimateZone)
    ) +
    
    geom_point(
        data = df, aes(x = longitude_deg, y = latitude_deg),
        shape = 21, stroke = .1, size = 1.5,
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
            nrow = 2, 
            override.aes = list(shape = 21, color = "white", size = .75),
            byrow = TRUE,
            label.theme = element_text(size = 11, family = "Calibri")
        )
    )




# patchwork ==================================




library(patchwork)


multi = (gr1 | gr2) / gr3 + 
    
    plot_layout(heights = c(1, 1)) +
    
    plot_annotation(
        tag_levels = "A", 
        theme = theme(plot.tag = element_text(size = 11, face = "bold", family = "Calibri"))
    )

ggsave(
    plot = multi, filename = "Fig1.pdf", device = cairo_pdf,
    width = 12, height = 12, units = "in"
)

# ggsave(
#     plot = gr3, filename = "Fig1C.jpeg",
#     width = 12, height = 6, units = "in", dpi = 600
# )




