
rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggsci)
library(ggh4x)

library(extrafont)

library(ggplotify)


# list of inputs ------------------------------------

globalProps  <- "emp-soil-analysis-clean-sub5k/globalProps-bootstrap.txt"
climate_info <- "climate-classification-info.csv"
workdir      <- dirname(globalProps)


df           <- fread(globalProps)
climate_info <- fread(climate_info)

df = merge(df, climate_info, by.x = "ClimateZone", by.y = "Code", all.x = TRUE)

index = c(
    "Average path length", 
    "Transitivity", 
    "Modularity", 
    # "Edge connectivity", 
    "Density"
)

for(i in index) {
    
    df[[i]] = as.numeric(df[[i]])
    
}

df = df[order(Run), ]



df$nTaxa = NULL
# df$Run = NULL

df = melt(
    df, id.vars = c("ClimateZone", "Description", "Group", 
                    "Precipitation Type", "Level of Heat", "Run"), 
    variable.factor = FALSE, value.factor = FALSE
)


gr = ggplot(data = df, aes(x = ClimateZone, y = value)) +
    
    
    geom_jitter(width = .1, shape = 21, stroke = .25, color = "grey70", fill = "grey40") +
    
    stat_boxplot(geom = 'errorbar', color = "black", width = .5, linewidth = .5) +
    
    geom_boxplot(aes(fill = Group), linewidth = .5, color = "black",
                 outlier.shape = NA, alpha = .75) +
    
    facet_wrap2(vars(variable), ncol = 2, scales = "free_y", axes = "all") +
    
    scale_fill_npg() + 
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        
        panel.grid = element_blank(),
        axis.line = element_line(linewidth = .3),
        axis.ticks = element_line(linewidth = .3),
        
        axis.title = element_blank(),
        strip.text = element_text(face = "bold"),
        
        plot.margin = margin(20, 20, 20, 20)
    )

ggsave(
    plot = gr, filename = paste0(workdir, "/globalProps-boxplot.pdf"),
    width = 10, height = 10, units = "in", device = cairo_pdf
)

ggsave(
    plot = gr, filename = paste0(workdir, "/globalProps-boxplot.svg"),
    width = 10, height = 10, units = "in"
)


df$sample = paste0(df$variable, ";", df$Run)


mm = dcast(df, sample ~ ClimateZone, value.var = "value")
mm = as.matrix(
    setDF(
        mm[, 2:ncol(mm)], rownames = mm$sample
    )
)

zm = t(scale(t(mm), center = TRUE, scale = TRUE))

library(ComplexHeatmap)

row_split = str_split(row.names(zm), "\\;", simplify = TRUE)[, 1]

ht = Heatmap(
    zm,
    name = "globalProps",
    
    clustering_distance_rows = "euclidean",
    clustering_distance_columns = "euclidean",
    
    clustering_method_columns = "ward.D2",
    clustering_method_rows = "ward.D2",
    
    row_split = row_split,
    column_split = 2,
    
    show_row_names = FALSE,
    
    border = TRUE,
    
    use_raster = TRUE,
    
    heatmap_legend_param = list(
        legend_height = unit(10, "lines"),
        grid_width = unit(.5, "lines")
    )
)

gr = as.ggplot(ht)

ggsave(
    plot = gr, filename = paste0(workdir, "/globalProps-heatmap.pdf"),
    width = 10, height = 10, units = "in", device = cairo_pdf
)

ggsave(
    plot = gr, filename = paste0(workdir, "/globalProps-heatmap.svg"),
    width = 10, height = 10, units = "in"
)




# ggplot(df, aes(x = ClimateZone, y = value)) +
#     
#     geom_col(width = .5, alpha = .75, aes(fill = Group), color = "grey10", linewidth = .25) +
#     
#     geom_line(group = 1, color = "grey10", linewidth = 1, linetype = "dotted") +
#     
#     geom_point() +
#     
#     facet_wrap2(vars(variable), ncol = 1, scales = "free", axes = "all") +
#     
#     scale_y_continuous(expand = c(0, 0)) +
#     scale_fill_futurama() +
#     
#     coord_cartesian(expand = TRUE, clip = "off") +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         strip.text = element_text(face = "bold", margin = margin(b = 10)),
#         
#         axis.title = element_blank(),
#         
#         panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line(linewidth = .3, linetype = "dashed"),
#         
#         axis.line.y = element_line(linewidth = .3),
#         axis.ticks.y = element_line(linewidth = .3),
#         
#         plot.margin = margin(20, 20, 20, 20)
#     )






















