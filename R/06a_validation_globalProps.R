
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

globalProps  <- c(
    "emp-soil-analysis-sub2k/globalProps-bootstrap.txt",
    "emp-soil-analysis-sub5k/globalProps-bootstrap.txt",
    "emp-soil-analysis-sub10k/globalProps-bootstrap.txt"
)
climate_info <- "climate-classification-info.csv"

climate_cols           <- "Beck_KG_V1/classification.txt"

workdir      <- dirname(globalProps)


df = list()
climate_info <- fread(climate_info)

for(i in globalProps) {
    
    d <- dirname(i)
    d <- str_remove_all(d, "emp|soil|analysis|\\-")
    
    df[[d]] <- fread(i)
    
    
}


df = rbindlist(df, idcol = "subset")

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

# df = df[order(Run), ]



df$nTaxa = NULL
# df$Run = NULL

df = melt(
    df, id.vars = c("subset", "ClimateZone", "Description", "Group", 
                    "Precipitation Type", "Level of Heat", "Run"), 
    variable.factor = FALSE, value.factor = FALSE
)

legend = fread(climate_cols)


my_col = str_remove_all(legend$rgbCode, "\\[|\\]")
my_col = str_split(my_col, "\\ ")
my_col = lapply(my_col, as.numeric)
my_col = lapply(my_col, function(x) {
    rgb(red = x[1], green = x[2], blue = x[3], maxColorValue = 255)
})

my_col = unlist(my_col)
names(my_col) = legend$ClimateZone

df$subset = factor(df$subset, levels = c("sub2k", "sub5k", "sub10k"))

gr = ggplot(data = df, aes(x = subset, y = value, fill = ClimateZone)) +
    
    
    # geom_jitter(width = .1, shape = 21, stroke = .25, color = "grey70", fill = "grey40") +
    
    # stat_boxplot(geom = 'errorbar', color = "black", width = .5, linewidth = .5) +
    
    geom_boxplot(linewidth = .1, color = "black", outlier.size = .75) +
    
    facet_wrap2(vars(variable), ncol = 4, scales = "free_y", axes = "all") +
    
    scale_fill_manual(
        values = my_col
    ) +
    
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
    ) +
    
    guides(
        fill = guide_legend(nrow = 2, byrow = TRUE)
    )

ggsave(
    plot = gr, filename = paste0("globalProps-boxplot.pdf"),
    width = 20, height = 5, units = "in", device = cairo_pdf
)

ggsave(
    plot = gr, filename = paste0("globalProps-boxplot.svg"),
    width = 20, height = 5, units = "in"
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






















