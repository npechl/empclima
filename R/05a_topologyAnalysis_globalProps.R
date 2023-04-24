
rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

# library(ggplot2)
# library(ggsci)
# library(ggh4x)
# 
# library(extrafont)
# 
# library(ggplotify)


# list of inputs ------------------------------------

globalProps  <- "test/globalProps-bootstrap.txt"
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

# 
# gr = ggplot(data = df, aes(x = ClimateZone, y = value)) +
#     
#     
#     geom_jitter(
#         width = .1, shape = 21, stroke = .1, size = 1,
#         color = "grey20", fill = "grey10"
#     ) +
#     
#     stat_boxplot(geom = 'errorbar', color = "black", width = .5, linewidth = .5) +
#     
#     geom_boxplot(aes(fill = Group), linewidth = .5, color = "black",
#                  outlier.shape = NA, alpha = .75) +
#     
#     facet_wrap2(vars(variable), ncol = 2, scales = "free_y", axes = "all") +
#     
#     scale_fill_npg() + 
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         legend.position = "bottom",
#         legend.title = element_blank(),
#         
#         panel.grid = element_blank(),
#         axis.line = element_line(linewidth = .3),
#         axis.ticks = element_line(linewidth = .3),
#         
#         axis.title = element_blank(),
#         strip.text = element_text(face = "bold"),
#         
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/globalProps-boxplot.pdf"),
#     width = 10, height = 10, units = "in", device = cairo_pdf
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/globalProps-boxplot.svg"),
#     width = 10, height = 10, units = "in"
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/globalProps-boxplot.jpeg"),
#     width = 10, height = 10, units = "in"
# )

library(rstatix)

wilc_stats <- df |>
    group_by(variable) |>
    pairwise_wilcox_test(value ~ ClimateZone, p.adjust.method = "fdr") |>
    setDT()

fwrite(
    wilc_stats, paste0(workdir, "/globalProperties.csv"),
    row.names = FALSE, quote = TRUE, sep = ","
)


