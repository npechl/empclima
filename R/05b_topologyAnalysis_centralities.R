
rm(list = ls())
gc()

# setwd(dir = dirname(path = here::here()))

# load libraries -----------------------------

library(data.table)
library(stringr)

library(rstatix)

# library(ggplot2)
# library(ggh4x)
# library(ggsci)
# library(ggdist)
# library(ggforce)
# 
# library(ggpubr)
# library(extrafont)

# list of inputs ------------------------------------

sample_map      <- "emp-soil-analysis-clean-sub5k/sample-metadata.Soil (non-saline).txt"
abundance_table <- "emp-soil-analysis-clean-sub5k/abundance-table.Soil (non-saline).txt"
centralites     <- "emp-soil-analysis-clean-sub5k/centralities-bootstrap.txt"
taxa_map        <- "emp-soil-analysis-clean-sub5k/taxonomy-table.Soil (non-saline).txt"
workdir         <- dirname(centralites)
climate_info    <- "climate-classification-info.csv"


df           <- fread(centralites)
climate_info <- fread(climate_info)
taxa_map <- fread(taxa_map)

centralities_stats <- list()

df = merge(df, climate_info, by.x = "ClimateZone", by.y = "Code", all.x = TRUE)
df = merge(df, taxa_map, by.x = "Taxa", by.y = "TaxaIDabv", all.x = TRUE)

df$TaxaID = NULL

for(i in c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) {
    
    df[[i]] = str_split(df[[i]], "\\__", simplify = TRUE)[,2]
    df[[i]] = str_remove_all(df[[i]], "\\[|\\]")
    
    df[[i]] = str_to_lower(df[[i]])
    
}

rm(i)

df[which(Phylum == ""), ]$Phylum   = "unassigned"
df[which(Class == ""), ]$Class     = "unassigned"
df[which(Order == ""), ]$Order     = "unassigned"
df[which(Family == ""), ]$Family   = "unassigned"
df[which(Genus == ""), ]$Genus     = "unassigned"
df[which(Species == ""), ]$Species = "unassigned"

library(ggplot2)
library(ggforce)

# ggplot(data = df, aes(x = ClimateZone, y = degree)) +
#     
#     geom_point(position = position_jitternormal(sd_y = 0, sd_x = .05),
#                color = "grey50") +
#     
#     geom_boxplot(fill = NA, outlier.shape = NA)

centralites_taxonomy_stats = list()

for(i in c("Phylum", "Class", "Order", "Family", "Genus", "Species")) {
    
    df = df[, by = c("ClimateZone", i), Filt := .N]
    
    stats = df[which(Filt >= 3), ]
    
    stats$var = stats[[i]]
    
    # Degree centrality -------------
    degree.stats = stats %>% 
        
        group_by(var) %>%
        
        wilcox_test(degree ~ ClimateZone, p.adjust.method = "holm")
    
    degree.stats = setDT(degree.stats)
    
    
    # Betweenness centrality ------------------
    between.stats = stats %>% 
        
        group_by(var) %>%
        
        wilcox_test(between ~ ClimateZone, p.adjust.method = "holm")
    
    between.stats = setDT(between.stats)
    
    # Closeness centrality ------------------
    close.stats = stats %>% 
        
        group_by(var) %>%
        
        wilcox_test(close ~ ClimateZone, p.adjust.method = "holm")
    
    close.stats = setDT(close.stats)
    
    
    # Eigenvector centrality ------------------
    eigenv.stats = stats %>% 
        
        group_by(var) %>%
        
        wilcox_test(eigenv ~ ClimateZone, p.adjust.method = "holm")
    
    eigenv.stats = setDT(eigenv.stats)
    
    centralites_taxonomy_stats[[i]] = rbind(
        degree.stats,
        between.stats,
        close.stats,
        eigenv.stats
    )
    
}

centralites_taxonomy_stats = rbindlist(centralites_taxonomy_stats, idcol = "level")

fwrite(
    centralites_taxonomy_stats, "centralites_taxonomy_stats.csv",
    row.names = FALSE, quote = TRUE, sep = ","
)

# centralities_stats[["Degree"]] = stats
# 
# df$`Level of Heat` = str_replace_all(df$`Level of Heat`, "summer", "sum")
# 
# 
# ggplot(data = df, aes(x = ClimateZone, y = degree)) +
#     
#     geom_point(
#         position = position_jitternormal(sd_x = .05, sd_y = 0),
#         shape = 21, size = 2, stroke = .05, fill = "grey10", color = "grey"
#     ) +
#     
#     geom_boxplot(fill = "grey", alpha = .5, outlier.shape = NA) +
#     
#     theme_minimal()
# 
# 
# gr = ggplot(data = df, 
#        aes(x = weave_factors(ClimateZone, `Level of Heat`, `Precipitation Type`, Group),
#            y = degree)) +
#     
#     stat_slabinterval(aes(fill = Group, fill_ramp = after_stat(level)),
#                       .width = c(.50, .80, .95, .99)) +
#     
#     scale_fill_npg() +
#     
#     guides(x = "axis_nested") +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         legend.position = "none",
#         axis.title.x = element_blank(),
#         ggh4x.axis.nestline.x = element_line(color = "grey", linewidth = 0.25),
#         
#         panel.grid = element_blank(),
#         
#         axis.line = element_line(linewidth = .3),
#         axis.ticks = element_line(linewidth = .3)
#     ) +
#     
#     labs(y = "Degree centrality")
# 
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Degree-centrality.pdf"),
#     width = 10, height = 8, units = "in", device = cairo_pdf
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Degree-centrality.svg"),
#     width = 10, height = 8, units = "in"
# )
# 
# 
# 
# stats = pairwise_wilcox_test(
#     data = df, between ~ ClimateZone, p.adjust.method = "holm"
# )
# stats = setDT(stats)
# 
# 
# centralities_stats[["Betweenness"]] = stats
# 
# gr = ggplot(data = df, 
#             aes(x = weave_factors(ClimateZone, `Level of Heat`, `Precipitation Type`, Group),
#                 y = between)) +
#     
#     stat_slabinterval(aes(fill = Group, fill_ramp = after_stat(level)),
#                       .width = c(.50, .80, .95, .99)) +
#     
#     scale_fill_npg() +
#     
#     guides(x = "axis_nested") +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         legend.position = "none",
#         axis.title.x = element_blank(),
#         ggh4x.axis.nestline.x = element_line(color = "grey", linewidth = 0.25),
#         
#         panel.grid = element_blank(),
#         
#         axis.line = element_line(linewidth = .3),
#         axis.ticks = element_line(linewidth = .3)
#     ) +
#     
#     labs(y = "Betweenness centrality")
# 
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Betweenness-centrality.pdf"),
#     width = 10, height = 8, units = "in", device = cairo_pdf
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Betweenness-centrality.svg"),
#     width = 10, height = 8, units = "in"
# )
# 
# 
# stats = pairwise_wilcox_test(
#     data = df, close ~ ClimateZone, p.adjust.method = "holm"
# )
# stats = setDT(stats)
# 
# 
# centralities_stats[["Closeness"]] = stats
# 
# gr = ggplot(data = df, 
#             aes(x = weave_factors(ClimateZone, `Level of Heat`, `Precipitation Type`, Group),
#                 y = close)) +
#     
#     stat_slabinterval(aes(fill = Group, fill_ramp = after_stat(level)),
#                       .width = c(.50, .80, .95, .99)) +
#     
#     scale_fill_npg() +
#     
#     guides(x = "axis_nested") +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         legend.position = "none",
#         axis.title.x = element_blank(),
#         ggh4x.axis.nestline.x = element_line(color = "grey", linewidth = 0.25),
#         
#         panel.grid = element_blank(),
#         
#         axis.line = element_line(linewidth = .3),
#         axis.ticks = element_line(linewidth = .3)
#     ) +
#     
#     labs(y = "Closeness")
# 
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Closeness-centrality.pdf"),
#     width = 10, height = 8, units = "in", device = cairo_pdf
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Closeness-centrality.svg"),
#     width = 10, height = 8, units = "in"
# )
# 
# centralities_stats = rbindlist(centralities_stats)
# 
# fwrite(
#     centralities_stats, paste0(workdir, "/centralities-stats.csv"),
#     row.names = FALSE, quote = TRUE, sep = ","
# )
# 
# rm(gr, df, stats, climate_info)
# gc()
# 
# 
# 
# centralities_stats = split(centralities_stats, centralities_stats$.y.)
# 
# for(i in names(centralities_stats)) {
#     
#     x = centralities_stats[[i]]
#     x = x[which(p.adj <= 0.05), ]
#     
#     x = data.table(
#         "Group1" = c(x$group1, x$group2),
#         "Group2" = c(x$group2, x$group1)
#     )
#     
#     x = x[, by = Group1, .(
#         diffs = paste(sort(Group2), collapse = ", "),
#         N     = .N
#     )]
#     
#     x = x[order(-N), ]
# }















