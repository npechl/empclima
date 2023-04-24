

rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(igraph)

graph_obj            <- "test/SpiecEasi-Soil (non-saline).graphml"
taxa_mapping         <- "test/taxonomy-table.Soil (non-saline).txt"
centralities_mapping <- "test/centralities-bootstrap.txt"

workdir  <- dirname(graph_obj)
taxonomy <- fread(taxa_mapping)

taxonomy$TaxaID = NULL


centr = fread(centralities_mapping)
centr = unique(centr[, c("ClimateZone", "Taxa"), with = FALSE])

g = read_graph(file = graph_obj, format = "graphml")
g <- delete_edges(g, edges = which(E(g)$weight < 0))

clusters = groups(cluster_fast_greedy(g))
clusters = lapply(clusters, function(x) { data.table("Taxa" = x) })
clusters = rbindlist(clusters, idcol = "Cluster")
clusters = merge(clusters, taxonomy, by.x = "Taxa", by.y = "TaxaIDabv", all.x = TRUE)


for(i in c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) {
    
    clusters[[i]] = str_split(clusters[[i]], "\\__", simplify = TRUE)[,2]
    clusters[[i]] = str_remove_all(clusters[[i]], "\\[|\\]")
    
}

rm(i)

clusters = melt(
    clusters, id.vars = c("Taxa", "Cluster"), 
    variable.factor = FALSE, value.factor = FALSE,
    variable.name = "level", value.name = "name"
)

clusters[which(name == "")]$name = "unassigned"


clusters$level = factor(
    clusters$level, 
    levels = c(
        "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"
    )
)

cluster1 = clusters[, by = .(Cluster, level, name), .N]
cluster1 = cluster1[, by = .(Cluster, level), Freq := round(N / sum(N), digits = 4)]

cluster1 = cluster1[order(Cluster, level, -Freq), ]

fwrite(
    cluster1, paste0(workdir, "/clustering1.csv"),
    row.names = FALSE, quote = TRUE, sep = ","
)



clusters = dcast(clusters, Cluster + Taxa ~ level, value.var = "name")

cluster2 = merge(centr, clusters, by = "Taxa", all.x = TRUE)

cluster2 = melt(
    cluster2, id.vars = c("Taxa", "ClimateZone", "Cluster"),
    variable.factor = FALSE, value.factor = FALSE,
    variable.name = "level", value.name = "name"
)


cluster2 = cluster2[, by = .(ClimateZone, Cluster, level, name), .N]
cluster2 = cluster2[, by = .(ClimateZone, Cluster, level), Freq := round(N / sum(N), digits = 4)]

cluster2$level = factor(
    cluster2$level, 
    levels = c(
        "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"
    )
)

cluster2 = cluster2[order(ClimateZone, Cluster, level, -N), ]

fwrite(
    cluster2, paste0(workdir, "/clustering3.csv"),
    row.names = FALSE, quote = TRUE, sep = ","
)

cluster3 = cluster2[which(level == "Kingdom")]

cluster3$level = NULL
cluster3$name  = NULL
cluster3$Freq  = NULL

cluster3 = cluster3[, by = Cluster, Freq := round(N / sum(N), digits = 4)]
cluster3 = dcast(cluster3, ClimateZone ~ Cluster, value.var = "Freq")

fwrite(
    cluster3, paste0(workdir, "/clustering2.csv"),
    row.names = FALSE, quote = TRUE, sep = ","
)



# clusters = unique(
#     df[, c("Taxa", "Group", "ClimateZone", "Kingdom", "Phylum", 
#            "Class", "Order", "Family", "Genus", "Species"), with = FALSE]
# )
# 
# dfs = list()
# 
# for(var in c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) {
#     
#     tmp = clusters[, by = c("ClimateZone", "Group", var), .N]
#     tmp = tmp[, by = .(ClimateZone, Group), Freq := N / sum(N)]
#     colnames(tmp) = c("ClimateZone", "cluster", "name", "N", "Freq")
#     tmp = tmp[order(ClimateZone, cluster, name), ]
#     tmp$level = var
#     tmp = tmp[, c("ClimateZone", "cluster", "level", "name", "N", "Freq"), with = FALSE]
#     
#     dfs[[var]] = tmp
# }
# 
# rm(var)
# 
# 
# dfs = rbindlist(dfs)
# 
# dfs[which(name == ""), ]$name = "unassigned"
# 
# dfs$level = factor(
#     dfs$level,
#     levels = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
# )
# 
# # dfs$ann = ifelse(
# #     dfs$Freq >= .9, dfs$name, "other"
# # )
# 
# gr = ggplot(data = dfs, aes(x = level, y = Freq, fill = name)) +
#     
#     geom_bar(position = "fill", stat = "identity") +
#     
#     facet_grid2(
#         rows = vars(ClimateZone),
#         cols = vars(cluster)
#     ) +
#     
#     scale_x_discrete(expand = c(0, 0))+
#     scale_y_continuous(expand = c(0, 0), 
#                        breaks = c(.25, .5, .75, 1),
#                        labels = scales::percent) +
#     
#     scale_fill_viridis_d(option = "magma") +
#     
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         legend.position = "none",
#         
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.text.y = element_text(size = 6),
#         
#         axis.title = element_blank(),
#         
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_line(linewidth = .3, linetype = "dashed"),
#         
#         # panel.grid.major.x = element_blank(),
#         # panel.grid.major.y = element_blank(),
#         # panel.grid.major.y = element_line(linewidth = .3, linetype = "dashed"),
#         
#         strip.text = element_text(face = "bold"),
#         
#         axis.ticks = element_line(linewidth = .3, color = "grey"),
#         panel.border = element_rect(linewidth = .3, color = "grey", fill = NA),
#         
#         plot.margin = margin(10, 10, 10, 10)
#     )
# 
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/clustering-taxonomy.pdf"),
#     width = 12, height = 10, units = "in", device = cairo_pdf
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/clustering-taxonomy.svg"),
#     width = 12, height = 10, units = "in"
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/clustering-taxonomy.jpeg"),
#     width = 12, height = 10, units = "in"
# )
# 
# clusters$sample = paste0(clusters$ClimateZone, ";", clusters$Group)
# clusters$value = 1
# 
# 
# tmp = dcast(clusters, Taxa ~ sample, value.var = "value", fill = 0)
# 
# tmp = t(as.matrix(setDF(tmp[, 2:ncol(tmp)], rownames = tmp$Taxa)))
# 
# 
# legend = fread("classification.txt")
# 
# my_col = str_remove_all(legend$rgbCode, "\\[|\\]")
# my_col = str_split(my_col, "\\ ")
# my_col = lapply(my_col, as.numeric)
# my_col = lapply(my_col, function(x) {
#     rgb(red = x[1], green = x[2], blue = x[3], maxColorValue = 255)
# })
# 
# my_col = unlist(my_col)
# names(my_col) = legend$ClimateZone
# 
# 
# library(ComplexHeatmap)
# library(ggplotify)
# 
# ha = rowAnnotation(
#     "ClimateZone" = str_split(row.names(tmp), ";", simplify = TRUE)[,1],
#     col = list(
#         "ClimateZone" = my_col
#     ),
#     
#     show_annotation_name = FALSE
# )
# 
# ht = Heatmap(
#     tmp,
#     name = "Clustering",
#     
#     col = c("white", "red4"),
#     
#     border = TRUE,
#     
#     row_split = 4,
#     
#     show_column_names = FALSE,
#     
#     clustering_distance_columns = "euclidean",
#     clustering_distance_rows = "euclidean",
#     
#     clustering_method_rows = "ward.D2",
#     clustering_method_columns = "ward.D2",
#     
#     right_annotation = ha,
#     
#     column_names_gp = gpar(fontsize = 1),
#     row_names_gp = gpar(fontsize = 7)
# )
# 
# gr = as.ggplot(ht)
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/clustering-grouping.pdf"),
#     width = 10, height = 12, units = "in", device = cairo_pdf
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/clustering-grouping.svg"),
#     width = 10, height = 12, units = "in"
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/clustering-grouping.jpeg"),
#     width = 10, height = 12, units = "in"
# )
# 
# 
# 
# rm(clusters, dfs, gr, ht, taxonomy, tmp)
# gc()
# 
# 
# df = fread("emp-soil-analysis-sub2k/sub-networks.txt")
# 
# library(ggnetwork)
# library(ggsci)
# 
# gr = ggplot(df, aes(x = x, y = y, xend = xend, yend = yend)) +
# 
#     geom_edges(color = "grey70", linewidth = .25) +
# 
#     geom_nodes(
#         aes(size = degree, fill = as.character(cluster), color = hub),
#         shape = 21, stroke = .25, alpha = .95
#     ) +
# 
#     geom_nodes(
#         data = df[which(hub == "yes"), ],
#         aes(size = degree, fill = as.character(cluster), color = hub),
#         shape = 21, stroke = .5, alpha = .95
#     ) +
#     
#     facet_wrap2(vars(ClimateZone), ncol = 5) +
# 
#     scale_size_continuous(range = c(1, 6)) +
# 
#     scale_fill_npg() +
#     scale_color_manual(values = c("yes" = "yellow3", "no"  = "#3b0000")) +
# 
#     theme_void(base_family = "Calibri") +
# 
#     theme(
#         legend.position = "none",
#         
#         strip.text = element_text(face = "bold"),
#         
#         plot.margin = margin(15, 15, 15, 15)
#     )
# 
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/sub-networks.pdf"),
#     width = 12, height = 8, units = "in", device = cairo_pdf
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/sub-networks.svg"),
#     width = 12, height = 8, units = "in"
# )









