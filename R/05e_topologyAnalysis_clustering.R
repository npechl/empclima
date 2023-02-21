

rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggh4x)
library(extrafont)

library(ggplotify)

sample_map <- "emp-soil-analysis-sub5k/sample-metadata.Soil (non-saline).txt"
taxa_map   <- "emp-soil-analysis-sub5k/taxonomy-table.Soil (non-saline).txt"
hubs       <- "emp-soil-analysis-sub5k/hubs.txt"
clusters   <- "emp-soil-analysis-sub5k/clustering.txt"
workdir    <- dirname(sample_map)

climate_info <- "climate-classification-info.csv"



df = fread(clusters)
hubs = fread(hubs)
taxonomy <- fread(taxa_map)

df = merge(df, taxonomy, by.x = "Taxa", by.y = "TaxaIDabv", all.x = TRUE)

df$TaxaID = NULL

for(i in c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) {
    
    df[[i]] = str_split(df[[i]], "\\__", simplify = TRUE)[,2]
    df[[i]] = str_remove_all(df[[i]], "\\[|\\]")
    
}

rm(i)


clusters = unique(
    df[, c("Taxa", "Group", "ClimateZone", "Kingdom", "Phylum", 
           "Class", "Order", "Family", "Genus", "Species"), with = FALSE]
)

dfs = list()

for(var in c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) {
    
    tmp = clusters[, by = c("ClimateZone", "Group", var), .N]
    tmp = tmp[, by = .(ClimateZone, Group), Freq := N / sum(N)]
    colnames(tmp) = c("ClimateZone", "cluster", "name", "N", "Freq")
    tmp = tmp[order(ClimateZone, cluster, name), ]
    tmp$level = var
    tmp = tmp[, c("ClimateZone", "cluster", "level", "name", "N", "Freq"), with = FALSE]
    
    dfs[[var]] = tmp
}

rm(var)


dfs = rbindlist(dfs)

dfs[which(name == ""), ]$name = "unassigned"

dfs$level = factor(
    dfs$level,
    levels = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
)

# dfs$ann = ifelse(
#     dfs$Freq >= .9, dfs$name, "other"
# )

gr = ggplot(data = dfs, aes(x = level, y = Freq, fill = name)) +
    
    geom_bar(position = "fill", stat = "identity") +
    
    facet_grid2(
        rows = vars(ClimateZone),
        cols = vars(cluster)
    ) +
    
    scale_x_discrete(expand = c(0, 0))+
    scale_y_continuous(expand = c(0, 0), 
                       breaks = c(.25, .5, .75, 1),
                       labels = scales::percent) +
    
    scale_fill_viridis_d(option = "magma") +
    
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "none",
        
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 6),
        
        axis.title = element_blank(),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed"),
        
        # panel.grid.major.x = element_blank(),
        # panel.grid.major.y = element_blank(),
        # panel.grid.major.y = element_line(linewidth = .3, linetype = "dashed"),
        
        strip.text = element_text(face = "bold"),
        
        axis.ticks = element_line(linewidth = .3, color = "grey"),
        panel.border = element_rect(linewidth = .3, color = "grey", fill = NA),
        
        plot.margin = margin(10, 10, 10, 10)
    )


ggsave(
    plot = gr, filename = paste0(workdir, "/clustering-taxonomy.pdf"),
    width = 12, height = 10, units = "in", device = cairo_pdf
)

ggsave(
    plot = gr, filename = paste0(workdir, "/clustering-taxonomy.svg"),
    width = 12, height = 10, units = "in"
)

ggsave(
    plot = gr, filename = paste0(workdir, "/clustering-taxonomy.jpeg"),
    width = 12, height = 10, units = "in"
)

clusters$sample = paste0(clusters$ClimateZone, ";", clusters$Group)
clusters$value = 1


tmp = dcast(clusters, Taxa ~ sample, value.var = "value", fill = 0)

tmp = t(as.matrix(setDF(tmp[, 2:ncol(tmp)], rownames = tmp$Taxa)))


legend = fread("classification.txt")

my_col = str_remove_all(legend$rgbCode, "\\[|\\]")
my_col = str_split(my_col, "\\ ")
my_col = lapply(my_col, as.numeric)
my_col = lapply(my_col, function(x) {
    rgb(red = x[1], green = x[2], blue = x[3], maxColorValue = 255)
})

my_col = unlist(my_col)
names(my_col) = legend$ClimateZone


library(ComplexHeatmap)
library(ggplotify)

ha = rowAnnotation(
    "ClimateZone" = str_split(row.names(tmp), ";", simplify = TRUE)[,1],
    col = list(
        "ClimateZone" = my_col
    ),
    
    show_annotation_name = FALSE
)

ht = Heatmap(
    tmp,
    name = "Clustering",
    
    col = c("white", "red4"),
    
    border = TRUE,
    
    row_split = 4,
    
    show_column_names = FALSE,
    
    clustering_distance_columns = "euclidean",
    clustering_distance_rows = "euclidean",
    
    clustering_method_rows = "ward.D2",
    clustering_method_columns = "ward.D2",
    
    right_annotation = ha,
    
    column_names_gp = gpar(fontsize = 1),
    row_names_gp = gpar(fontsize = 7)
)

gr = as.ggplot(ht)

ggsave(
    plot = gr, filename = paste0(workdir, "/clustering-grouping.pdf"),
    width = 10, height = 12, units = "in", device = cairo_pdf
)

ggsave(
    plot = gr, filename = paste0(workdir, "/clustering-grouping.svg"),
    width = 10, height = 12, units = "in"
)

ggsave(
    plot = gr, filename = paste0(workdir, "/clustering-grouping.jpeg"),
    width = 10, height = 12, units = "in"
)



rm(clusters, dfs, gr, ht, taxonomy, tmp)
gc()


df = fread("emp-soil-analysis-sub2k/sub-networks.txt")

library(ggnetwork)
library(ggsci)

gr = ggplot(df, aes(x = x, y = y, xend = xend, yend = yend)) +

    geom_edges(color = "grey70", linewidth = .25) +

    geom_nodes(
        aes(size = degree, fill = as.character(cluster), color = hub),
        shape = 21, stroke = .25, alpha = .95
    ) +

    geom_nodes(
        data = df[which(hub == "yes"), ],
        aes(size = degree, fill = as.character(cluster), color = hub),
        shape = 21, stroke = .5, alpha = .95
    ) +
    
    facet_wrap2(vars(ClimateZone), ncol = 5) +

    scale_size_continuous(range = c(1, 6)) +

    scale_fill_npg() +
    scale_color_manual(values = c("yes" = "yellow3", "no"  = "#3b0000")) +

    theme_void(base_family = "Calibri") +

    theme(
        legend.position = "none",
        
        strip.text = element_text(face = "bold"),
        
        plot.margin = margin(15, 15, 15, 15)
    )


ggsave(
    plot = gr, filename = paste0(workdir, "/sub-networks.pdf"),
    width = 12, height = 8, units = "in", device = cairo_pdf
)

ggsave(
    plot = gr, filename = paste0(workdir, "/sub-networks.svg"),
    width = 12, height = 8, units = "in"
)









