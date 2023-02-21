


rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(treeio)

library(ggplot2)
library(extrafont)

library(ggsci)

sample_map   <- "emp-soil-analysis-sub5k/sample-metadata.Soil (non-saline).txt"
taxa_map     <- "emp-soil-analysis-sub5k/taxonomy-table.Soil (non-saline).txt"
centralities <- "emp-soil-analysis-sub5k/centralities-bootstrap.txt"
hubs         <- "emp-soil-analysis-sub5k/hubs.txt"
tree         <- "emp90.5000_1000_rxbl_placement_pruned75.tog.tre"
workdir      <- dirname(sample_map)

climate_info <- "climate-classification-info.csv"


df = fread(hubs)

x = fread(taxa_map)

df = merge(df, x, by.x = "Taxa", by.y = "TaxaIDabv", all.x = TRUE)



lvl = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

df$label = df$Kingdom

for(i in seq_len(length(lvl))) {
    
    
    x = df[[ lvl[i] ]]
    x = str_split(x, "_")
    x = lapply(x, function(ob) { return(ob[length(ob)]) })
    x = unlist(x)
    
    x = str_remove_all(x, "\\[|\\]")
    
    # x = ifelse(
    #     x == "", paste0(str_to_lower(str_sub(lvl[i], 1, 1)), "_unassigned"), x
    # )
    
    df$label = ifelse(
        x == "" & i > 1, 
        df$label, paste0(str_to_lower(str_sub(lvl[i], 1, 1)), "__", x)
    )
    
    df[[ lvl[i] ]] = x
    
}



x = unique(df[, c("Taxa", "TaxaID", "label"), with = FALSE])

tmp = str_remove(x$Taxa, "Taxa")
tmp = max(str_length(tmp)) - str_length(tmp)
tmp = sapply(tmp, function(x) { return(paste( rep("0", x), collapse = "" )) })




x$label = paste0(
    x$label, " (", 
    tmp,
    str_remove(x$Taxa, "Taxa"), ")"
)

# index = 

tree = read.tree(tree)

to_drop = tree$tip.label[which(!(tree$tip.label %in% df$TaxaID))]

tree_reduced <- drop.tip(tree, to_drop)

index = match(tree_reduced$tip.label, x$TaxaID)

tree_reduced$tip.label = x[index, ]$label


centralities = fread(centralities)
centralities = unique(centralities[, c("Taxa", "ClimateZone"), with = FALSE])

z = centralities[which(Taxa %in% df$Taxa), ]

z$value = 1
z$label = "present"

z = split(z, z$ClimateZone)

for(i in names(z)) {
    
    z[[i]][which(Taxa %in% df[which(ClimateZone == i), ]$Taxa), ]$value = 100
    z[[i]][which(Taxa %in% df[which(ClimateZone == i), ]$Taxa), ]$label = "Hub" 
    
    
}

z = rbindlist(z)

index = match(z$Taxa, x$Taxa)

z$Taxa = x[index, ]$label

z1 = dcast(z, Taxa ~ ClimateZone, value.var = "label")
z2 = dcast(z, Taxa ~ ClimateZone, value.var = "value", fill = 0)

z1 = setDF(z1[, 2:ncol(z1)], rownames = z1$Taxa)
z2 = as.matrix(setDF(z2[, 2:ncol(z2)], rownames = z2$Taxa))

ht = hclust(dist(t(z2), method = "euclidean"), method = "ward.D2")

z1 = z1[, ht$labels[ht$order]]

library(ggtree)

p = ggtree(tree_reduced) + 
    
    geom_tiplab(size = 3, family = "Calibri",
                align = TRUE,
                linetype = "dotted",
                geom = "label",
                label.size = NA,
                hjust = 1,
                offset = .15,
                label.padding = unit(.1, "lines"),
                linesize = 0.5) +
    
    coord_cartesian(expand = TRUE, clip = "off")

gr = gheatmap(p, z1, colnames = TRUE, width = 1.5,
         colnames_angle = 90,
         hjust = 1,
         font.size = 3, offset = .15) +
    
    scale_x_ggtree() +
    
    scale_fill_manual(
        values = c(
            "Hub" = "red3",
            "present" = "grey75"
        ),
        
        na.value = "white"
    ) +
    
    theme(
        legend.title = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
    )


ggsave(
    plot = gr, filename = "Rplot.pdf", device = cairo_pdf,
    width = 10, height = 10, units = "in"
)


df$label = NULL

x = melt(
    df, id.vars = c("ClimateZone", "Taxa", "TaxaID"),
    variable.name = "level", value.name = "label",
    variable.factor = FALSE, value.factor = FALSE
)

x[which(x$label == ""), ]$label = "unassigned"

x$label = paste0(
    str_to_lower(str_sub(x$level, 1, 1)), "__", x$label
)

y = x[, by = .(ClimateZone, level, label), .N]

y = y[, by = .(ClimateZone, level), Freq := N / sum(N)]


y$level = factor(
    y$level, 
    levels = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
)


y = y[order(ClimateZone, level, -Freq), ]


z = dcast(data = y, level + label ~ ClimateZone, value.var = "Freq", fill = 0)


library(ComplexHeatmap)
library(circlize)

m = as.matrix(setDF(z[, 3:ncol(z)], rownames = z$label))

my_col = colorRamp2(
    c(0, .1, .2, 1),
    c("white", "#0066cc", "#00cccc", "#cc0000")
)

Heatmap(
    m,
    
    border = TRUE,
    
    clustering_distance_rows = "euclidean",
    clustering_distance_columns = "euclidean",
    
    clustering_method_columns = "ward.D2",
    clustering_method_rows = "ward.D2",
    
    name = "Hubs",
    
    col = my_col,
    
    row_split = z$level,
    # column_split = 3,
    
    row_title_rot = 0,
    
    cluster_row_slices = FALSE,
    
    row_names_gp = gpar(fontsize = 7, family = "Calibri"),
    column_names_gp = gpar(fontsize = 10, family = "Calibri"),
    
    heatmap_legend_param = list(
        legend_height = unit(10, "lines")
    )
)



library(ggplot2)
library(ggsci)

library(ggh4x)
library(extrafont)

y$my_col = ifelse(
    y$Freq >= .15, y$label, "Other"
)

y[which(str_detect(y$my_col, "unassigned")), ]$my_col = "unassigned"

cols = unique(y[, c("level", "my_col"), with = FALSE])
cols = cols[order(level), ]
cols$index = rowid(cols$level)

cols$col = pal_npg("nrc")(10)[cols$index]

cols[which(cols$my_col == "unassigned"), ]$col = "grey"
cols[which(cols$my_col == "Other"), ]$col = "white"
    
cols = unique(cols[, c("my_col", "col"), with = FALSE])

my_col = cols$col

names(my_col) = cols$my_col

y$my_col = factor(
    y$my_col,
    levels = c(
        cols[which(!(cols$my_col %in% c("Other", "unassigned"))), ]$my_col,
        "Other", "unassigned"
    )
)

gr = ggplot(data = y, aes(x = level, y = Freq, fill = my_col)) +
    
    geom_bar(position="fill", stat="identity", color = "grey", linewidth = .1) +
    
    # scale_fill_igv(
    #     na.value = "white"
    # ) +
    
    scale_fill_manual(
        values = my_col
    ) +
    
    scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
    
    facet_wrap2(vars(ClimateZone), axes = "x") +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_blank(),
        
        panel.grid = element_blank(),
        
        strip.text = element_text(face = "bold"),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    guides(
        fill = guide_legend(
            ncol = 4,
            label.theme = element_text(size = 8)
        )
    )

ggsave(
    plot = gr, filename = "Rplot2.pdf", device = cairo_pdf,
    width = 10, height = 10, units = "in"
)



