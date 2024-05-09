


rm(list = ls())
gc()

# Figure 4A ==============================================================

library(data.table)
library(stringr)

library(ggh4x)
library(ggplot2)
library(ggforce)

library(ggpubr)
library(paletteer)

library(extrafont)

a = fread("emp-soil-analysis-clean-release1/globalProps-bootstrap.txt")

an = fread("draft/Supplementary Table 1.csv")

a$nTaxa = NULL
a$Run   = NULL

a = melt(
    a, id.vars = "ClimateZone", 
    variable.factor = FALSE, value.factor = FALSE
)

a = merge(a, an, by.x = "ClimateZone", by.y = "Code")


a$variable = factor(
    a$variable, 
    levels = c(
        "Average path length",
        "Modularity",
        "Density",
        "Transitivity"
    )
)

gr1 = ggplot(data = a, aes(x = ClimateZone, y = value)) +
    
    stat_boxplot(
        geom = 'errorbar', width = 0.25, color = "black", linewidth = .1
    ) +
    
    geom_boxplot(
        aes(fill = Group), outlier.shape = NA, linewidth = .1
    ) +
    
    geom_point(
        shape = 20, color = "grey10", fill = "grey10",
        size = .5,
        position = position_jitternormal(sd_y = 0, sd_x = .05)
    ) +
    
    geom_hline(data = a[, by = variable, .(value = mean(value))],
               mapping = aes(yintercept = value),
               linetype = 2, linewidth = 1, color = "yellow3") +
    
    stat_compare_means(
        label = "p.signif", method = "wilcox.test", ref.group = ".all.", 
        hide.ns = TRUE,
        symnum.args = list(
            cutpoints = c(0, 0.05, Inf), 
            symbols = c("*", "ns")
        )
    ) +
    
    scale_fill_manual(
        values = paletteer_d("ggthemes::Color_Blind"),
        guide = guide_legend(
            title.position = "top",
            title.theme = element_text(size = 11, family = "Calibri"),
            label.theme = element_text(size = 11, family = "Calibri")
        )
    ) +
    
    facet_wrap2(vars(variable), scales = "free_y", nrow = 1, axes = "all") +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "bottom",
        legend.justification = c(0, 1),
        
        strip.text = element_text(face = "bold", size = 11),
        axis.title = element_text(size = 11),
        
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle = 90, hjust = 1, vjust = .5),
        
        axis.line = element_line(),
        axis.ticks = element_line(),
        
        panel.grid = element_blank(), # element_line(linetype = "dashed"),
        
        axis.title.y = element_blank()
    )


# Figure 4B ==========================================

rm(a, an)

# load libraries -----------------------------

library(data.table)
library(stringr)

library(treeio)

library(ggplot2)
library(extrafont)

library(ggsci)

sample_map   <- "emp-soil-analysis-clean-release1/sample-metadata.Soil (non-saline).txt"
taxa_map     <- "emp-soil-analysis-clean-release1/taxonomy-table.Soil (non-saline).txt"
centralities <- "emp-soil-analysis-clean-release1/centralities-bootstrap.txt"
hubs         <- "emp-soil-analysis-clean-release1/hubs1.csv"
tree         <- "data-raw/emp90.5000_1000_rxbl_placement_pruned75.tog.tre"
workdir      <- dirname(sample_map)

climate_info <- "draft/Supplementary Table 1.csv"


df = fread(hubs)

x = fread(taxa_map)

index = match(df$Taxa, x$TaxaIDabv)

df$TaxaID = x[index, ]$TaxaID

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

p = ggtree(tree_reduced, linewidth = .1) + 
    
    # geom_tiplab(size = 3, family = "Calibri",
    #             align = TRUE,
    #             linetype = "dotted",
    #             geom = "label",
    #             label.size = NA,
    #             hjust = 1,
    #             offset = .15,
    #             label.padding = unit(.1, "lines"),
    #             linesize = 0.5) +
    
    coord_cartesian(expand = TRUE, clip = "off")

gr2 = gheatmap(p, z1, colnames = TRUE, width = 1.5,
              colnames_angle = 90,
              hjust = 1,
              font.size = 3) +
    
    scale_x_ggtree() +
    
    scale_fill_manual(
        values = c(
            "Hub" = "red3",
            "present" = "grey75"
        ),
        
        na.value = "white",
        guide = guide_legend(
            title = "Annotation",
            title.position = "top"
        )
    ) +
    
    theme(
        legend.position = "bottom",
        legend.justification = c(0, 1),
        plot.margin = margin(20, 20, 20, 20)
    )


rm(centralities, ht, p, tree, tree_reduced, x, z, z1, z2)
rm(climate_info, hubs, i, index, sample_map, taxa_map, tmp, to_drop)



# Figure 4C ----------------------------------------- 

df$Taxa = NULL
df$TaxaID = NULL
df$label = NULL

df = melt(
    df, id.vars = "ClimateZone",
    variable.factor = FALSE, value.factor = FALSE
)

df$value = ifelse(df$value == "", "unassigned", df$value)

df = df[, by = .(ClimateZone, variable, value), .N]
df = df[, by = .(ClimateZone, variable), Freq := N / sum(N)]

df$variable = factor(df$variable, levels = lvl)

df$value = paste0(
    str_to_lower(str_sub(df$variable, 1, 1)), "__",
    df$value
)

length(
    unique(
        df[which(Freq >= .15)]$value
    )
)

df$lbl = ifelse(
    df$Freq >= .15, df$value, "Other"
)

df = df[order(variable, -Freq), ]

df$lbl = ifelse(str_detect(df$lbl, "unassigned"), "unassigned", df$lbl)

vlvl = c(
    unique(df$lbl)[which(!(unique(df$lbl) %in% c("unassigned", "Other")))],
    "Other",
    "unassigned"
)

df$lbl = factor(
    df$lbl, levels = vlvl
)

my_col = str_split(vlvl, "_", simplify = TRUE)[,1]
my_col = rowid(my_col)

my_col = paletteer_d("ggthemes::Red_Blue_Brown")[my_col]
my_col[(length(my_col) - 1):length(my_col)] = c("white", "grey50")

gr3 = ggplot(data = df, aes(x = variable, y = Freq)) +
    
    geom_col(aes(fill = lbl), position = "fill", color = "grey", linewidth = .25) +
    
    scale_fill_manual(
        values = my_col,
        guide = guide_legend(ncol = 6, 
                             label.theme = element_text(size = 11, family = "Calibri"))
    ) +
    
    scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
    scale_x_discrete(expand = c(0, 0)) +
    
    facet_wrap(vars(ClimateZone), ncol = 4) +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "bottom",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        
        panel.spacing = unit(1, "lines"),
        
        strip.text = element_text(face = "bold", size = 11),
        
        panel.grid.minor = element_blank()
    )



# Figure 4D --------------------------------------------------


# df = fread("emp-soil-analysis-clean-sub5k/negative.csv")
# 
# x0 = df[which(level == "Kingdom")]
# 
# x0$level = x0$from = x0$to = NULL
# 
# x0 = melt(
#     x0, id.vars = "ClimateZone", 
#     value.factor = FALSE, variable.factor = FALSE
# )
# 
# x0 = x0[order(ClimateZone, variable)]
# x0 = x0[, by = ClimateZone, ymax := cumsum(value)]
# 
# x0 = x0[, by = ClimateZone, ymin := c(0, head(ymax, -1))]
# 
# 
# # Make the plot
# gr4 = ggplot(x0, 
#        aes(
#            ymax = ymax, ymin = ymin, 
#            xmax = 4, xmin = 3, 
#            fill = variable
#        )) +
#     
#     geom_rect() +
#     
#     scale_fill_manual(
#         values = c(
#             "Negative co-occurrence links" = "red3",
#             "Positive co-occurrence links" = "#1170AAFF"
#         ),
#         
#         guide = guide_legend(
#             label.theme = element_text(size = 11, family = "Calibri")
#         )
#     ) +
#     
#     # scale_fill_manual(values = paletteer_d("ggthemes::Color_Blind")) +
#     
#     coord_polar(theta = "y") +
#     
#     facet_wrap2(vars(ClimateZone), nrow = 1) +
#     
#     xlim(c(2, 4)) +
#     
#     theme_void(base_family = "Calibri") +
#     
#     theme(
#         strip.text = element_text(face = "bold", size = 11),
#         
#         legend.position = "bottom",
#         legend.justification = c(0, 1),
#         plot.tag = element_text(face = "bold"),
#         legend.title = element_blank()
#     )


# patchwork ================================

library(patchwork)

# multi = gr1 / (gr2 | gr3) / gr4 +
#     
#     plot_annotation(tag_levels = 'A') +
#     plot_layout(heights = c(1.5, 2.5, .5)) &
#     
#     theme(
#         plot.margin = margin(10, 10, 10, 10)
#     )

multi = gr1 / (gr2 | gr3) +
    
    plot_annotation(tag_levels = 'A') +
    plot_layout(heights = c(1, 2)) &
    
    theme(
        plot.margin = margin(10, 10, 10, 10)
    )


ggsave(
    plot = multi, filename = "Fig4.pdf", device = cairo_pdf,
    width = 14, height = 14, units = "in"
)

ggsave(
    plot = multi, filename = "Fig4.png", dpi = 600,
    width = 14, height = 14, units = "in"
)

# ggsave(
#     plot = gr4, filename = "Fig4b.jpeg",
#     width = 14, height = 7, units = "in"
# )




























