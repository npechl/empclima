


rm(list = ls())
gc()

# Figure 4A ==============================================================

library(data.table)
library(stringr)


library(ggplot2)
library(ggforce)
library(paletteer)

library(extrafont)

a = fread("emp-soil-analysis-clean-sub5k/globalProps-bootstrap.txt")

an = fread("climate-classification-info.csv")

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
        geom = 'errorbar', width = 0.25, color = "black", linewidth = .5
    ) +
    
    geom_point(
        shape = 21, color = "black", fill = "grey10",
        size = 1, stroke = .25,
        position = position_jitternormal(sd_y = 0, sd_x = .05)
    ) +
    
    geom_boxplot(
        aes(fill = Group), alpha = .75,
        outlier.shape = NA
    ) +
    
    scale_fill_manual(
        values = paletteer_d("ggthemes::Color_Blind"),
        guide = guide_legend(title.position = "top")
    ) +
    
    facet_wrap(vars(variable), scales = "free_y", nrow = 1) +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "bottom",
        legend.justification = c(0, 1),
        
        strip.text = element_text(face = "bold"),
        
        axis.line = element_line(),
        axis.ticks = element_line(),
        
        panel.grid = element_line(linetype = "dashed"),
        
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

sample_map   <- "emp-soil-analysis-clean-sub5k/sample-metadata.Soil (non-saline).txt"
taxa_map     <- "emp-soil-analysis-clean-sub5k/taxonomy-table.Soil (non-saline).txt"
centralities <- "emp-soil-analysis-clean-sub5k/centralities-bootstrap.txt"
hubs         <- "emp-soil-analysis-clean-sub5k/hubs-bootstrap.txt"
tree         <- "emp90.5000_1000_rxbl_placement_pruned75.tog.tre"
workdir      <- dirname(sample_map)

climate_info <- "climate-classification-info.csv"


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

gr2 = gheatmap(p, z1, colnames = TRUE, width = 1.5,
              colnames_angle = 90,
              hjust = 1,
              font.size = 3, offset = .15) +
    
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
        df[which(Freq >= .5)]$value
    )
)

df$lbl = ifelse(
    df$Freq >= .25, df$value, "Other"
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
    
    geom_col(aes(fill = lbl), position = "fill", color = "grey") +
    
    scale_fill_manual(
        values = my_col,
        guide = guide_legend(ncol = 6)
    ) +
    
    scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
    scale_x_discrete(expand = c(0, 0)) +
    
    facet_wrap(vars(ClimateZone), ncol = 3) +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "bottom",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        
        panel.spacing = unit(1, "lines"),
        
        strip.text = element_text(face = "bold"),
        
        panel.grid.minor = element_blank()
    )



# Figure 4D --------------------------------------------------


df = fread("emp-soil-analysis-clean-sub5k/negative.csv")


# patchwork ================================


multi = gr1 / (gr2 | gr3) +
    plot_layout(heights = c(1, 2)) &
    
    theme(
        plot.margin = margin(10, 10, 10, 10)
    )


ggsave(
    plot = multi, filename = "Fig4.jpeg",
    width = 14, height = 14, units = "in"
)































