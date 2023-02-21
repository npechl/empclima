


rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(extrafont)

library(ggsci)

taxa_map     <- c(
    "emp-soil-analysis-sub2k/taxonomy-table.Soil (non-saline).txt",
    "emp-soil-analysis-sub5k/taxonomy-table.Soil (non-saline).txt",
    "emp-soil-analysis-sub10k/taxonomy-table.Soil (non-saline).txt"
)

hubs         <- c(
    "emp-soil-analysis-sub2k/hubs.txt",
    "emp-soil-analysis-sub5k/hubs.txt",
    "emp-soil-analysis-sub10k/hubs.txt"
)

climate_info <- "climate-classification-info.csv"


df = list()
climate_info <- fread(climate_info)

for(i in seq_len(length(hubs))) {
    
    d <- dirname(hubs[i])
    
    tmp = fread(paste0(d, "/hubs.txt"))
    x = fread(paste0(d, "/taxonomy-table.Soil (non-saline).txt"))
    
    x = x[, 1:2]
    
    tmp = merge(tmp, x, by.x = "Taxa", by.y = "TaxaIDabv", all.x = TRUE)
    
    df[[str_remove_all(d, "emp|soil|analysis|\\-")]] <- tmp
    
    
}


plot.df = lapply(df, function(x) { return(unique(x$TaxaID)) })


library(ggVennDiagram)

venn <- Venn(plot.df)
data <- process_data(venn)

gr = ggplot() +
    geom_sf(fill = "white", data = venn_region(data)) +
    geom_sf(size = 2, color = "black", data = venn_setedge(data), show.legend = F) +
    geom_sf_text(aes(label = name), data = venn_setlabel(data)) +
    geom_sf_text(aes(label=count), fontface = "bold", data = venn_region(data)) +
    theme_void()


ggsave(
    plot = gr, filename = paste0("hubs-stats.pdf"),
    width = 8, height = 8, device = cairo_pdf
)

ggsave(
    plot = gr, filename = paste0("hubs-stats.jpeg"),
    width = 8, height = 8
)



# taxonomy     = fread(taxa_map)
# climate_info = fread(climate_info)
# 
# df = merge(df, taxonomy, by.x = "Taxa", by.y = "TaxaIDabv", all.x = TRUE)
# 
# df$TaxaID = NULL
# 
# 
# for(i in c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) {
#     
#     df[[i]] = str_split(df[[i]], "\\__", simplify = TRUE)[,2]
#     df[[i]] = str_remove_all(df[[i]], "\\[|\\]")
#     
# }
# 
# rm(i)

stats = df[, by = Taxa, .(N.of.ClimateZone = .N)]
stats = stats[, by = N.of.ClimateZone, .(N.of.Taxa = .N)]

stats$label = paste0(stats$N.of.Taxa, " Taxa")

gr = ggplot(data = stats, aes(x = N.of.ClimateZone, y = N.of.Taxa)) +
    
    geom_col(width = .5, fill = "grey50", color = "grey10") +
    
    geom_text(aes(label = N.of.Taxa), nudge_y = 0.75, family = "Calibri") +
    
    scale_x_continuous(breaks = c(1:12)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        panel.grid.minor = element_blank(),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = .3, linetype = "dashed"),
        
        axis.line = element_line(linewidth = .3),
        axis.ticks = element_line(linewidth = .3),
        
        plot.margin = margin(10, 10, 10, 10)
    ) +
    
    labs(x = "No. of Climate Zones", y = "No. of Taxa")


ggsave(
    plot = gr, filename = paste0(workdir, "/hubs-stats.pdf"),
    width = 8, height = 8, device = cairo_pdf
)

ggsave(
    plot = gr, filename = paste0(workdir, "/hubs-stats.svg"),
    width = 8, height = 8
)


plot_df = fread(centralities)
plot_df = unique(plot_df[, c("ClimateZone", "Taxa"), with = FALSE])
plot_df$value = 1

plot_df = plot_df[which(Taxa %in% df$Taxa), ]

df$index = paste0(df$ClimateZone, ";", df$Taxa)
plot_df$index = paste0(plot_df$ClimateZone, ";", plot_df$Taxa)

plot_df[which(plot_df$index %in% df$index), ]$value = 10

mm = dcast(plot_df, Taxa ~ ClimateZone, value.var = "value", fill = 0)
mm = as.matrix(setDF(mm[, 2:ncol(mm)], rownames = mm$Taxa))

taxaOrder = hclust(dist(mm, method = "euclidean"), method = "average")
zoneOrder = hclust(dist(t(mm), method = "euclidean"), method = "average")


taxaOrder = taxaOrder$labels[taxaOrder$order]
zoneOrder = zoneOrder$labels[zoneOrder$order]

# stats = plot_df[, by = ClimateZone, .N]
# stats = stats[order(N), ]

# plot_df = df
plot_df$ClimateZone = factor(plot_df$ClimateZone, levels = zoneOrder)
plot_df$Taxa = factor(plot_df$Taxa, levels = taxaOrder)

plot_df$hub = ifelse(plot_df$value == 1, "no", "yes")

nclimatezones = length(unique(plot_df$ClimateZone))

ann = plot_df[, by = Taxa, .N]
ann = ann[which(N == nclimatezones), ]
ann = plot_df[which(Taxa %in% ann$Taxa), ]
ann$index = as.numeric(ann$ClimateZone)
ann = ann[, by = Taxa, .(
    y1 = ClimateZone[which.min(index)],
    y2 = ClimateZone[which.max(index)]
)]

gr = ggplot(data = plot_df, aes(x = Taxa, y = ClimateZone)) +
    
    # geom_segment(data = ann, aes(x = Taxa, xend = Taxa,
    #                              y = y1, yend = y2)) +

    geom_point(aes(fill = hub, size = hub),
               shape = 21, stroke = .05, color = "grey10") +
    
    scale_fill_manual(
        values = c(
            "yes" = "#d91c2c",
            "no"  = "grey"
        )
    ) +
    
    scale_size_manual(
        values = c(
            "yes" = 2.5,
            "no" = 1.5
        )
    ) +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "right",
        
        panel.border = element_rect(linewidth = .3, color = "grey", fill = NA),
        panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linewidth = .3, linetype = "dashed"),
        panel.grid.major = element_blank(),
        
        axis.ticks = element_line(linewidth = .3, color = "grey"),
        
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        
        plot.margin = margin(10, 10, 10, 10)
    ) +
    
    guides(fill = guide_legend(title.position = "top"),
           size = guide_legend(title.position = "top")) +
    
    labs(x = "Hub Taxa", y = "Climate zone")

ggsave(
    plot = gr, filename = paste0(workdir, "/hubs-grouping.pdf"),
    width = 12, height = 6, device = cairo_pdf
)    

ggsave(
    plot = gr, filename = paste0(workdir, "/hubs-grouping.svg"),
    width = 12, height = 6
)  
    



