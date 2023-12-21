


rm(list = ls())
gc()



library(data.table)
library(stringr)

library(extrafont)

a = fread("emp-soil-analysis-clean-release1-v2/links1.csv")
a = a[which(level == "Phylum")]

b = fread("emp-soil-analysis-clean-release1-v2/hypergeometric1.csv")
b = b[which(p.adj <= 0.05 & level == "Phylum"), ]

cl = fread("draft/Supplementary Table 1.csv")

a$key = paste0(a$from, ";", a$to) |> 
    str_split(pattern = ";") |> 
    lapply(sort) |> 
    lapply(paste, collapse = " - ") |> 
    unlist()

a$key = paste0(a$ClimateZone, "; ", a$key)

b$key = paste0(b$from, ";", b$to) |> 
    str_split(pattern = ";") |> 
    lapply(sort) |> 
    lapply(paste, collapse = " - ") |> 
    unlist()

b$key = paste0(b$ClimateZone, "; ", b$key)


a$ann = ifelse(
    a$key %in% b$key,
    "Sign. link",
    "Other"
)

index = match(a$ClimateZone, cl$Code)

a$Group = cl[index]$Group

# 2023-11-23 circlize try ------------------------

library(circlize)

plot_circos <- function(x, zone) {
    
    chordDiagramFromDataFrame(
        df = a0,
        col = a0$col,
        
        grid.col = "grey10",
        
        annotationTrack = c("grid"),
        annotationTrackHeight = c(0.01),
        
        preAllocateTracks = list(
            track.height = .1
        )
    )
    
    circos.track(track.index = 1, panel.fun = function(x, y) {
        
        circos.text(
            CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
            facing = "downward", niceFacing = TRUE, adj = c(0, 0.5),
            cex = .8
        )
        
    }, bg.border = NA)
    
    title(zone)
    
}


par(mfrow = c(4, 5))


for(i in unique(a$ClimateZone)) {
    
    x = a[which(ClimateZone == i)]
    
    a0 = x[, c("from", "to", "N"), with = FALSE]
    
    colnames(a0) = c("from", "to", "value")
    
    a0$col = ifelse(x$ann == "Sign. link", "red","grey")
    
    plot_circos(a0, i)
    
}


circos.clear()


# df = a[which(a$key %in% b$key), ]

library(ggplot2)
library(ggh4x)
library(extrafont)

gr = ggplot() +
    
    # geom_point(data = a, aes(x = from, y = to, size = Freq), size = .1, color = "grey10") +
    
    geom_point(data = a[which(ann != "Sign. link")], aes(x = from, y = to, size = Freq, fill = ann), 
               color = "white", shape = 21, stroke = .25) +
    
    geom_point(data = a[which(ann == "Sign. link")], aes(x = from, y = to, size = Freq, fill = ann), 
               color = "white", shape = 21, stroke = .25) +
    
    scale_fill_manual(
        values = c(
            "Sign. link" = "red3",
            "Other"      = alpha("grey", alpha = 1)
        ),
        
        guide = guide_legend(override.aes = list(size = 4, color = "black"))
    ) +
    
    # geom_point(data = df, aes(x = from, y = to, size = Freq),
    #            shape = 21, fill = "#B60A1C", color = "white", stroke = .1) +
    
    scale_size_continuous(
        range = c(1.5, 7),
        labels = scales::percent, 
        guide = guide_legend(override.aes = list(color = "black"))
    ) +
    
    facet_wrap2(vars(ClimateZone), ncol = 5) +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        axis.title = element_blank(),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .25),
        
        strip.text = element_text(face = "bold"),
        
        # panel.border = element_rect(fill = NA),
        
        plot.margin = margin(10, 10, 10, 10)
    )


ggsave(
    plot = gr, filename = "Fig3_v2.pdf", device = cairo_pdf,
    width = 14, height = 12, units = "in"
)

# part 2 -----------------------------------

# library(ggrepel)
# 
# df = fread("emp-soil-analysis-clean-release1/nodes-edges-counts.csv")
# 
# m = fread("draft/Supplementary Table 1.csv")
# 
# 
# df = merge(df, m, by.x = "Climate zone", by.y = "Code")
# 
# ggplot(data = df, aes(x = `No. of nodes`, y = `No. of edges`)) +
#     
#     geom_point()

# Create a graph of the flare class system ----------------------

# rm(list = ls())
# gc()
# 
# library(data.table)
# library(stringr)
# library(tidygraph)
# library(ggraph)
# library(ggh4x)
# 
# library(extrafont)
# 
# b = fread("emp-soil-analysis-clean-release1/hypergeometric1.csv")
# b = b[which(level != "Kingdom"), ]
# 
# b$level = factor(
#     b$level, 
#     levels = c("Phylum", "Class", "Family", "Genus", "Order", "Species")
# )
# 
# b = b[order(level, ClimateZone, from, to)]
# 
# df = b[, c("ClimateZone", "level", "from", "to", "p.adj"), with = FALSE]
# 
# # df$from = paste0(df$ClimateZone, ";", df$from)
# # df$to   = paste0(df$ClimateZone, ";", df$to)
# # 
# lbl = df[which(p.adj <= .05)] 
# lbl = lbl |> split(lbl$level)
# 
# 
# create_con <- function(graph_df, facet_n) {
#     
#     graph <- graph_df |> as_tbl_graph()
#     
#     
#     gr = ggraph(graph = graph, layout = 'linear', circular = TRUE) + 
#         
#         # geom_edge_arc(aes(filter = (p.adj > 0.05)), edge_colour = "grey75",
#         #               edge_width = .25, edge_alpha = .3) +
#         
#         geom_edge_arc(edge_colour = "red",
#                       edge_width = .75) + 
#         
#         
#         geom_node_point(
#             shape = 21, size = 2.5, stroke = .25,
#             fill = "black", color = "white"
#         ) +
#         
#         geom_node_label(
#             aes(label = name), repel = TRUE,
#             label.size = NA, fill = alpha("white", alpha = .75),
#             size = 4, family = "Calibri", max.overlaps = Inf
#         ) +
#         
#         facet_edges(
#             vars(ClimateZone),
#             nrow = facet_n
#         ) +
#         
#         # scale_x_continuous(limits = c(-2, 2)) +
#         # scale_y_continuous(limits = c(-2, 2)) +
#         
#         coord_fixed() +
#         
#         theme_minimal(base_family = "Calibri") +
#         
#         theme(
#             legend.position = "bottom",
#             strip.text = element_text(face = "bold", size = 11),
#             panel.spacing = unit(2, 'lines'),
#             panel.grid = element_line(linewidth = .3, linetype = "dashed"),
#             
#             axis.title = element_blank(),
#             axis.text = element_blank(),
#             
#             plot.margin = margin(20, 20, 20, 20)
#         )
#     
#     
#     return(gr)
#     
# }
# 
# 
# gr = list(
#     "Phylum"  = create_con(lbl$Phylum, 3),
#     "Class"   = create_con(lbl$Class, 3),
#     "Family"  = create_con(lbl$Family, 2),
#     "Genus"   = create_con(lbl$Genus, 3),
#     "Order"   = create_con(lbl$Order, 2),
#     "Species" = create_con(lbl$Species, 2)
# )
# 
# 
# ggsave(
#     plot = gr$Phylum, filename = "Fig3.pdf", device = cairo_pdf,
#     width = 16, height = 8, units = "in"
# )

# ggsave(
#     plot = gr$Class, filename = "Fig3-class.jpeg",
#     width = 16, height = 8, units = "in", dpi = 600
# )
#
# ggsave(
#     plot = gr$Family, filename = "Fig3-family.jpeg",
#     width = 16, height = 8, units = "in", dpi = 600
# )
# 
# ggsave(
#     plot = gr$Genus, filename = "Fig3-genus.jpeg",
#     width = 20, height = 10, units = "in", dpi = 600
# )
# 
# ggsave(
#     plot = gr$Order, filename = "Fig3-order.jpeg",
#     width = 16, height = 8, units = "in", dpi = 600
# )
# 
# ggsave(
#     plot = gr$Species, filename = "Fig3-species.jpeg",
#     width = 16, height = 8, units = "in", dpi = 600
# )
