
rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

# library(ggplot2)
# library(ggsci)
# library(ggrepel)
# library(ggtext)
# library(extrafont)

# list of inputs ------------------------------------

sample_map      <- "emp-soil-analysis-clean-sub10k/sample-metadata.Soil (non-saline).txt"
abundance_table <- "emp-soil-analysis-clean-sub10k/abundance-table.Soil (non-saline).txt"
workdir         <- dirname(sample_map)


s0 <- fread(sample_map)
df <- fread(abundance_table)

rm(abundance_table, sample_map)
gc()

stats = list()

for(i in unique(s0$ClimateZone)) {
    
    s1 = s0[which(ClimateZone == i), ]
    
    tmp = df[, s1$SampleIDabv, with = FALSE]
    
    index = which(rowSums(tmp) != 0)
    
    
    stats[[i]] = data.table(
        "ClimateZone" = i,
        "Group"       = unique(s1$Group),
        "No of Samples"    = nrow(s1),
        "No of ESVs"       = length(index)
    )
    
    
}

stats = rbindlist(stats)

rm(tmp, index, i, s1)

stats$Ratio = stats$`No of ESVs` / stats$`No of Samples`

stats = stats[order(Ratio), ]

fwrite(
    stats, paste0(workdir, "/ESVdistribution.csv"),
    row.names = FALSE, quote = TRUE, sep = ","
)


# ggplot(data = stats, aes(x = `No of Samples`, y = Ratio)) + 
#     geom_point(aes(size = `No of ESVs`)) +
#     geom_label_repel(aes(label = ClimateZone))
#
# stats$lbl = paste0(
#     stats$ClimateZone, "\n",
#     "No. of samples: ", stats$nSamples, "\n",
#     "No. of taxa: ", stats$nTaxa
# )

# gr = ggplot(data = stats, aes(x = nSamples, y = nTaxa)) +
#     
#     geom_point(aes(fill = Group), size = 4, shape = 21, stroke = .25, color = "grey10") +
# 
#     
#     # geom_richtext(
#     #     aes(label = lbl), size = 3.5,
#     #     
#     #     label.size = NA,
#     #     fill = alpha("white", alpha = .5),
#     #     hjust = 0
#     # ) +
#         
#     geom_label_repel(
#         aes(label = lbl),
#         segment.linetype = "dotted",
#         
#         min.segment.length = 0,
#         
#         hjust = 0,
# 
#         label.size = NA,
#         fill = alpha("white", alpha = .5),
# 
#         family = "Calibri",
# 
#         size = 3.5
#     ) +
#     
#     scale_x_continuous(limits = c(0, 65)) +
#     scale_y_continuous(labels = scales::comma) +
#     scale_fill_npg() +
#     scale_size_continuous(guide = "none", range = c(2, 6)) +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         legend.position = "bottom",
#         legend.title = element_blank(),
#         
#         panel.grid = element_line(linewidth = .3, linetype = "dashed"),
#         axis.ticks = element_line(linewidth = .3),
#         
#         panel.border = element_rect(linewidth = .3, fill = NA),
#         
#         plot.margin = margin(10, 10, 10, 10)
#     ) + 
#     
#     labs(y = "No. of Taxa", x = "No. of Samples")
#     
# ggsave(
#     plot = gr,
#     filename = paste0(workdir, "/nTaxa.pdf"),
#     width = 8, height = 8, units = "in",
#     device = cairo_pdf
# )
# 
# 
# ggsave(
#     plot = gr,
#     filename = paste0(workdir, "/nTaxa.svg"),
#     width = 8, height = 8, units = "in"
# )



# stats = stats[order(nTaxa), ]
# 
# stats$ClimateZone = factor(
#     stats$ClimateZone,
#     levels = stats$ClimateZone
# )
# 
# stats$xlabel = paste0(
#     "<b>", stats$ClimateZone, "</b>", "<br>", 
#     "(N = ", stats$nSamples, ")"
# )
# 
# gr = ggplot(data = stats, aes(x = ClimateZone, y = nTaxa)) +
#     
#     # geom_line(group = 1) +
#     
#     geom_point(aes(size = nSamples), shape = 21,
#                fill = "#e32636", color = "grey10",
#                stroke = .25) +
#     
#     scale_x_discrete(
#         breaks = stats$ClimateZone,
#         labels = stats$xlabel
#     ) +
#     
#     scale_y_continuous(
#         labels = function(x) {
#             format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)
#         }
#     ) +
#     
#     scale_size_binned(range = c(2, 8)) +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         legend.position = "none",
#         
#         axis.text.x = element_markdown(),
#         
#         panel.grid.minor = element_line(linewidth = .3),
#         panel.grid.major = element_line(linewidth = .3),
#         
#         panel.border = element_rect(linewidth = .3, color = "grey", fill = NA),
#         
#         axis.ticks = element_line(color = "grey", linewidth = .3),
#         
#         plot.margin = margin(10, 10, 10, 10)
#     ) + 
#     
#     labs(y = "No. of Taxa")





# message(
#     c(
#         "Minimum No. of Taxa: ",min(stats$nTaxa)
#     ) 
# )



