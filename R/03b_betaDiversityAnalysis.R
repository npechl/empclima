

rm(list = ls())
gc()


# load libraries -----------------------------

library(data.table)
library(stringr)

library(vegan)

# list of inputs ------------------------------------

sample_map      <- "emp-soil-analysis-clean-sub10k/sample-metadata.Soil (non-saline).txt"
abundance_table <- "emp-soil-analysis-clean-sub10k/abundance-table.Soil (non-saline).txt"
workdir         <- dirname(sample_map)

s0 = fread(sample_map)

d0 = fread(abundance_table)

d1 = transpose(d0, keep.names = "sample", make.names = "TaxaIDabv")
d1 = setDF(d1[, 2:ncol(d1)], rownames = d1$sample)


d1.bray = vegdist(x = d1, method = "bray")

mds      <- metaMDS(d1.bray, distance = "bray", trymax = 100)
mds_data <- as.data.frame(mds$points)
mds_data <- setDT(mds_data, keep.rownames = "SampleID")
mds_data <- merge(s0, mds_data, by.y = "SampleID", by.x = "SampleIDabv")


fwrite(
    mds_data, sample_map,
    row.names = FALSE, quote = FALSE, sep = "\t"
)


to_write = adonis2(d1 ~ ClimateZone, data = s0, method = "bray") |>
    
    as.data.frame() |>
    setDT(keep.rownames = "ID")

fwrite(
    to_write, paste0(workdir, "/diversity2.csv"),
    row.names = FALSE, quote = TRUE, sep = ","
)


# pc.bray = cmdscale(d1.bray, k = 2)
# colnames(pc.bray) = c("PC1", "PC2")
# pc.bray = setDT(as.data.frame(pc.bray), keep.rownames = "sample")
# pc.bray = merge(s0, pc.bray, by.x = "SampleIDabv", by.y = "sample", all.x = TRUE)
    
    
# library(ggplot2)
# library(ggrepel)
# library(ggsci)
# 
# library(extrafont)
# 
# library(ggdensity)
# library(ggh4x)
# 
# library(patchwork)
# 
# gr1 = ggplot(data = mds_data, aes(x = MDS1, y = MDS2)) + 
#     
#     geom_point(
#         aes(fill = Group),
#         color = "white", 
#         stroke = .25, shape = 21, size = 1.5
#     ) +
#     
#     # geom_text_repel(
#     #     aes(label = ClimateZone), 
#     #     max.overlaps = Inf,
#     #     size = 3,
#     #     segment.linetype = "dotted",
#     #     segment.size = .35,
#     #     
#     #     family = "Calibri"
#     # ) +
#     
#     # scale_fill_viridis_d(option = "magma") +
#     
#     # scale_fill_grey() +
#     
#     scale_fill_npg() +
#     
#     scale_color_npg() +
#     
#     coord_cartesian(expand = TRUE, clip = "off") +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         # legend.position = "bottom",
#         axis.line = element_line(linewidth = .3),
#         axis.ticks = element_line(linewidth = .3),
#         
#         panel.grid = element_blank(),
#         
#         plot.margin = margin(10, 10, 10, 10)
#     ) +
#     
#     guides(
#         fill = guide_legend(title.position = "top"),
#         color = guide_legend(title.position = "top"),
#         alpha = guide_legend(title.position = "top")
#     )
# 
# 
# 
# 
# gr2 = ggplot(data = mds_data, aes(x = MDS1, y = MDS2)) + 
#     
#     geom_hdr(aes(fill = Group), show.legend = FALSE) +
#     
#     geom_point(
#         aes(fill = Group),
#         color = "white", 
#         stroke = .25, shape = 21, size = 1.5
#     ) +
#     
#     scale_fill_npg() +
#     scale_color_npg() + 
#     
#     # facet_wrap(vars(Group), nrow = 1) +
#     
#     # scale_y_continuous(expand = c(0, 0)) +
#     # scale_x_continuous(expand = c(0, 0)) +
#     
#     coord_cartesian(expand = TRUE, clip = "on") +
#     
#     facet_wrap2(vars(ClimateZone), nrow = 3, axes = "all") +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         # legend.position = "bottom",
#         strip.text = element_text(face = "bold"),
#         
#         panel.grid = element_blank(),
#         
#         axis.line = element_line(linewidth = .3),
#         axis.ticks = element_line(linewidth = .3),
#         axis.text = element_text(size = 6),
#         
#         plot.margin = margin(10, 10, 10, 10)
#     ) +
# 
#     guides(
#         fill = guide_legend(title.position = "top"),
#         color = guide_legend(title.position = "top"),
#         alpha = guide_legend(title.position = "top")
#     )
# 
# 
# 
# 
# 
# multi = gr1 + gr2 + 
#     plot_layout(guides = 'collect') &
#     theme(
#         legend.position = "bottom"
#     ) 
# 
# ggsave(
#     plot = multi,
#     filename = paste0(workdir, "/Beta-diversity.pdf"),
#     width = 12, height = 6, device = cairo_pdf,
#     units = "in"
# )
# 
# ggsave(
#     plot = multi,
#     filename = paste0(workdir, "/Beta-diversity.svg"),
#     width = 12, height = 6,
#     units = "in"
# )









