


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(geosphere)
library(vegan)

# library(ggplot2)
# library(extrafont)

sample_map      <- "emp-soil-analysis-clean-sub10k/sample-metadata.Soil (non-saline).txt"
abundance_table <- "emp-soil-analysis-clean-sub10k/abundance-table.Soil (non-saline).txt"
workdir         <- dirname(sample_map)

s0 = fread(sample_map)
d0 = fread(abundance_table)

d1 = transpose(d0, keep.names = "sample", make.names = "TaxaIDabv")
d1 = setDF(d1[, 2:ncol(d1)], rownames = d1$sample)

d1.bray = d1 |>
    vegdist(method = "bray") |>
    as.matrix() |>
    as.data.frame() |>
    setDT(keep.rownames = "P1")

prs = melt(
    d1.bray, id.vars = "P1", 
    variable.name = "P2", value.name = "BrayCurtis",
    variable.factor = FALSE, value.factor = FALSE
)

# prs = t(combn(s0$SampleIDabv, 2))

index = match(prs$P1, s0$SampleIDabv)

prs$lon.p1 = s0[index, ]$longitude_deg
prs$lat.p1 = s0[index, ]$latitude_deg


index = match(prs$P2, s0$SampleIDabv)

prs$lon.p2 = s0[index, ]$longitude_deg
prs$lat.p2 = s0[index, ]$latitude_deg

prs = prs[which(prs$P1 != prs$P2), ]

prs$geodistance = distVincentyEllipsoid(
    p1 = as.matrix(prs[, c("lon.p1", "lat.p1"), with = FALSE]), 
    p2 = as.matrix(prs[, c("lon.p2", "lat.p2"), with = FALSE])
)


fwrite(
    prs, paste0(workdir, "/geospatial-plot.txt"),
    row.names = FALSE, quote = FALSE, sep = '\t'
)

dist.abund = dcast(data = prs, P1 ~ P2, value.var = "BrayCurtis", fill = 0)
dist.geo   = dcast(data = prs, P1 ~ P2, value.var = "geodistance", fill = 0)

dist.abund = as.matrix(setDF(dist.abund[, 2:ncol(dist.abund)], rownames = dist.abund$P1))
dist.geo   = as.matrix(setDF(dist.geo[, 2:ncol(dist.geo)], rownames = dist.geo$P1))

dist.abund = as.dist(dist.abund)
dist.geo   = as.dist(dist.geo)

abund_geo = mantel(
    dist.abund, dist.geo, method = "spearman", 
    permutations = 999, na.rm = TRUE
)

abund_geo$perm = NULL

abund_geo$control = NULL

abund_geo = unlist(abund_geo)
abund_geo = paste0(
    names(abund_geo), " = ", 
    as.character(abund_geo)
)

writeLines(abund_geo, paste0(workdir, "/geospatial.txt"))


# gr = ggplot(data = prs, aes(x = geodistance, y = BrayCurtis)) +
#     
#     geom_point(shape = 21, color = "grey50", fill = "grey75",
#                stroke = .25, size = 1.5) +
#     
#     geom_smooth(formula =  y ~ x, # method = "gam", 
#                 span = 1, color = "red", linewidth = 1.25) +
#     
#     scale_x_continuous(
#         expand = c(0, 0),
#         labels = scales::comma_format(scale = .001)
#     ) +
#     
#     scale_y_continuous(expand = c(0, 0)) +
#     
#     coord_cartesian(expand = TRUE, clip = "off") +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         
#         axis.ticks = element_line(linewidth = .3, color = "grey90"),
#         panel.grid = element_line(linewidth = .3, color = "grey90"),
#         
#         axis.title.x = element_text(margin = margin(t = 10)),
#         axis.title.y = element_text(margin = margin(r = 10)),
#         
#         plot.margin = margin(20, 20, 20, 20)
#     ) +
#     
#     labs(
#         x = "Geographic distance (km)", y = "Bray Curtis dissimilarity"
#     )
# 
# ggsave(
#     plot = gr,
#     filename = paste0(workdir, "/geospatial.pdf"),
#     width = 10, height = 10, device = cairo_pdf,
#     units = "in"
# )
# 
# ggsave(
#     plot = gr,
#     filename = paste0(workdir, "/geospatial.svg"),
#     width = 10, height = 8, units = "in"
# )
# 
# ggsave(
#     plot = gr,
#     filename = paste0(workdir, "/geospatial.jpeg"),
#     width = 10, height = 8, units = "in"
# )















