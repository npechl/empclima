

rm(list = ls())
gc()


# load libraries -----------------------------

library(data.table)
library(stringr)

# library(hillR)
library(vegan)

library(rstatix)

# library(ggplot2)
# library(ggforce)
# library(ggpubr)
# library(ggh4x)
# library(ggsci)
# 
# library(extrafont)

# list of inputs ------------------------------------

sample_map      <- "emp-soil-analysis-clean-sub5k-v2/sample-metadata.Soil (non-saline).txt"
abundance_table <- "emp-soil-analysis-clean-sub5k-v2/abundance-table.Soil (non-saline).txt"
workdir         <- dirname(sample_map)


# analysis ------------------------------

s0 <- fread(sample_map)
d0 <- fread(abundance_table)

d1 <- transpose(d0, keep.names = "sample", make.names = "TaxaIDabv")
d1 <- setDF(d1[, 2:ncol(d1)], rownames = d1$sample)

# for(i in hill) {
#     
#     s0[[paste0("hill.q", i)]] <- hill_taxa(comm = d1, q = i)
#     
# }


tmp        <- diversity(d1, index = "simpson")
index      <- match(s0$SampleIDabv, names(tmp))
s0$simpson <- tmp[index]

tmp        <- diversity(d1, index = "shannon")
index      <- match(s0$SampleIDabv, names(tmp))
s0$shannon <- tmp[index]

tmp <- estimateR(d1)
tmp <- setDT(as.data.frame(t(tmp)), keep.rownames = "sample")

s0 <- merge(s0, tmp, by.x = "SampleIDabv", by.y = "sample", all.x = TRUE)

fwrite(
    s0, sample_map,
    row.names = FALSE, quote = FALSE, sep = "\t"
)

# analysis libraries -------------------------

rm(abundance_table, index, sample_map, d0,tmp, d1)
gc()

alpha_stats    <- list()
s0$ClimateZone <- as.factor(s0$ClimateZone)

# Shannon index -------------------------------------

anova_test(data = s0, shannon ~ ClimateZone)

x = tukey_hsd(x = s0, shannon ~ ClimateZone, p.adjust.method = "holm")
# x = x[which(x$p.adj <= 0.05), ]
x = setDT(x)

alpha_stats[["Shannon"]] = x

# x$group1 = factor(x$group1, levels = levels(s0$ClimateZone))
# x$group2 = factor(x$group2, levels = levels(s0$ClimateZone))
# 
# x$xmin = as.numeric(x$group1)
# x$xmax = as.numeric(x$group2)
# 
# x$g1 = str_sub(x$group1, 1, 1)
# x$g2 = str_sub(x$group2, 1, 1)
# 
# x = x[order(g1, g2), ]
# 
# x$y.position = numeric()
# 
# for(i in seq_len(nrow(x))) {
#     
#     x[i, ]$y.position = 5.6 + (i * 0.4)
#     
# }
# 
# s0$`Level of Heat` = str_replace_all(s0$`Level of Heat`, "summer", "sum")
# 
# gr = ggplot(data = s0, 
#             aes(
#                 x = weave_factors(
#                     ClimateZone, `Level of Heat`, 
#                     `Precipitation Type`, Group
#                 ), 
#                 y = shannon
#             )) +
#     
#     
#     stat_boxplot(geom = 'errorbar', width = 0.25, color = "black", linewidth = .5) +
#     
#     geom_boxplot(aes(fill = Group), color = "black",
#                  outlier.shape = NA, width = .5, linewidth = .5) +
#     
#     geom_point(
#         # aes(fill = Group),
#         fill = "black", color = "grey10",
#         shape = 21, size = 1, stroke = .1,
#         position = position_jitternormal(sd_y = 0, sd_x = .04)
#     ) +
#     
#     stat_pvalue_manual(x, label = "p.adj.signif", hide.ns = TRUE,
#                        tip.length = 0.005, vjust = .8) +
#     
#     scale_y_continuous(breaks = c(2.5, 5, 10, 15)) +
#     
#     scale_fill_npg() + 
#     
#     guides(x = "axis_nested") +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         legend.position = "none",
#         axis.title.x = element_blank(),
#         ggh4x.axis.nestline.x = element_line(color = "grey", linewidth = 0.25),
#         
#         panel.grid = element_blank(),
#         
#         axis.line = element_line(linewidth = .3),
#         axis.ticks = element_line(linewidth = .3),
#         
#         plot.margin = margin(20, 20, 20, 20)
#     ) +
#     
#     labs(y = "Shannon index")
# 
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Shannon-index.pdf"),
#     width = 10, height = 8, units = "in", device = cairo_pdf
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Shannon-index.svg"),
#     width = 10, height = 8, units = "in"
# )


# Chao1 ------------------------------------------------

anova_test(data = s0, S.chao1 ~ ClimateZone)

x = tukey_hsd(x = s0, S.chao1 ~ ClimateZone, p.adjust.method = "holm")
# x = x[which(x$p.adj <= 0.05), ]
x = setDT(x)

alpha_stats[["Chao1"]] = x
# 
# x$group1 = factor(x$group1, levels = levels(s0$ClimateZone))
# x$group2 = factor(x$group2, levels = levels(s0$ClimateZone))
# 
# x$xmin = as.numeric(x$group1)
# x$xmax = as.numeric(x$group2)
# 
# x$g1 = str_sub(x$group1, 1, 1)
# x$g2 = str_sub(x$group2, 1, 1)
# 
# x = x[order(g1, g2), ]
# 
# x$y.position = numeric()
# 
# for(i in seq_len(nrow(x))) {
#     
#     x[i, ]$y.position = 650 + (i * 150)
#     
# }
# 
# gr = ggplot(data = s0, 
#             aes(
#                 x = weave_factors(
#                     ClimateZone, `Level of Heat`, 
#                     `Precipitation Type`, Group
#                 ), 
#                 y = S.chao1
#             )) +
#     
#     stat_boxplot(geom = 'errorbar', width = 0.25, color = "black", linewidth = .5) +
#     
#     geom_boxplot(aes(fill = Group), color = "black",
#                  outlier.shape = NA, width = .5, linewidth = .5) +
#     
#     geom_point(
#         # aes(fill = Group),
#         fill = "black", color = "grey10",
#         shape = 21, size = 1, stroke = .1,
#         position = position_jitternormal(sd_y = 0, sd_x = .04)
#     ) +
#     
#     # stat_pvalue_manual(
#     #     x, label = "p.adj.signif", hide.ns = TRUE,
#     #     tip.length = 0.005, vjust = .8
#     # ) +
#     
#     scale_y_continuous(
#         labels = function(x) {
#             format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)
#         },
#         
#         breaks = c(250, 500, 1000, 2000, 4000, 6000)
#     ) +
#     
#     scale_fill_nejm() +
#     
#     guides(x = "axis_nested") +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         legend.position = "none",
#         axis.title.x = element_blank(),
#         ggh4x.axis.nestline.x = element_line(color = "grey", linewidth = 0.25),
#         
#         panel.grid = element_blank(),
#         
#         axis.line = element_line(linewidth = .3),
#         axis.ticks = element_line(linewidth = .3),
#         
#         plot.margin = margin(20, 20, 20, 20)
#     ) +
#     
#     labs(y = "Chao1 index")
# 
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Chao1-index.pdf"),
#     width = 10, height = 8, units = "in", device = cairo_pdf
# )
# 
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Chao1-index.svg"),
#     width = 10, height = 8, units = "in"
# )

# Simpson ------------------------------------------------

anova_test(data = s0, simpson ~ ClimateZone)

x = tukey_hsd(x = s0, simpson ~ ClimateZone)
# x = x[which(x$p.adj <= 0.05), ]
x = setDT(x)

alpha_stats[["Simpson"]] = x
# 
# x$group1 = factor(x$group1, levels = levels(s0$ClimateZone))
# x$group2 = factor(x$group2, levels = levels(s0$ClimateZone))
# 
# x$xmin = as.numeric(x$group1)
# x$xmax = as.numeric(x$group2)
# 
# x$g1 = str_sub(x$group1, 1, 1)
# x$g2 = str_sub(x$group2, 1, 1)
# 
# x = x[order(g1, g2), ]
# 
# x$y.position = numeric()
# 
# for(i in seq_len(nrow(x))) {
# 
#     x[i, ]$y.position = 1 + (i * 0.25)
# 
# }
# 
# 
# 
# gr = ggplot(data = s0, 
#             aes(
#                 x = weave_factors(
#                     ClimateZone, `Level of Heat`, 
#                     `Precipitation Type`, Group
#                 ), 
#                 y = simpson
#             )) +
#     
#     stat_boxplot(geom = 'errorbar', width = 0.25, color = "black", linewidth = .5) +
#     
#     geom_boxplot(aes(fill = Group), color = "black",
#                  outlier.shape = NA, width = .5, linewidth = .5) +
#     
#     geom_point(
#         # aes(fill = Group),
#         fill = "black", color = "grey10",
#         shape = 21, size = 1, stroke = .1,
#         position = position_jitternormal(sd_y = 0, sd_x = .04)
#     ) +
#     
#     # stat_pvalue_manual(
#     #     x, label = "p.adj.signif", hide.ns = TRUE,
#     #     tip.length = 0.005, vjust = .8
#     # ) +
#     
#     scale_fill_nejm() +
#     
#     guides(x = "axis_nested") +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         legend.position = "none",
#         axis.title.x = element_blank(),
#         ggh4x.axis.nestline.x = element_line(color = "grey", linewidth = 0.25),
#         
#         panel.grid = element_blank(),
#         
#         axis.line = element_line(linewidth = .3),
#         axis.ticks = element_line(linewidth = .3),
#         
#         plot.margin = margin(20, 20, 20, 20)
#     ) +
#     
#     labs(y = "Simpson index")
# 
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Simpson-index.pdf"),
#     width = 12, height = 8, units = "in", device = cairo_pdf
# )
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/Simpson-index.svg"),
#     width = 12, height = 8, units = "in"
# )


alpha_stats = rbindlist(alpha_stats, idcol = "index")

fwrite(
    alpha_stats, paste0(workdir, "/diversity1.csv"),
    row.names = FALSE, quote = TRUE, sep = ","
)
