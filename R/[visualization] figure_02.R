

rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggforce)
library(extrafont)
library(ggh4x)
library(ggpubr)

library(paletteer)

# Fig. 2 A =============================================

s0 = fread("emp-soil-analysis-clean-release1/sample-metadata.Soil (non-saline).txt")
x  = fread("emp-soil-analysis-clean-release1/diversity1.csv")

x = x[which(x$p.adj <= 0.05), ]

x$group1 = factor(x$group1, levels = sort(unique(s0$ClimateZone)))
x$group2 = factor(x$group2, levels = sort(unique(s0$ClimateZone)))

x$xmin = as.numeric(x$group1)
x$xmax = as.numeric(x$group2)

x$g1 = str_sub(x$group1, 1, 1)
x$g2 = str_sub(x$group2, 1, 1)

x = x[order(g1, g2), ]

x$y.position = numeric()

for(i in seq_len(nrow(x))) {

    x[i, ]$y.position = 5.6 + (i * 0.4)

}

s0$`Level of Heat` = str_replace_all(s0$`Level of Heat`, "summer", "sum")

gr1 = ggplot(data = s0,
            aes(
                x = ClimateZone,
                y = shannon
            )) +


    stat_boxplot(geom = 'errorbar', width = 0.25, color = "black", linewidth = .5) +

    geom_boxplot(aes(fill = Group), color = "black",
                 outlier.shape = NA, width = .5, linewidth = .25) +

    geom_point(
        # aes(fill = Group),
        fill = "grey10", color = "black",
        shape = 21, size = 1.5, stroke = .1,
        position = position_jitternormal(sd_y = 0, sd_x = .02)
    ) +
    
    geom_hline(yintercept = mean(s0$shannon), linetype = 2, linewidth = 1, color = "yellow3") +
    
    stat_compare_means(
        label = "p.signif", method = "wilcox.test", ref.group = ".all.", 
        hide.ns = TRUE
    ) +

    # stat_pvalue_manual(x, label = "p.adj.signif", hide.ns = TRUE,
    #                    tip.length = 0.005, vjust = .8) +

    scale_y_continuous(breaks = c(2.5, 5, 10, 15)) +

    # scale_fill_npg() +
    
    scale_fill_manual(
        values = paletteer_d("ggthemes::Color_Blind")
    ) +

    guides(x = "axis_nested", fill = guide_legend(
        label.theme = element_text(size = 11, family = "Calibri")
    )) +

    theme_minimal(base_family = "Calibri") +

    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        # axis.title.x = element_blank(),
        ggh4x.axis.nestline.x = element_line(color = "grey", linewidth = 0.25),

        panel.grid = element_blank(),

        axis.title = element_text(size = 11),
        axis.text = element_text(size = 11),
        
        axis.line = element_line(linewidth = .3),
        axis.ticks = element_line(linewidth = .3)
    ) +

    labs(y = "Shannon index", x = "Climate zone")



# Fig. 2 B =============================================

library(ggplot2)
library(ggrepel)
library(ggsci)

library(extrafont)

library(ggdensity)
library(ggh4x)

library(patchwork)


s0 = fread("emp-soil-analysis-clean-sub10k/sample-metadata.Soil (non-saline).txt")

a = ggplot(data = s0, aes(x = MDS1, y = MDS2)) +
    
    geom_hdr(aes(fill = Group), show.legend = FALSE) +

    geom_point(
        aes(fill = Group),
        color = "white",
        stroke = .25, shape = 21, size = 2
    ) +

    # geom_text_repel(
    #     aes(label = ClimateZone),
    #     max.overlaps = Inf,
    #     size = 3,
    #     segment.linetype = "dotted",
    #     segment.size = .35,
    #
    #     family = "Calibri"
    # ) +

    # scale_fill_viridis_d(option = "magma") +

    # scale_fill_grey() +

    # scale_fill_npg() +
    # 
    # scale_color_npg() +

    scale_fill_manual(values = paletteer_d("ggthemes::Color_Blind")) +
    scale_color_manual(values = paletteer_d("ggthemes::Color_Blind")) +

    coord_cartesian(expand = TRUE, clip = "off") +

    theme_minimal(base_family = "Calibri") +

    theme(
        legend.position = "none",
        axis.line = element_line(linewidth = .3),
        axis.ticks = element_line(linewidth = .3),
        
        # axis.title.x = element_blank(),

        panel.grid = element_blank()
    ) +

    # guides(
    #     fill = guide_legend(title.position = "top"),
    #     color = guide_legend(title.position = "top"),
    #     alpha = guide_legend(title.position = "top")
    # ) +
    
    labs(x = "NMDS1", y = "NMDS2")




b = ggplot(data = s0, aes(x = MDS1, y = MDS2)) +

    geom_hdr(aes(fill = Group), show.legend = FALSE) +

    geom_point(
        aes(fill = Group),
        color = "white",
        stroke = .25, shape = 21, size = 2
    ) +

    # scale_fill_npg() +
    # scale_color_npg() +
    
    scale_fill_manual(values = paletteer_d("ggthemes::Color_Blind"), guide = "none") +
    scale_color_manual(values = paletteer_d("ggthemes::Color_Blind"), guide = "none") +

    # facet_wrap(vars(Group), nrow = 1) +

    # scale_y_continuous(expand = c(0, 0)) +
    # scale_x_continuous(expand = c(0, 0)) +

    coord_cartesian(expand = TRUE, clip = "on") +

    facet_wrap2(vars(Group), nrow = 1, axes = "all") +

    theme_minimal(base_family = "Calibri") +

    theme(
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 11),

        panel.grid = element_blank(),

        axis.line = element_line(linewidth = .3),
        axis.ticks = element_line(linewidth = .3),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 11)
    )
    
    labs(x = 'NMDS1', y = "NMDS2")





# gr2 = (a / b) +
#     plot_layout(guides = 'collect') &
#     theme(
#         legend.title = element_blank(),
#         legend.position = "bottom"
#     )

# Fig. 2 C =============================================

prs = fread("emp-soil-analysis-clean-sub10k/geospatial-plot.txt")

prs$BrayCurtis = 1 - prs$BrayCurtis


gr3 = ggplot(data = prs, aes(x = geodistance, y = BrayCurtis)) +

    geom_point(shape = 20, color = "grey75",
               size = 1.5) +

    geom_smooth(
        formula =  y ~ x, # method = "gam",
        span = 1, 
        color = "red", 
        linewidth = 1
    ) +
    
    annotate(
        "text", x = 7500000, y = .8, 
        label = "Mantel: R-squared = 0.24, p-value < 0.001", 
        family = "Calibri", fontface = "bold", hjust = .5
    ) +

    scale_x_continuous(
        expand = c(0, 0),
        labels = scales::comma_format(scale = .001)
    ) +

    scale_y_continuous(expand = c(0, 0)) +

    coord_cartesian(expand = TRUE, clip = "on") +

    theme_minimal(base_family = "Calibri") +

    theme(
        panel.grid = element_line(linewidth = .3, linetype = "dashed"),

        axis.title.x = element_text(margin = margin(t = 10), size = 11),
        axis.title.y = element_text(margin = margin(r = 10), size = 11),
        
        axis.text = element_text(size = 11),
        
        axis.line = element_line(linewidth = .3),
        axis.ticks = element_line(linewidth = .3)
    ) +

    labs(
        x = "Geographic distance (km)", 
        y = "Bray Curtis Similarity"
    )

# patchwork =================================

my = gr3 | gr1

multi = wrap_plots(my, b, ncol = 1) +
    
    plot_annotation(tag_levels = 'A') +
    
    plot_layout(guides = 'collect', heights = c(2, 1)) &
    
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.tag = element_text(face = "bold"),
        plot.margin = margin(10, 10, 10, 10)
    )


ggsave(
    plot = multi, filename = "Fig2.pdf", device = cairo_pdf,
    width = 14, height = 12, units = "in"
)

ggsave(
    plot = multi, filename = "Fig2.png", dpi = 600,
    width = 14, height = 12, units = "in"
)



