


rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(extrafont)

library(ggsci)

fls <- c(
    "emp-soil-analysis-sub2k/centralities-stats.csv",
    "emp-soil-analysis-sub5k/centralities-stats.csv",
    "emp-soil-analysis-sub10k/centralities-stats.csv"
)


climate_info <- "climate-classification-info.csv"


df = list()

for(i in seq_len(length(fls))) {
    
    d <- dirname(fls[i])
    
    tmp = fread(paste0(d, "/centralities-stats.csv"))
    
    df[[str_remove_all(d, "emp|soil|analysis|\\-")]] <- tmp
    
    
}

df = rbindlist(df, idcol = "subset")

df = df[which(p.adj <= 0.05), ]

df = split(df, df$subset)

z = lapply(df, function(x) {
    
    # x = x[which(index == "Shannon"), ]
    
    x = data.table(
        "subset" = c(x$subset, x$subset),
        "index"       = c(x$.y., x$.y.),
        "group1"      = c(x$group1, x$group2),
        "group2"      = c(x$group2, x$group1)
    )
    
    x = x[, by = .(subset, index, group1), .(
        groups = paste(sort(group2), collapse = ", "),
        N = length(sort(group2))
    )]
    
    x = x[order(index, -N), ]
    
    return(x)
    
})


z = rbindlist(z)

library(ggh4x)
library(extrafont)


z$subset = factor(
    z$subset, levels = c("sub2k", "sub5k", "sub10k")
)

z$index = factor(
    z$index, levels = c("degree", "between", "close")
)

rorder = unique(z[order(index, N), ]$group1)

z$y = factor(
    z$group1,
    levels = rorder
)

gr = ggplot(data = z, aes(x = N, y = group1)) +
    
    geom_point(size = 3) +
    
    scale_x_continuous(limits = c(1, 16),
                       breaks = seq(2, 16, by = 2)) +
    
    facet_grid2(
        rows = vars(subset),
        cols = vars(index),
        
        scales = "free_y",
        space = "free_y"
    ) +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        axis.title.y = element_blank(),
        
        strip.text = element_text(face = "bold"),
        
        panel.border = element_rect(fill = NA, linewidth = .3),
        
        axis.ticks = element_line(linewidth = .3),
        
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 10)),
        
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed"),
        panel.grid.minor = element_line(linewidth = .2, linetype = "dashed"),
        
        plot.margin = margin(10, 10, 10, 10)
    ) +
    
    labs(x = "No. of statistically significant differences")

ggsave(
    plot = gr, filename = "validation-topological.pdf",
    width = 10, height = 10, units = "in", device = cairo_pdf
)

ggsave(
    plot = gr, filename = "validation-topological.svg",
    width = 10, height = 10, units = "in"
)















