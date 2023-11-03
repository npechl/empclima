
rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)
library(igraph)

# library(ggplot2)
library(ggnetwork)
# library(extrafont)

# list of inputs ------------------------------------

sample_map      <- "emp-soil-analysis-clean-sub5k/sample-metadata.Soil (non-saline).txt"
abundance_table <- "emp-soil-analysis-clean-sub5k/abundance-table.Soil (non-saline).txt"
# graph_obj       <- "emp-soil-analysis-clean-sub5k"
workdir         <- dirname(sample_map)

s0 <- fread(sample_map)
df <- fread(abundance_table)

rm(abundance_table, sample_map)
gc()


net_spieceasi_fls <- workdir |> 
    list.files(pattern = "graphml", full.names = TRUE)
# net_spieceasi <- read_graph(graph_obj, format = "graphml") 
# E(net_spieceasi)$weight = ifelse(E(net_spieceasi)$weight < 0, 0, E(net_spieceasi)$weight)

abund_degree = list()

ve_counts = list()

for(i in net_spieceasi_fls) {
    
    zone = i |> basename() |> str_remove_all("SpiecEasi|graphml|-|\\.")
    
    net_spieceasi <- read_graph(i, format = "graphml")
    
    ve_counts[[zone]] = data.table(
        "No. of nodes" = vcount(net_spieceasi),
        "No. of edges" = ecount(net_spieceasi)
    )
    
    net_degree <- degree(
        graph      = net_spieceasi,
        mode       = c("all"),
        loops      = TRUE,
        normalized = FALSE
    )
    
    net_degree <- data.table(
        "Taxa" = names(net_degree),
        "k"    = net_degree
    )
    
    tmp <- df[, 2:ncol(df)] |> 
        lapply(function(x) { return(x / sum(x)) }) |>
        setDT()
    
    df  <- cbind(df[, 1], tmp)
    
    df2 <- df |> 
        melt(
            id.vars = "TaxaIDabv", 
            variable.name = "Sample", value.name = "value",
            variable.factor = FALSE, value.factor = FALSE
        ) |>
        merge(net_degree, by.x = "TaxaIDabv", by.y = "Taxa")
    
    cor.test(x = df2$value, y = df2$k)
    
    abund_degree[[zone]] = df2

    
}

abund_degree = abund_degree |> rbindlist(idcol = "Climate zone")
ve_counts    = ve_counts |> rbindlist(idcol = "Climate zone")


fwrite(
    abund_degree, paste0(workdir, "/abundance-degree.txt"),
    row.names = FALSE, quote = FALSE, sep = "\t"
)

fwrite(
    ve_counts, paste0(workdir, "/nodes-edges.txt"),
    row.names = FALSE, quote = FALSE, sep = "\t"
)


gr <- ggplot(data = abund_degree[which(value != 0)], aes(x = k, y = value)) +

    geom_point(
        shape = 21, size = 1.5, stroke = 0,
        fill = alpha("grey10", alpha = .1), color = "grey95"
    ) +

    theme_minimal(base_family = "Calibri") +

    scale_y_continuous(labels = scales::percent, trans = "log10") +

    theme(
        panel.border = element_rect(linewidth = .3, fill = NA),

        axis.ticks = element_line(linewidth = .3),

        plot.margin = margin(20, 20, 20, 20)
    ) +

    labs(
        y = "Relative abundance"
    )

ggsave(
    plot = gr, filename = paste0(workdir, "/degree-abundance.jpeg"),
    width = 10, height = 10, units = "in", dpi = 600
)





# net_degree    <- net_degree[, by = k, .N]
# net_degree$k2 <- net_degree$k ^ 2
# net_degree$k3 <- net_degree$k ^ 3
# net_degree$k4 <- net_degree$k ^ 4
# 
# model <- lm(N ~ k + k2 + k3 + k4, data = net_degree)
# 
# summary(model)

# fwrite(
#     net_degree, "degree-distribution.txt",
#     row.names = FALSE, quote = FALSE, sep = "\t"
# )

# library(extrafont)
# 
# ggplot(data = net_degree, aes(x = k, y = N)) +
# 
#     geom_point() +
#     
#     scale_y_continuous(expand = c(0, 0), limits = c(0, 105),
#                        labels = function(x) {
#                            
#                            return(
#                                paste0(
#                                    format(
#                                        x, big.mark = " ", decimal.mark = ".", 
#                                        scientific = FALSE
#                                    ), 
#                                    "\n",
#                                    "(", round(100 * x / sum(net_degree$N), digits = 2), "%)"
#                                )
#                            )
#                        }) +
# 
#     coord_cartesian(expand = TRUE, clip = "off") +
# 
#     theme_minimal(base_family = "Calibri") +
# 
#     theme(
#         panel.border = element_rect(linewidth = .3, fill = NA),
#         axis.ticks = element_line(linewidth = .3),
#         plot.margin = margin(20, 20, 20, 20)
#     ) +
# 
#     labs(y = "Count number")
# 
# ggsave(
#     plot = gr, filename = paste0(workdir, "/degree-counts.jpeg"), 
#     width = 10, height = 10, units = "in"
# )
# 
# 
# clustering   <- list()
# globalProps  <- list()
# # plot_obj     <- list()
# 
# for(i in unique(s0$ClimateZone)) {
#     
#     s1 <- s0[which(ClimateZone == i), ]
#     
#     mm <- setDF(df[, s1$SampleIDabv, with = FALSE], rownames = df$TaxaIDabv)
#     mm <- sort(rowSums(mm), decreasing = TRUE)
#     mm <- names(mm[which(mm != 0)])
#     
#     net_spieceasi_sub <- subgraph(net_spieceasi, mm)
#     
#     # dt <- data.table(
#     #     "ClimateZone" = i,
#     #     "Taxa"        = mm,
#     #     "degree"      = numeric(length = length(mm)),
#     #     # "between"     = numeric(length = length(mm)),
#     #     # "close"       = numeric(length = length(mm)),
#     #     "eigenv"      = numeric(length = length(mm))
#     # )
#     # 
#     # tmp <- degree(net_spieceasi_sub, normalized = TRUE)
#     # dt$degree <- tmp[match(dt$Taxa, names(tmp))]
#     #
#     # tmp = betweenness(net_spieceasi_sub, directed = FALSE, normalized = TRUE)
#     # dt$between = tmp[match(dt$Taxa, names(tmp))]
#     # 
#     # tmp = closeness(net_spieceasi_sub, normalized = TRUE)
#     # dt$close = tmp[match(dt$Taxa, names(tmp))]
#     #
#     # tmp <- eigen_centrality(net_spieceasi_sub)$vector
#     # dt$eigenv <- tmp[match(dt$Taxa, names(tmp))]
#     #
#     # centralities[[i]] = dt
#     #
#     # index <- which(
#     #     dt$degree >= quantile(dt$degree, probs = 0.95) & 
#     #         dt$eigenv >= quantile(dt$eigenv, probs = 0.95)
#     # )
#     # 
#     # hubs[[i]] <- data.table(
#     #     "ClimateZone" = i,
#     #     "Taxa"        = dt[index, ]$Taxa
#     # ) 
#     #
#     # globalProps[[i]] = data.table(
#     #     "ClimateZone"         = i,
#     #     "Average path length" = mean_distance(net_spieceasi_sub, directed = FALSE),
#     #     "Transitivity"        = transitivity(net_spieceasi_sub),
#     #     "Modularity"          = modularity(cluster_walktrap(net_spieceasi_sub)),
#     #     # "Edge connectivity"   = edge_connectivity(net_spieceasi_sub),
#     #     "Density"             = edge_density(net_spieceasi_sub),
#     #     "Diameter"            = diameter(net_spieceasi_sub, unconnected = TRUE, directed = FALSE)
#     # )
#     
#     
#     
#     tmp <- groups(cluster_fast_greedy(net_spieceasi_sub))
#     tmp <- lapply(tmp, as.data.table)
#     tmp <- rbindlist(tmp, idcol = "Group")
#     
#     clustering[[i]] <- data.table(
#         "ClimateZone" = i,
#         "Group"       = tmp$Group,
#         "Taxa"        = tmp[[2]]
#     )
#     
#     
#     # net_plot <- fortify(model = net_spieceasi_sub, layout = nicely())
#     # net_plot <- setDT(net_plot)
#     
#     # # net_plot$degree      <- centralities[[i]][match(net_plot$name, centralities[[i]]$Taxa), ]$degree
#     # net_plot$cluster     <- clustering[[i]][match(net_plot$name, clustering[[i]]$Taxa), ]$Group
#     # # net_plot$hub         <- ifelse(net_plot$name %in% hubs[[i]]$Taxa, "yes", "no")
#     # net_plot$ClimateZone <- i
# 
#     # plot_obj[[i]] <- net_plot
# }
# 
# # centralities <- rbindlist(centralities)
# # hubs         <- rbindlist(hubs)
# clustering   <- rbindlist(clustering)
# # globalProps  <- rbindlist(globalProps)
# # plot_obj     <- rbindlist(plot_obj)
# 
# # fwrite(
# #     centralities, paste0(workdir, "/centralities.txt"),
# #     row.names = FALSE, quote = FALSE, sep = "\t"
# # )
# #
# # fwrite(
# #     hubs, paste0(workdir, "/hubs.txt"),
# #     row.names = FALSE, quote = FALSE, sep = "\t"
# # )
# 
# fwrite(
#     clustering, paste0(workdir, "/clustering.txt"),
#     row.names = FALSE, quote = FALSE, sep = "\t"
# )
# 
# # fwrite(
# #     globalProps, paste0(workdir, "/globalProps.txt"),
# #     row.names = FALSE, quote = FALSE, sep = "\t"
# # )
# # 
# # fwrite(
# #     plot_obj, paste0(workdir, "/sub-networks.txt"),
# #     row.names = FALSE, quote = FALSE, sep = "\t"
# # )
# 

