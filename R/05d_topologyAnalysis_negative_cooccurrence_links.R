
rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(igraph)

library(progress)

# list of inputs ------------------------------------

sample_map      <- "emp-soil-analysis-clean-release1-v2/sample-metadata.Soil (non-saline).txt"
abundance_table <- "emp-soil-analysis-clean-release1-v2/abundance-table.Soil (non-saline).txt"
# graph_obj       <- "emp-soil-analysis-clean-sub5k/SpiecEasi-Soil (non-saline).graphml"
taxa_map        <- "emp-soil-analysis-clean-release1-v2/taxonomy-table.Soil (non-saline).txt"
workdir         <- dirname(sample_map)



# analysis -------------------------------------

s0       <- fread(sample_map)
df       <- fread(abundance_table)
taxa_map <- fread(taxa_map)

taxa_map$TaxaID <- NULL

for(i in c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) {
    
    taxa_map[[i]] = str_split(taxa_map[[i]], "\\__", simplify = TRUE)[,2]
    taxa_map[[i]] = str_remove_all(taxa_map[[i]], "\\[|\\]")
    taxa_map[[i]] = str_to_lower(taxa_map[[i]])
    
}

# net_spieceasi <- read_graph(graph_obj, format = "graphml") 

net_spieceasi = workdir |> 
    list.files(pattern = "graphml", full.names = TRUE) |>
    lapply(function(x) {
        
        g <- x |> read_graph(format = "graphml")
        # g <- g |> delete_edges( edges = which(E(g)$weight < 0))
        
        return(g)
        
    })

names(net_spieceasi) = workdir |> 
    list.files(pattern = "graphml", full.names = TRUE) |>
    basename() |>
    str_remove_all("SpiecEasi|graphml|-|\\.")

l = list()

for(i in names(net_spieceasi)) {
        
        s1 <- s0[which(ClimateZone == i), ]
        
        # mm <- setDF(df[, s1$SampleIDabv, with = FALSE], rownames = df$TaxaIDabv)
        # mm <- rowSums(mm)
        # mm <- sort(mm, decreasing = TRUE)
        # mm <- names(mm[which(mm != 0)])
        
        net_spieceasi_sub <- net_spieceasi[[i]] # subgraph(net_spieceasi, mm)
                
        dt = as_long_data_frame(net_spieceasi_sub)
        dt = setDT(dt)
                
        dt$from    = NULL
        dt$to      = NULL
        dt$from_id = NULL
        dt$to_id   = NULL
                
        dt = merge(dt, taxa_map, by.x = "from_name", by.y = "TaxaIDabv", all.x = TRUE)
        dt = merge(dt, taxa_map, by.x = "to_name", by.y = "TaxaIDabv", all.x = TRUE, suffixes = c(".from", ".to"))
        
        
        dt = melt(
            dt, id.vars = c("to_name", "from_name", "weight"),
            variable.factor = FALSE, value.factor = FALSE,
            variable.name = "level", value.name = "value"
        )
        
        dt[which(value == ""), ]$value = "unassigned"
        
        dt$connect = str_split(dt$level, "\\.", simplify = TRUE)[, 2]
        dt$level = str_split(dt$level, "\\.", simplify = TRUE)[, 1]
        
        dt = dcast(dt, to_name + from_name + weight + level ~ connect)
        
        dt$ClimateZone = i
        
        l[[i]] = dt
}
    
l = rbindlist(l)

# s0 = l[, by = .(from_name, to_name), .(
#     zones = paste(sort(unique(ClimateZone)), collapse = ", "),
#     N     = length(unique(ClimateZone))
# )]
# 
# s0 = s0[, by = N, Freq := .N / nrow(s0)]
# s0 = s0[order(Freq, zones), ]

# fwrite(
#     s0, paste0(workdir, "/links.csv"),
#     row.names = FALSE, quote = TRUE, sep = ","
# )

s0 = unique(l[, c("from_name", "to_name", "ClimateZone"), with = FALSE])

s0$value = mapply(function(x, y) {
    
    paste(sort(c(x, y)), collapse = " - ")
    
}, s0$from_name, s0$to_name, USE.NAMES = FALSE)

combinations = combn(x = sort(unique(s0$ClimateZone)), m = 2, simplify = FALSE)

s0 = split(s0, s0$ClimateZone)
s0 = lapply(s0, function(x) { x[["value"]] })

s1 = lapply(combinations, function(x) {
    
    m = length(which(s0[[ x[1] ]] %in% s0[[ x[2] ]]))
    
    out = data.table(
        "ClimateZone1" = x[1],
        "ClimateZone2" = x[2],
        
        "overlap"   = m,
        
        
        "overlap.freq" = m / sum(c(length(s0[[ x[1] ]]), length(s0[[ x[2] ]])))
    )
    
    return(out)
    
})

s1 = rbindlist(s1)
 
# ggplot(data = s1, aes(x = ClimateZone1, y = ClimateZone2)) +
#     
#     geom_point(aes(size = overlap.freq), 
#                shape = 21, color = "black", fill = "grey75", stroke = .25) +
#     
#     scale_size_continuous(
#         breaks = c(.1, .2, .3, .4, .5),
#         range = c(1, 8),
#         labels = scales::percent
#     )




l$level = factor(
    l$level, 
    levels = c(
        "Kingdom", "Phylum", "Class", 
        "Order", "Family", "Genus", "Species"
    )
)

l = l[order(to_name, from_name, level), ]

rm(df, dt, net_spieceasi, net_spieceasi_sub, s1, taxa_map)
rm(abundance_table, i, sample_map)
gc()


neg_uniq = unique(
    l[, c("from_name", "to_name", "weight", "ClimateZone"), with = FALSE]
)

neg_uniq$class = ifelse(
    neg_uniq$weight < 0,
    "Negative co-occurrence links",
    "Positive co-occurrence links"
)


neg_stats = neg_uniq[, by = .(ClimateZone, class), .N]
neg_stats = neg_stats[, by = ClimateZone, Freq := round(N / sum(N), digits = 4)]

neg_stats = neg_stats[order(ClimateZone), ]

neg_stats = dcast(neg_stats, ClimateZone ~ class, value.var = "Freq", fill = 0)

# library(ggplot2)
# library(ggsci)
# 
# library(extrafont)
# 
# gr = ggplot(data = neg_stats, aes(x = ClimateZone, y = Freq)) +
#     
#     geom_col(aes(fill = negative), color = "white", alpha = .95) +
#     
#     geom_hline(yintercept = .9, color = "yellow2", linewidth = 1, linetype = "dashed") +
#     
#     scale_y_continuous(expand = c(0, 0), labels = scales::percent,
#                        breaks = c(0, .25, .5, .75, .9, 1)) +
#     
#     scale_fill_igv() +
#     
#     theme_minimal(base_family = "Calibri") +
#     
#     theme(
#         legend.position = "bottom",
#         legend.title = element_blank(),
#         
#         axis.title = element_blank(),
#         
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# 
# ggsave(
#     plot = gr, filename = "neg.jpeg",
#     width = 10, height = 8, units = "in"
# )

l$class = ifelse(
    l$weight < 0,
    "Negative co-occurrence links",
    "Positive co-occurrence links"
)


taxon = paste0(l$from, ";", l$to)
taxon = str_split(taxon, ";")
taxon = lapply(taxon, sort)

l$from = unlist(lapply(taxon, function(x) { x[1] }))
l$to   = unlist(lapply(taxon, function(x) { x[2] }))

df = l[, by = .(ClimateZone, level, from, to, class), .N]
df = df[, by = .(ClimateZone, level), Freq := N / sum(N)]



df = dcast(
    df, ClimateZone + level + from + to ~ class, 
    value.var = "Freq", fill = 0
)

df = df[order(ClimateZone, level, -`Negative co-occurrence links`), ]

fwrite(
    df, paste0(workdir, "/negative.csv"),
    row.names = FALSE, quote = TRUE, sep = ","
)








