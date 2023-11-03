
rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(igraph)

library(progress)

# list of inputs ------------------------------------

sample_map      <- "emp-soil-analysis-clean-sub5k/sample-metadata.Soil (non-saline).txt"
abundance_table <- "emp-soil-analysis-clean-sub5k/abundance-table.Soil (non-saline).txt"
taxonomy        <- "emp-soil-analysis-clean-sub5k/taxonomy-table.Soil (non-saline).txt"
workdir         <- dirname(sample_map)
# filterTaxaPar   <- 300 
nbootstraps     <- 100



# analysis -------------------------------------

s0 <- fread(sample_map)
df <- fread(abundance_table)
t0 <- fread(taxonomy)

pb <- progress_bar$new(total = nbootstraps)

rm(abundance_table, sample_map)
gc()


graph_obj = workdir |> 
    list.files(pattern = "graphml", full.names = TRUE) |>
    lapply(function(x) {
        
        g <- x |> read_graph(format = "graphml")
        g <- g |> delete_edges( edges = which(E(g)$weight < 0))
        
        return(g)
        
    })

names(graph_obj) = workdir |> 
    list.files(pattern = "graphml", full.names = TRUE) |>
    basename() |>
    str_remove_all("SpiecEasi|graphml|-|\\.")

ntaxa = graph_obj |> 
    lapply(V) |> 
    lapply(names) |>  
    lapply(length) |> 
    unlist() |> 
    min() 

ntaxa = (3 * ntaxa / 4) |> floor()

centralities <- list()
globalProps  <- list()


for(b in seq_len(length.out = nbootstraps)) {
    
    # ntaxa = filterTaxaPar # sample(50:filterTaxaPar, 1)
    
    centralities_tmp <- list()
    globalProps_tmp  <- list()
    
    for(i in names(graph_obj)) {
        
        s1 <- s0[which(ClimateZone == i), ]
        
        # mm <- df[, s1$SampleIDabv, with = FALSE] |>
        #     setDF(rownames = df$TaxaIDabv) |>
        #     rowSums() |>
        #     sort(decreasing = TRUE)
        # 
        # mm <- mm[which(mm != 0)] |>
        #     names() |>
        #     sample(size = ntaxa)
        
        g_sub <- graph_obj[[i]]
        
        index = V(g_sub) |> names() |> sample(size = ntaxa)
        
        g_sub = g_sub |> subgraph(index)
        
        dt <- data.table(
            "ClimateZone" = i,
            "nTaxa"       = ntaxa,
            "Taxa"        = V(g_sub) |> names(),
            "degree"      = numeric(length = ntaxa),
            "between"     = numeric(length = ntaxa),
            "close"       = numeric(length = ntaxa),
            "eigenv"      = numeric(length = ntaxa) 
        )
        
        tmp <- degree(g_sub, normalized = TRUE)
        dt$degree <- tmp[match(dt$Taxa, names(tmp))]
        
        tmp <- betweenness(g_sub, directed = FALSE, normalized = TRUE)
        dt$between <- tmp[match(dt$Taxa, names(tmp))]
        
        tmp <- closeness(g_sub, normalized = TRUE)
        dt$close <- tmp[match(dt$Taxa, names(tmp))]
        
        tmp <- eigen_centrality(g_sub)$vector
        dt$eigenv <- tmp[match(dt$Taxa, names(tmp))]
        
        centralities_tmp[[ paste0(b, ";", i) ]] <- dt
        
        globalProps_tmp[[i]] <- data.table(
            "ClimateZone"         = i,
            "nTaxa"               = ntaxa,
            "Average path length" = mean_distance(g_sub),
            "Transitivity"        = transitivity(g_sub),
            "Modularity"          = modularity(cluster_fast_greedy(g_sub)),
            "Density"             = edge_density(g_sub)
        )
        
    }
    
    centralities_tmp     <- rbindlist(centralities_tmp)
    centralities_tmp$Run <- b
    
    globalProps_tmp     <- rbindlist(globalProps_tmp)
    globalProps_tmp$Run <- b
    
    centralities[[b]] <- centralities_tmp
    globalProps[[b]]  <- globalProps_tmp
    
    pb$tick()
    
}

centralities <- rbindlist(centralities)
globalProps  <- rbindlist(globalProps)

centralities <- centralities[, by = .(ClimateZone, nTaxa, Taxa), .(
    degree  = mean(degree, na.rm = TRUE),
    between = mean(between, na.rm = TRUE),
    close   = mean(close, na.rm = TRUE),
    eigenv  = mean(eigenv, na.rm = TRUE)
)]


hubs = split(centralities, centralities$ClimateZone)

for(i in names(hubs)) {
    
    tmp = hubs[[i]]
    
    tmp = tmp[which(
        degree >= quantile(tmp$degree, probs = 0.95) &
            eigenv >= quantile(tmp$eigenv, probs = 0.95)
    )]
    
    tmp = tmp[, c("ClimateZone", "Taxa"), with = FALSE]
    
    hubs[[i]] = tmp
}

hubs = rbindlist(hubs)

hubs         = merge(hubs, t0, by.x = "Taxa", by.y = "TaxaIDabv", all.x = TRUE)
centralities = merge(centralities, t0, by.x = "Taxa", by.y = "TaxaIDabv", all.x = TRUE)

hubs$TaxaID         = NULL
centralities$TaxaID = NULL

fwrite(
    hubs, paste0(workdir, "/hubs-bootstrap.txt"),
    row.names = FALSE, quote = FALSE, sep = "\t"
)

fwrite(
    centralities, paste0(workdir, "/centralities-bootstrap.txt"),
    row.names = FALSE, quote = FALSE, sep = "\t"
)

fwrite(
    globalProps, paste0(workdir, "/globalProps-bootstrap.txt"),
    row.names = FALSE, quote = FALSE, sep = "\t"
)










