
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
graph_obj       <- "emp-soil-analysis-clean-sub5k/SpiecEasi-Soil (non-saline).graphml"
workdir         <- dirname(sample_map)
filterTaxaPar   <- 400 
nbootstraps     <- 100



# analysis -------------------------------------

s0 <- fread(sample_map)
df <- fread(abundance_table)

pb <- progress_bar$new(total = nbootstraps)

rm(abundance_table, sample_map)
gc()

net_spieceasi <- read_graph(graph_obj, format = "graphml") 


centralities <- list()
globalProps  <- list()


for(b in seq_len(length.out = nbootstraps)) {
    
    ntaxa = filterTaxaPar # sample(50:filterTaxaPar, 1)
    
    centralities_tmp <- list()
    globalProps_tmp  <- list()
    
    for(i in unique(s0$ClimateZone)) {
        
        s1 <- s0[which(ClimateZone == i), ]
        
        mm <- setDF(df[, s1$SampleIDabv, with = FALSE], rownames = df$TaxaIDabv)
        mm <- rowSums(mm)
        mm <- sort(mm, decreasing = TRUE)
        mm <- names(mm[which(mm != 0)])
        mm <- sample(mm, ntaxa)
        
        net_spieceasi_sub <- subgraph(net_spieceasi, mm)
        
        dt <- data.table(
            "ClimateZone" = i,
            "nTaxa"       = ntaxa,
            "Taxa"        = mm,
            "degree"      = numeric(length = length(mm)),
            "between"     = numeric(length = length(mm)),
            "close"       = numeric(length = length(mm)),
            "eigenv"      = numeric(length = length(mm)) 
        )
        
        tmp <- degree(net_spieceasi_sub, normalized = TRUE)
        dt$degree <- tmp[match(dt$Taxa, names(tmp))]
        
        tmp <- betweenness(net_spieceasi_sub, directed = FALSE, normalized = TRUE)
        dt$between <- tmp[match(dt$Taxa, names(tmp))]
        
        tmp <- closeness(net_spieceasi_sub, normalized = TRUE)
        dt$close <- tmp[match(dt$Taxa, names(tmp))]
        
        tmp <- eigen_centrality(net_spieceasi_sub)$vector
        dt$eigenv <- tmp[match(dt$Taxa, names(tmp))]
        
        centralities_tmp[[ paste0(b, ";", i) ]] <- dt
        
        globalProps_tmp[[i]] <- data.table(
            "ClimateZone"         = i,
            "nTaxa"               = ntaxa,
            "Average path length" = mean_distance(net_spieceasi_sub),
            "Transitivity"        = transitivity(net_spieceasi_sub),
            "Modularity"          = modularity(cluster_walktrap(net_spieceasi_sub)),
            # "Edge connectivity"   = edge_connectivity(net_spieceasi_sub),
            "Density"             = edge_density(net_spieceasi_sub)
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
    ), ]
    
    
    tmp = tmp[, c("ClimateZone", "Taxa"), with = FALSE]
    
    hubs[[i]] = tmp
}

hubs = rbindlist(hubs)

fwrite(
    hubs, paste0(workdir, "/hubs.txt"),
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










