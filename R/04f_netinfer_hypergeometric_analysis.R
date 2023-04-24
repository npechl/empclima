


rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(igraph)

library(progress)

# list of inputs ------------------------------------

sample_map      <- "test/sample-metadata.Soil (non-saline).txt"
abundance_table <- "test/abundance-table.Soil (non-saline).txt"
graph_obj       <- "test/SpiecEasi-Soil (non-saline).graphml"
taxa_map        <- "test/taxonomy-table.Soil (non-saline).txt"
centralities    <- "test/centralities-bootstrap.txt"
workdir         <- dirname(sample_map)



# analysis -------------------------------------

s0 <- fread(sample_map)
df <- fread(abundance_table)

taxa_map     <- fread(taxa_map)

taxa_map$TaxaID <- NULL

for(i in c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) {
    
    taxa_map[[i]] = str_split(taxa_map[[i]], "\\__", simplify = TRUE)[,2]
    taxa_map[[i]] = str_remove_all(taxa_map[[i]], "\\[|\\]")
    
    taxa_map[[i]] = str_to_lower(taxa_map[[i]])
    
}

centr = fread(centralities)
centr = unique(centr[, c("ClimateZone", "Taxa"), with = FALSE])

centr = merge(centr, taxa_map, by.x = "Taxa", by.y = "TaxaIDabv", all.x = TRUE)

# rm(abundance_table, sample_map)
# gc()

net_spieceasi <- read_graph(graph_obj, format = "graphml") 
net_spieceasi <- delete_edges(net_spieceasi, edges = which(E(net_spieceasi)$weight < 0))

over = list()

for(i in unique(s0$ClimateZone)) {
    
    s1 <- s0[which(ClimateZone == i), ]
    
    mm <- setDF(df[, s1$SampleIDabv, with = FALSE], rownames = df$TaxaIDabv)
    mm <- rowSums(mm)
    mm <- sort(mm, decreasing = TRUE)
    mm <- names(mm[which(mm != 0)])
        
    net_spieceasi_sub <- subgraph(net_spieceasi, mm)
    
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
    
    over[[i]] = dt
}


rm(df, dt, net_spieceasi, net_spieceasi_sub)
rm(s0, s1, taxa_map, i, mm, abundance_table, graph_obj, sample_map)
gc()



over = rbindlist(over)

taxon = paste0(over$from, "-", over$to)
taxon = str_split(taxon, "-")
taxon = lapply(taxon, sort)

over$from = unlist(lapply(taxon, function(x) { x[1] }))
over$to   = unlist(lapply(taxon, function(x) { x[2] }))



links = over[, by = c("ClimateZone", "level", "from", "to"), .(N = .N)]
links = links[, by = .(ClimateZone, level), Freq := N / sum(N)]


links$level = factor(
    links$level, 
    levels = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
)

links = links[order(ClimateZone, level, -Freq), ]

links = links[, by = .(ClimateZone, level), `cumsum Freq` := cumsum(Freq)]

fwrite(
    links, paste0(workdir, "/links1.csv"),
    row.names = FALSE, quote = TRUE, sep = ","
)


links = links[, by = .(level, from, to), .(
    `No of ClimateZones` = length(unique(ClimateZone)),
    ClimateZones         = paste(sort(unique(ClimateZone)), collapse = ", ")
)]

links = links[order(level, -`No of ClimateZones`)]


fwrite(
    links, paste0(workdir, "/links2.csv"),
    row.names = FALSE, quote = TRUE, sep = ","
)

df2 = over[, c("level", "from", "to"), with = FALSE]
df2 = df2[, by = .(level, from, to), .(n1 = .N)]
df2 = df2[, by = level, n2 := (sum(n1) - n1)]

df = over[, by = .(ClimateZone, level, from, to), .(x = .N)]
df = df[, by = .(ClimateZone, level), n := sum(x)]


df$key  = paste(df$level, df$from, df$to, sep = ";")
df2$key = paste(df2$level, df2$from, df2$to, sep = ";")

index = match(df$key, df2$key)

df$n1 = df2[index, ]$n1
df$n2 = df2[index, ]$n2

df$key = NULL

df$pvalue = 1 - phyper(df$x, df$n1, df$n2, df$n)
df$p.adj  = p.adjust(df$pvalue, method = "fdr")

df$level = factor(
    df$level,
    levels = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
)


df = df[order(ClimateZone, level, from, to, p.adj)]

fwrite(
    df, paste0(workdir, "/hypergeometric1.csv"),
    row.names = FALSE, quote = TRUE, sep = ","
)


