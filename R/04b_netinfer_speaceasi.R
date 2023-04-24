
rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(SpiecEasi)
library(igraph)

# list of inputs ------------------------------------

sample_map      <- "emp-soil-analysis-clean-sub10k/sample-metadata.Soil (non-saline).txt"
abundance_table <- "emp-soil-analysis-clean-sub10k/abundance-table.Soil (non-saline).txt"
workdir         <- dirname(sample_map)


s0 <- fread(sample_map)
df <- fread(abundance_table)

rm(abundance_table, sample_map)
gc()


mm = setDF(df[, s0$SampleIDabv, with = FALSE], rownames = df$TaxaIDabv)
mm = t(mm)

start = Sys.time()

se.mb.emp <- spiec.easi(
    data          = mm, 
    method        = 'mb', 
    sel.criterion = "bstars",
    pulsar.params = list(rep.num = 20)
)

end = Sys.time()


cat(
    difftime(end, start, units = "min"), 
    file = paste0(workdir, "/SpiecEasi-elapsedTime (min).txt")
)

## Create igraph objects
ig.mb.emp <- adj2igraph(
    symBeta(getOptBeta(se.mb.emp), mode = 'maxabs'),
    vertex.attr = list(name = colnames(mm))
)

rm(mm, start, end)
gc()

write_graph(
    graph  = ig.mb.emp,
    file   = paste0(workdir, "/SpiecEasi-", unique(s0$empo_3), ".graphml"),
    format = "graphml"
)

saveRDS(object = se.mb.emp, file = paste0(workdir, "/se_mb_emp.rds"))



