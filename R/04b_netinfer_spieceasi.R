
rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(SpiecEasi)
library(igraph)

# list of inputs ------------------------------------

sample_map      <- "emp-soil-analysis-clean-release1-v2/sample-metadata.Soil (non-saline).txt"
abundance_table <- "emp-soil-analysis-clean-release1-v2/abundance-table.Soil (non-saline).txt"
workdir         <- dirname(sample_map)


s0 <- fread(sample_map)
df <- fread(abundance_table)

rm(abundance_table, sample_map)
gc()


s0 = s0 |> split(s0$ClimateZone)

climatezones = s0 |> lapply(nrow) |> unlist() |> sort(decreasing = TRUE) |> names()

for(i in climatezones) {
    
    message(i,"...\t", appendLF = FALSE)
    
    if(s0[[i]]$SampleIDabv |> length() <= 5) {
        
        message("skip")
        
        next
        
    }
    
    mm = df[, s0[[i]]$SampleIDabv, with = FALSE] |> setDF(rownames = df$TaxaIDabv)
    
    index = (mm |> rowSums() != 0) |> which()
    
    mm = mm[index, ]
    
    index = ((mm != 0) |> rowSums() > 1) |> which()
    
    mm = mm[index, ]
    
    # nrep = ifelse(
    #     nrow(mm) < 10, 5,
    #     (s0[[i]]$SampleIDabv |> length() / 2) |> floor()
    # )
    
    start = Sys.time()
    
    se.mb.emp <- spiec.easi(
        data          = t(mm), 
        method        = 'mb', 
        sel.criterion = "bstars",
        pulsar.select = TRUE,
        lambda.min.ratio = .01,
        nlambda = 100,
        
        verbose = FALSE
    )
    
    message(c("stability = ", getStability(se.mb.emp), "\t"), appendLF = FALSE)
    
    end = Sys.time()
    
    cat(
        c(i, ": ", difftime(end, start, units = "min"), "\n"), append = TRUE,
        file = paste0(workdir, "/SpiecEasi-elapsedTime (min).txt")
    )
    
    ## Create igraph objects
    ig.mb.emp <- adj2igraph(
        symBeta(getOptBeta(se.mb.emp), mode = 'maxabs'),
        vertex.attr = list(name = colnames(t(mm)))
    )
    
    rm(mm, start, end)
    gc()
    
    write_graph(
        graph  = ig.mb.emp,
        file   = paste0(workdir, "/SpiecEasi-", i, ".graphml"),
        format = "graphml"
    )
    
    message("done")
    
}




# saveRDS(object = se.mb.emp, file = paste0(workdir, "/se_mb_emp.rds"))



