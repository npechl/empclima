


rm(list = ls())
gc()

# load libraries -----------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(extrafont)

library(ggsci)

fls     <- c(
    "emp-soil-analysis-sub2k/alpha-diversity-stats.csv",
    "emp-soil-analysis-sub5k/alpha-diversity-stats.csv",
    "emp-soil-analysis-sub10k/alpha-diversity-stats.csv"
)


climate_info <- "climate-classification-info.csv"


df = list()

for(i in seq_len(length(fls))) {
    
    d <- dirname(fls[i])
    
    tmp = fread(paste0(d, "/alpha-diversity-stats.csv"))
    
    df[[str_remove_all(d, "emp|soil|analysis|\\-")]] <- tmp
    
    
}

df = rbindlist(df, idcol = "subset")

df = split(df, df$subset)

z = lapply(df, function(x) {
    
    x = x[which(index == "Shannon"), ]
    
    x = data.table(
        "ClimateZone" = c(x$subset, x$subset),
        "group1"      = c(x$group1, x$group2),
        "group2"      = c(x$group2, x$group1)
    )
    
    x = x[, by = .(ClimateZone, group1), .(
        groups = paste(sort(group2), collapse = ", "),
        N = length(sort(group2))
    )]
    
    x = x[order(-N), ]
    
    return(x)
    
})















