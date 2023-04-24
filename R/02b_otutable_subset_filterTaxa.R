
rm(list = ls())
gc()

# load libraries and inputs ------------

library(rbiom)

library(data.table)
library(stringr)

biom_name        <- "data-raw-test/emp_deblur_90bp.subset_2k.biom"
sample_metadata  <- "test/emp_qiime_mapping_subset_2k.tsv"
empo3_ontology   <- "Soil (non-saline)"
abundance_thres  <- 1e-04
prevalence_thres <- 0.10
workdir          <- dirname(sample_metadata)




# import BIOM file ------------------------

obs0 <- read.biom(src = biom_name, tree = FALSE)
sam0 <- fread(sample_metadata)

    
sample.subset <- sam0[which(empo_3 == empo3_ontology), ]

subset <- select(obs0, samples = sample.subset$`#SampleID`)

abundance_table <- counts(subset)
taxonomy_table  <- taxonomy(subset)


# filter abundance ---------------------------------

rel_abundance_table <- apply(abundance_table, 2, function(x) {

    return(x / sum(x))

})


rel_abundance_table[rel_abundance_table < abundance_thres] <- 0


# filter in prevalence -----------

index <- apply(rel_abundance_table, 1, function(x) {
    
    return(sum(x > 0) >= floor(prevalence_thres * nsamples(subset)))
    
})

index = names(index[which(index)])

abundance_table <- abundance_table[index, ]
taxonomy_table  <- taxonomy_table[index, ]

taxonomy_table <- setDT(
    as.data.frame(taxonomy_table),
    keep.rownames = "TaxaID"
)

taxonomy_table$TaxaIDabv = rleid(taxonomy_table$TaxaID, prefix = "Taxa")

taxonomy_table = taxonomy_table[, c(
    "TaxaID",
    "TaxaIDabv",
    "Kingdom",
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus",
    "Species"
), with = FALSE]


abundance_table <- setDT(
    as.data.frame(abundance_table),
    keep.rownames = "TaxaIDabv"
)


index = match(abundance_table$TaxaIDabv, taxonomy_table$TaxaID)

abundance_table$TaxaIDabv = taxonomy_table[index, ]$TaxaIDabv

# index = match(colnames(abundance_table), sample.subset$SampleID)
# index = index[-1]

# colnames(abundance_table) = c("TaxaIDabv", sample.subset[index, ]$SampleIDabv)


taxonomy_table <- taxonomy_table[which(Kingdom == "k__Bacteria"), ]
abundance_table <- abundance_table[which(TaxaIDabv %in% taxonomy_table$TaxaIDabv), ]


fwrite(
    sample.subset, paste0(workdir, "/sample-metadata.", empo3_ontology, ".txt"),
    row.names = FALSE, quote = FALSE, sep = "\t"
)

fwrite(
    abundance_table, paste0(workdir, "/abundance-table.", empo3_ontology, ".txt"),
    row.names = FALSE, quote = FALSE, sep = "\t"
)

fwrite(
    taxonomy_table, paste0(workdir, "/taxonomy-table.", empo3_ontology, ".txt"),
    row.names = FALSE, quote = FALSE, sep = "\t"
)




