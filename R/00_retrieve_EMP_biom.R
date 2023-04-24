

rm(list = ls())
gc()

options(timeout = 3600)


ftp_site <- "http://ftp.microbio.me/emp/release1/"


output_folder <- "data-raw-test"

dir.create(output_folder, showWarnings = FALSE)


download.file(
    
    url = paste0(
        ftp_site, "mapping_files/", "emp_qiime_mapping_subset_2k.tsv"
    ),
    
    method = "curl",
    
    destfile = paste0(output_folder, "/emp_qiime_mapping_subset_2k.tsv")
    
)


download.file(
    
    url = paste0(
        ftp_site, "mapping_files/", "emp_qiime_mapping_subset_5k.tsv"
    ),
    
    method = "curl",
    
    destfile = paste0(output_folder, "/emp_qiime_mapping_subset_5k.tsv")
    
)


download.file(
    
    url = paste0(
        ftp_site, "mapping_files/", "emp_qiime_mapping_subset_10k.tsv"
    ),
    
    method = "curl",
    
    destfile = paste0(output_folder, "/emp_qiime_mapping_subset_10k.tsv")
    
)


download.file(
    
    url = paste0(
        ftp_site, "mapping_files/", "emp_qiime_mapping_subset_10k.tsv"
    ),
    
    method = "curl",
    
    destfile = paste0(output_folder, "/emp_qiime_mapping_subset_10k.tsv")
    
)


download.file(
    
    url = paste0(
        ftp_site, "otu_tables/deblur/", "emp_deblur_90bp.subset_2k.biom"
    ),
    
    method = "curl",
    
    destfile = paste0(output_folder, "/emp_deblur_90bp.subset_2k.biom")
    
)


download.file(
    
    url = paste0(
        ftp_site, "otu_tables/deblur/", "emp_deblur_90bp.subset_5k.biom"
    ),
    
    method = "curl",
    
    destfile = paste0(output_folder, "/emp_deblur_90bp.subset_5k.biom")
    
)

download.file(
    
    url = paste0(
        ftp_site, "otu_tables/deblur/", "emp_deblur_90bp.subset_10k.biom"
    ),
    
    method = "curl",
    
    destfile = paste0(output_folder, "/emp_deblur_90bp.subset_10k.biom")
    
)





