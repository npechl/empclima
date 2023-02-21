
rm(list = ls())
gc()

# load libraries and inputs ------------

library(rbiom)

library(data.table)
library(stringr)

library(ggplot2)
library(scales)
library(ggrepel)
library(extrafont)

library(patchwork)

biom_name       <- "data/biom/emp_deblur_90bp.subset_5k.biom"
sample_metadata <- "emp-soil-analysis-clean-sub5k/sample-metadata.tsv"
empo3_ontology  <- "Soil (non-saline)"
prevalence      <- .1

workdir         <- dirname(sample_metadata)




# import BIOM file ------------------------

obs0 <- read.biom(src = biom_name, tree = FALSE)
sam0 <- fread(sample_metadata)


# creating mapping files ----------------------

# for(i in unique(sam0$empo_3)) {
    
    sample.subset <- sam0[which(empo_3 == empo3_ontology), ]$`#SampleID`

    subset <- select(obs0, samples = sample.subset)

    # filter in presence in at least 10% of samples -----------

    abundance <- counts(subset)


    df <- apply(abundance, 1, function(x) { sum(x > 0) })
    
    df <- data.table(
        "SeqID" = names(df),
        "N"     = df,
        "Freq"  = df / length(sample.subset)
    )
    
    prevalence_curve = df[, by = N, .(N2 = .N)] 
    
    gr = ggplot(data = prevalence_curve, aes(x = N, y = N2)) +
        
        geom_point() +
        
        geom_smooth(span = .5) +
        
        scale_y_continuous(
            trans = "log10",
            expand = c(0, 0),
            labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)
        ) +
        
        scale_x_continuous(
            expand = c(0, 0), limits = c(0, max(df$N)),
            labels = function(x) {
                return(
                    paste0(
                        x, "\n",
                        "(", round(
                            100 * x / length(sample.subset), digits = 2
                        ),
                        "%)"
                    )
                )
            }
        ) +
        
        coord_cartesian(expand = TRUE, clip = "off") +
        
        theme_minimal(base_family = "Calibri") +
        
        theme(
            legend.position = "none",
            panel.grid.major = element_line(linewidth = .3, linetype = "dashed"),
            panel.grid.minor = element_line(linewidth = .3, linetype = "dashed"),
            
            axis.ticks = element_line(linewidth = .3, color = "grey"),
            
            plot.subtitle = element_text(margin = margin(b = 10)),
            plot.title.position = "plot",
            
            axis.text = element_text(size = 8),
            
            plot.margin = margin(15, 15, 15, 15)
        ) +
        
        labs(x = "No. of samples", y = "No. of Taxa", title = empo3_ontology)
    
    ggsave(
        plot = gr, filename = paste0(
            workdir, 
            "/prevalence-curve-log10-", empo3_ontology, ".pdf"
        ), width = 8, height = 8, device = cairo_pdf
    )
    
    ggsave(
        plot = gr, filename = paste0(
            workdir, 
            "/prevalence-curve-log10-", empo3_ontology, ".svg"
        ), width = 8, height = 8
    )
    
    gr = ggplot(data = prevalence_curve, aes(x = N, y = N2)) +
        
        geom_point() +
        
        geom_smooth(span = .5) +
        
        scale_y_continuous(
            expand = c(0, 0),
            labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)
        ) +
        
        scale_x_continuous(
            expand = c(0, 0), limits = c(0, max(df$N)),
            labels = function(x) {
                return(
                    paste0(
                        x, "\n",
                        "(", round(
                            100 * x / length(sample.subset), digits = 2
                        ),
                        "%)"
                    )
                )
            }
        ) +
        
        coord_cartesian(expand = TRUE, clip = "off") +
        
        theme_minimal(base_family = "Calibri") +
        
        theme(
            legend.position = "none",
            panel.grid.major = element_line(linewidth = .3, linetype = "dashed"),
            panel.grid.minor = element_line(linewidth = .3, linetype = "dashed"),
            
            axis.ticks = element_line(linewidth = .3, color = "grey"),
            
            plot.subtitle = element_text(margin = margin(b = 10)),
            plot.title.position = "plot",
            
            axis.text = element_text(size = 8),
            
            plot.margin = margin(15, 15, 15, 15)
        ) +
        
        labs(x = "No. of samples", y = "No. of Taxa", title = empo3_ontology)
    
    # ggsave(
    #     plot = gr, filename = paste0(
    #         workdir, 
    #         "/prevalence-curve-", empo3_ontology, ".pdf"
    #     ), width = 8, height = 8, device = cairo_pdf
    # )
    # 
    # ggsave(
    #     plot = gr, filename = paste0(
    #         workdir, 
    #         "/prevalence-curve-", empo3_ontology, ".svg"
    #     ), width = 8, height = 8
    # )
    
    
    dens <- density(df$N)
    dens <- data.table(x = dens$x, y = dens$y)

    dens$quant = "drop"
    
    dens[which(x >= floor(prevalence * length(sample.subset)) ), ]$quant = "keep"
    
    dens_label = dens[which(quant == "keep"), ]
    
    dens_label = dens_label[10, ]
    dens_label$y = dens_label$y / 2
    dens_label$label = paste0(
        comma(nrow(df[which(N >= floor(prevalence * length(sample.subset)) ), ])), 
        "\n(", round(
            100 * nrow(df[which(N >= floor(prevalence * length(sample.subset)) ), ]) / nrow(df), digits = 2
        ), "%)"
    )
    
    gr = ggplot(dens, aes(x, y)) + 
        
        geom_line(linewidth = 0.3) +
        
        geom_ribbon(
            aes(
                ymin = 0, ymax = y, fill = quant
            ),
            
            alpha = 0.3
        ) +
        
        # geom_text_repel(
        #     data = dens_label,
        #     aes(x = x, y = y, label = label),
        #     box.padding = 2,
        #     min.segment.length = 0
        # ) +
        
        geom_label_repel(
            data = dens_label,
            aes(x = x, y = y, label = label),
            max.overlaps = Inf,
            min.segment.length = 0,
            segment.linetype = "dotted",
            segment.size = 0.3,
            family = "Calibri",
            label.size = NA,
            box.padding = 4
        ) +
        
        scale_fill_manual(
            values = c(
                "drop" = NA,
                "keep" = "grey10"
            ),
            
            na.value = NA
        ) +
        
        scale_x_continuous(
            expand = c(0, 0), # limits = c(0, max(df$N)),
            labels = function(x) {
                return(
                    paste0(
                        x, "\n",
                        "(", round(
                            100 * x / length(sample.subset), digits = 2
                        ),
                        "%)"
                    )
                )
            }
        ) +
        
        scale_y_continuous(expand = c(0, 0)) +
        
        theme_minimal(base_family = "Calibri") +
        
        theme(
            legend.position = "none",
            panel.grid.major = element_line(linewidth = .3, linetype = "dashed"),
            panel.grid.minor = element_line(linewidth = .3, linetype = "dashed"),
            
            axis.ticks = element_line(linewidth = .3, color = "grey"),
            
            plot.subtitle = element_text(margin = margin(b = 10)),
            plot.title.position = "plot",
            
            axis.text = element_text(size = 8),
            
            plot.margin = margin(15, 15, 15, 15)
        ) +
        
        labs(x = "No. of samples", y = "Density", title = empo3_ontology,
             subtitle = paste0(
                 "keeping Taxa present in >= ", floor(prevalence * length(sample.subset)),
                 " (",
                 100 * prevalence,
                 "%) of samples"
             ))
        
    # ggsave(
    #     plot = gr, 
    #     filename = paste0(
    #         workdir, 
    #         "/density-", empo3_ontology, ".pdf"
    #     ),
    #     width = 10, height = 8, device = cairo_pdf
    # )
    
    # ggsave(
    #     plot = gr, filename = paste0(workdir, "/density-", empo3_ontology, ".svg"),
    #     width = 10, height = 8
    # )
# }





