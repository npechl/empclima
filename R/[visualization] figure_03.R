


rm(list = ls())
gc()



library(data.table)
library(stringr)


a = fread("emp-soil-analysis-clean-sub5k/links1.csv")
a = a[which(level == "Phylum")]

b = fread("emp-soil-analysis-clean-sub5k/hypergeometric1.csv")
b = b[which(p.adj <= 0.05 & level == "Phylum"), ]

a$key = paste0(a$from, ";", a$to) |> 
    str_split(pattern = ";") |> 
    lapply(sort) |> 
    lapply(paste, collapse = " - ") |> 
    unlist()

a$key = paste0(a$ClimateZone, "; ", a$key)

b$key = paste0(b$from, ";", b$to) |> 
    str_split(pattern = ";") |> 
    lapply(sort) |> 
    lapply(paste, collapse = " - ") |> 
    unlist()

b$key = paste0(b$ClimateZone, "; ", b$key)


a$ann = ifelse(
    a$key %in% b$key,
    "Sign. link",
    "Other"
)

# df = a[which(a$key %in% b$key), ]

library(ggplot2)
library(ggh4x)
library(extrafont)

gr = ggplot() +
    
    # geom_point(data = a, aes(x = from, y = to, size = Freq), size = .1, color = "grey10") +
    
    geom_point(data = a[which(ann != "Sign. link")], aes(x = from, y = to, size = Freq, fill = ann), 
               color = "white", shape = 21, stroke = .25) +
    
    geom_point(data = a[which(ann == "Sign. link")], aes(x = from, y = to, size = Freq, fill = ann), 
               color = "white", shape = 21, stroke = .25) +
    
    scale_fill_manual(
        values = c(
            "Sign. link" = "red3",
            "Other"      = alpha("grey", alpha = 1)
        ),
        
        guide = guide_legend(override.aes = list(size = 4, color = "black"))
    ) +
    
    # geom_point(data = df, aes(x = from, y = to, size = Freq),
    #            shape = 21, fill = "#B60A1C", color = "white", stroke = .1) +
    
    scale_size_continuous(
        range = c(1.5, 7),
        labels = scales::percent, 
        guide = guide_legend(override.aes = list(color = "black"))
    ) +
    
    facet_wrap2(vars(ClimateZone), ncol = 5) +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        axis.title = element_blank(),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .25),
        
        strip.text = element_text(face = "bold"),
        
        # panel.border = element_rect(fill = NA),
        
        plot.margin = margin(10, 10, 10, 10)
    )


ggsave(
    plot = gr, filename = "Fig3.jpeg",
    width = 14, height = 10, units = "in"
)










