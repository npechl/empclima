


library(data.table)
library(stringr)

library(circlize)


df = fread("emp-soil-analysis-clean-release1/links2.csv")


df = df[which(df$`No of ClimateZone` == 1)]

df = df[which(level == "Phylum")]

df$value = 1

df2 = df[, c("from", "to", "value", "ClimateZones"), with = FALSE] |> unique()

par(mfrow = c(3, 4))

chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "Af", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))
chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "Am", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))
chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "Aw", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))
chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "BSk", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))
chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "Cfa", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))
chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "Csb", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))
chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "Dfa", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))
chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "Dfb", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))
chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "Dfc", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))
chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "Dwc", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))
chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "EF", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))
chordDiagramFromDataFrame(df2, link.visible = df2$ClimateZones == "ET", grid.col = "grey10", annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.01))



df = df |> 
    dcast(
        ClimateZones + from ~ to + ClimateZones, 
        fill = 0, value.var = "value"
    ) |>
    
    melt(
        id.vars = c("ClimateZones", "from"),
        value.factor = FALSE, variable.factor = FALSE,
        variable.name = "to"
    )

df$to = str_split(df$to, "_", simplify = TRUE)[,1]

df = df |> unique() |> split(df$ClimateZones)





