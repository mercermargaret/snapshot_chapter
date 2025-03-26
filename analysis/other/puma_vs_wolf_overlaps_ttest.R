# comparing overall overlap between pred and prey to see if diff between puma and wolves
# margaret mercer
# march 22 2025

# clear workspace
rm(list=ls())

# temporal
temporal <- read.csv("results/puma_vs_wolf_temporal_overlap.csv")

puma_temp <- temporal$Overlap[1:9]
wolf_temp <- temporal$Overlap[10:18]

t.test(puma_temp, wolf_temp, paired = TRUE)
# yes there's a difference in average overlap between wolves and pumas (less overlap with wolves)

mean(puma_temp)
mean(wolf_temp)


# spatial
spatial <- read.csv("results/puma_vs_wolf_spatial_overlap.csv")

puma_spat <- spatial$overlap[1:7]
wolf_spat <- spatial$overlap[8:14]

t.test(puma_spat, wolf_spat, paired = TRUE)
# no difference between overlap of puma and prey and wolf and prey

mean(puma_spat)
mean(wolf_spat)

