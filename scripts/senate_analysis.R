# senate analysis
# load libraries
library(redist)
library(redistmetrics)
library(ggredist)
library(geomander)
library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)
library(tidyverse)
library("readxl")

# prep geo data
geo_pa = get_alarm(
  "PA",
  epsg = 3857
)
prison_alloc <- read_excel("2021 Prison Adjusted Census Population.xlsx")

# prep and merge data
colnames(prison_alloc)[1] <- "GEOID20"
prison_alloc2 = prison_alloc %>% select(c('GEOID20', 'P0010001'))
merged_data = merge(x = prison_alloc2, y = geo_pa, by = 'GEOID20')

sub_map <- read.csv('pa-senate/balancing_population_on_the_lrc_senate_draft_by_mw_jan-2-2022.csv')
colnames(sub_map)[2] <- "GEOID20"
colnames(sub_map)[3] <- "District"

# create redist map
joined_map = merge(x = merged_data, y = sub_map, by = 'GEOID20')
rd_map = redist_map(joined_map, existing_plan = District, total_pop = 'P0010001')

# load plans
plans <- readRDS("C:/Users/adog1/Downloads/senate_ensemble_10k.rds")
plans = plans %>% mutate(Compactness = comp_polsby(pl(), rd_map),
                         `Population deviation` = plan_parity(rd_map),
                         `Democratic vote` = group_frac(rd_map, ndv, (ndv + nrv)),
                         mean_media = part_mean_median(pl(), rd_map, dvote = ndv, rvote = nrv))

# create data frames of collection of plans
compactness_metrics = c(0.4532, 0.3713, 0.3819, 0.3812, 0.4158, 0.3847, 0.3627, 
                        0.3728, 0.3851, 0.3698, 0.3237, 0.3446, 0.3812, 0.3625, 
                        0.4229, 0.3681, 0.3806, 0.3819, 0.367, 0.3361, 0.3704, 
                        0.3239, 0.3732, 0.3344, 0.2885)
mean_median_metrics = c(-0.0018, 0.0019, -0.0085, 0.0277, 0.0111, -0.0032, 0.0165, 
                        -0.0048, 0.0071, 0.0056, -0.0139, 0.0066, 0.0277, -0.0105, 
                        0.0002, -0.0062, -0.0086, -0.0085, 0.0038, 0.003, 0.0019, 
                        -0.001, 0.0086, -0.0011, 0.0151)
overall_metrics = data.frame(compactness_metrics, mean_median_metrics)

# compute metrics and create variables for analysis
avg_comp = mean(comp_polsby(plans = rd_map$District, shp = rd_map))
redist.parity(plans = rd_map$District, total_pop = rd_map$P0010001)
part_dseats(plans = rd_map$District, shp = rd_map, dvote = ndv, rvote = nrv)
part_decl(plans = rd_map$District, shp = rd_map, dvote = ndv, rvote = nrv)
part_resp(plans = rd_map$District, shp = rd_map, dvote = ndv, rvote = nrv)
mean_median = part_mean_median(plans = rd_map$District, shp = rd_map, dvote = ndv, rvote = nrv)

# plot map
sub_map <- redist.plot.map(rd_map, adj=adj, plan = District, boundaries = is.null(fill))

png("sub_map.png", width = 800, height = 600)
print(sub_map)
dev.off()

# histograms on compactness
comp_his <- ggplot(plans, aes(x=Compactness)) + 
  geom_histogram() +
  geom_vline(xintercept = compactness_metrics, color='#1b9e77', linewidth=0.5)+
  geom_vline(xintercept = avg_comp, 
             color="#d95f02", linewidth=2)+
  xlab('Compactness') +
  ylab('Frequency')+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

png("sub_map_comp_his2.png", width = 800, height = 600)
print(comp_his)
dev.off()

# partisan

# mean-median histogram
mean_his <- ggplot(plans, aes(x=mean_media)) + 
  geom_histogram() +
  geom_vline(xintercept = mean_median_metrics, color='#1b9e77', linewidth=0.5)+
  geom_vline(xintercept = mean_median, 
             color="#d95f02", linewidth=2)+
  xlab('Mean Median Score') +
  ylab('Frequency')+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

png("sub_map_mean_median.png", width = 800, height = 600)
print(mean_his)
dev.off()