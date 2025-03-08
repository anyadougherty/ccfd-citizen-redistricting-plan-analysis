# house analysis
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

sub_map <- read.csv('pa-house/plan_e_variation_for_lancaster_county_by_nathan_rybner.csv')
colnames(sub_map)[2] <- "GEOID20"
colnames(sub_map)[3] <- "District"

# create redist map
joined_map = merge(x = merged_data, y = sub_map, by = 'GEOID20')
rd_map = redist_map(joined_map, existing_plan = District, total_pop = 'P0010001')

# load plans
plans <- readRDS('house_plans.rds')
plans = plans %>% mutate(mean_media = part_mean_median(pl(), rd_map, dvote = ndv, rvote = nrv))

# create data frames of collection of plans
compactness_metrics = c(0.3546, 0.405, 0.3543, 0.3542, 0.3543, 0.3566, 0.3543, 
                        0.3588, 0.272, 0.3544)
mean_median_metrics = c(0.0117, 0.0112, 0.0092, 0.0116, 0.0115, 0.0116, 0.0092,
                        0.0164, 0.0372, 0.0092)
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

# histograms on compactnes

comp_his <- ggplot(plans, aes(x=Compactness)) + 
  geom_histogram() +
  geom_vline(xintercept = compactness_metrics, color='#1b9e77', linewidth=0.5)+
  geom_vline(xintercept = avg_comp, 
             color="#d95f02", linewidth=2)+
  xlab('Compactness') +
  ylab('Frequency')+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

png("sub_map_comp_his.png", width = 800, height = 600)
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
