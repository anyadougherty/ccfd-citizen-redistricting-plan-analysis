# analyze congressional
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

# load data
plans <- readRDS('cong_plans.rds')
blocks <- st_read('tl_2020_42_tabblock20.shp')
pop_data <- read.csv('population_based_vote_allocation2.csv')
sub_plan <- read.csv('pa-congressional/bipartisan_minimal_variance_plan_by_brandon_bechtel_2021-11-22.csv')

# prep data for blocks rd map
colnames(sub_plan)[2] <- "GEOID"
colnames(blocks)[5] <- "GEOID"
colnames(sub_plan)[3] <- 'DISTRICTNO'
sub_plan2 <- merge(sub_plan, pop_data, by = "GEOID")
sub_plan3 <- merge(sub_plan2, blocks, by = "GEOID")

# create rd plan
sub_plan3[is.na(sub_plan3)] = 0
rd_map = redist_map(sub_plan3, existing_plan = DISTRICTNO, total_pop = 'P1_001N')

# load and prep data for precinct rd map
geo_pa = get_alarm(
  "PA",
  epsg = 3857
)
adj_cong = redist.adjacency(geo_pa)
rd_map_prec = redist_map(geo_pa, existing_plan = NULL, total_pop = pop, ndists = 17, pop_tol = 0.01, adj = adj_cong)

# create additional columns and variables
plans = plans %>% mutate(mean_media = part_mean_median(pl(), rd_map_prec, dvote = ndv, rvote = nrv))

# create data frames of collection of plans
compactness_metrics = c(0.3268, 0.3779, 0.2388, 0.2647, 0.342, 0.3957, 0.3309, 
                        0.3908, 0.2745, 0.3026, 0.3268, 0.2388, 0.3879, 0.3707,
                        0.3606, 0.2869, 0.3072, 0.2505, 0.2677, 0.338)
mean_median_metrics = c(0.0538, 0.0094, 0.0022, 0.0052, -0.0145, 0.0079, 0.0318,
                        0.0383, 0.0278, 0.0285, 0.04219, 0.053, 0.0071, 0.0137, 
                        0.047, 0.0121, 0.0341, 0.0413, 0.0062, 0.0071)
overall_metrics = data.frame(compactness_metrics, mean_median_metrics)

# calculate metrics
avg_comp = mean(comp_polsby(plans = rd_map$DISTRICTNO, shp = rd_map))
redist.parity(plans = rd_map$DISTRICTNO, total_pop = rd_map$P1_001N)
part_dseats(plans = rd_map$DISTRICTNO, shp = rd_map, dvote = dem, rvote = rep)
part_decl(plans = rd_map$DISTRICTNO, shp = rd_map, dvote = dem, rvote = rep)
part_resp(plans = rd_map$DISTRICTNO, shp = rd_map, dvote = dem, rvote = rep)
mean_median = part_mean_median(plans = rd_map$DISTRICTNO, shp = rd_map, dvote = dem, rvote = rep)

# plot congressional plan
sub_map <- redist.plot.map(rd_map, adj=adj, plan = DISTRICTNO, boundaries = is.null(fill))

png("sub_map.png", width = 800, height = 600)
print(sub_map)
dev.off()

# preview simulated plans
redist.plot.plans(plans, draws = 1:6, shp = rd_map_prec)

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

png("sub_map_comp_his.png", width = 800, height = 600)
print(comp_his)
dev.off()

# partisan mean-median
mean_his <- ggplot(plans, aes(x=mean_media)) + 
  geom_histogram() +
  geom_vline(xintercept = mean_median_metrics, color='#1b9e77', linewidth=0.5)+
  geom_vline(xintercept = mean_median, 
             color="#d95f02", linewidth=2)+
  xlab('Mean Median Score') +
  ylab('Frequency')+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

png("sub_map2_mean_median.png", width = 800, height = 600)
print(mean_his)
dev.off()