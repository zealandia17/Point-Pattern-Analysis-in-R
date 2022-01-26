#PPA asignment

library (sp)
library (sf)
library (raster)
library (spatstat)
library (maptools)
library (foreign)
library(maps)
library(ggplot2)

setwd("D:/Study/Copernicus Master/Semester 2/Spatial Statistics/assignment 2/PPA_data")
dt_digitised <- readShapePoints("digitised_trees.shp")
ds_simulated <- readShapePoints("simulated_trees.shp")
ds<- mean(nndist(ds_simulated@coords[,1], ds_simulated@coords[,2]))
ds
#Calculate the observed average nearest neighbour distance of two dataset with nndist command
dt_nndist <- mean(nndist(dt_digitised@coords[,1], dt_digitised@coords[,2]))
dt_nndist 
ds_nndist <- mean(nndist(ds_simulated@coords[,1], ds_simulated@coords[,2]))
ds_nndist

#Calculate the mean of the theoretical (completely randomly) distributed nn distance
dt_points <- length(dt_digitised) #the number of points equal of the length list
dt_points
ds_points <- length(ds_simulated)
ds_points

#investigation area (A) can be calculated from the corners of bb
dt_digitised@bbox
dt_area <-(dt_digitised@bbox[1,2]-dt_digitised@bbox[1,1])* (dt_digitised@bbox[2,2] - dt_digitised@bbox[2,1])
dt_area

ds_simulated@bbox
ds_area <- (ds_simulated@bbox[1,2] - ds_simulated@bbox[1,1])* (ds_simulated@bbox[2,2] - ds_simulated@bbox[2,1])
ds_area

# mean of the theoretical random distances
nn_dt <- 1/(2* sqrt(dt_points/dt_area))
nn_dt

nn_ds <- 1/(2* sqrt(ds_points/ds_area))
nn_ds

#Ratio NNdist and the theoretical distance
ratio_dt <- dt_nndist/nn_dt
ratio_dt

ratio_ds <- ds_nndist/nn_ds
ratio_ds

plot(dt_digitised, main = "Digitised Trees")
plot(ds_simulated, main = "Simulated Trees")
#result show the pattern for both is clustered


#calculate a histogram of simulated tree heights to get an overview of tree heights
st_table <- read.dbf("simulated_trees.dbf")
head(st_table)

#define two subgroup of height in histogram
height <- st_table$HEIGHT
hist(height, breaks=2)

#nearest neighbour for two subgroup
st_small <- st_table[st_table$HEIGHT < 20,]
st_small

st_small_nn <- mean(nndist(st_small))
st_small_nn

st_big <- st_table[st_table$HEIGHT > 20,]
st_big

st_big_nn <- mean(nndist(st_big))
st_big_nn


# Visual inspection
plot(ds_simulated, main="Simulated Trees Plot")
map.axes(axis=(ds_simulated@bbox))

#Deliniate areas that are typical for each of the three point distribution pattern(dispersed, random, clustered)
st_clust <- ds_simulated[ds_simulated@coords[,1] > 672900&ds_simulated@coords[,1] < 673400&ds_simulated@coords[,2] > 5189100&ds_simulated@coords[,2] < 5189400,]
st_disp <- ds_simulated[ds_simulated@coords[,1] > 672600&ds_simulated@coords[,1] < 673000&ds_simulated@coords[,2] > 5189350&ds_simulated@coords[,2] < 5189600,]
st_rand <- ds_simulated[ds_simulated@coords[,1] > 673300&ds_simulated@coords[,1] < 673800&ds_simulated@coords[,2] > 5188800&ds_simulated@coords[,2] < 5189000,]

plot(st_clust, col = 'green', axes= TRUE, add=TRUE)
plot(st_disp, col = 'blue', axes=TRUE, add=TRUE)
plot(st_rand, col = 'red', axes = TRUE, add=TRUE)

#Nearest Neighbor average on each group
st_clust_nndist <- mean(nndist(st_clust@coords))
st_clust_nndist
st_disp_nndist <- mean(nndist(st_disp@coords))
st_disp_nndist
st_rand_nndist <- mean(nndist(st_rand@coords))
st_rand_nndist

#Multi distance spatial cluster analysis (Ripley's K)
st_clust_ppp <- as(st_clust, "ppp")
K_clust <- Kest(st_clust_ppp)
plot(K_clust, main ="Simulated Trees Clustered")

st_disp_ppp <- as(st_disp, "ppp")
K_disp <- Kest(st_disp_ppp)
plot(K_disp, main ="Simulated Trees Dispersed")

st_rand_ppp <- as(st_rand, "ppp")
K_rand <- Kest(st_rand_ppp)
plot(K_rand, main ="Simulated Trees Random")
