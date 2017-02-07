# build data for synthesis project


library(foreach)
library(doSNOW)
library(rgl)
library(vegan)
library(plyr)
library(labdsv)
library(geometry)
library(reshape2)
l_ply(as.list(paste0("R/",dir("R"))), source )

## import data

### plant traits

#source("code/import_plant_traits.r")
load("data/plant_trait_matrix.rData")

### plant abundances
#source("code/import_plant_abundance.r")
load("data/plants_full.rData")

### arthropod traits

#source("code/import_arthropod_traits.r")
load("data/arthropod_trait_matrix.rData")

### predator and herbivore abundances

#source("code/import_arthropod_abundance.r")
load("data/herbivores_core.rData")
load("data/predators_core.rData")

### soil properties

#source("code/data_soil.R")
load("data/microbial.rData")

### environmental data: lui

#source("code/import_lui.r")
load("data/lui.rData")

## perform calculations 

### correct data structure and get community weighted means

#source("code/weightedmeans.r")
load("data/cwm_plants.rData")
load("data/cwm_herbivores.rData")
load("data/cwm_predators.rData")

### Preview data structure

pairs(log(cwm_plants[3:11]), pch = 20, upper.panel = panel.lm, lower.panel = panel.region, split = cwm_plants$Region, oma = c(2,9,2,9))

pairs(cwm_herbivores[3:5], pch = 20, upper.panel = panel.lm, lower.panel = panel.region, split = cwm_herbivores$Region, oma = c(2,9,2,9))

pairs(cwm_predators[3:5], pch = 20, upper.panel = panel.lm, lower.panel = panel.region, split = cwm_predators$Region, oma = c(2,9,2,9))

pairs(microbial[,c("Ratio_Cmic_Nmic", "Ratio_neg_pos", "fungi_bacteria", "Invertebrates")], pch = 20, upper.panel = panel.lm, lower.panel = panel.region, split = microbial$Region)



mfrow3d(2,2)
with(na.omit(cwm_plants), crystalplot(log(height),log(leaf_N),log(LMA), col = "black", xlab = "plant height", ylab = "leaf N", zlab = "LMA"))

with(na.omit(cwm_herbivores), crystalplot(log(Body_Size),(Dispersal_ability),(Stratum_use_numeric), col = "black", xlab = "body size", ylab = "Dispersal ability", zlab = "Stratum use") )

with(na.omit(cwm_predators), crystalplot(log(Body_Size),(Dispersal_ability),(Stratum_use_numeric), col = "black", xlab = "body size", ylab = "Dispersal ability", zlab = "Stratum use") )

with(na.omit(microbial), crystalplot((Ratio_Cmic_Nmic),(Ratio_neg_pos),(fungi_bacteria), col = "black", xlab = "Cmic/Nmic", ylab = "gram neg/pos", zlab = "fungi/bacteria") )


### pool plot replicates


### get principal component axes


## analysis

par(mfrow = c(2,2), mar = c(4,4,1,1))

trait_pca(cwm_plants, 
          traits = c("SLA", "leaf_N", "LMA", "height", "seedmass", "stem_drymass"), 
          log = FALSE, cex = 1) -> pca_plants
trait_pca(cwm_herbivores, 
          traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric", "Feeding_suckers", "Feeding_specialization_numeric"), 
          log = FALSE, cex = 1) -> pca_herbivores
trait_pca(cwm_predators, 
          traits = c("Body_Size", "Dispersal_ability", "Stratum_use_numeric"), 
          log = FALSE, cex = 1) -> pca_predators

trait_pca(microbial, 
          traits = c("Ratio_Cmic_Nmic",  "fungi_bacteria", "Invertebrates", "Ratio_neg_pos"), 
          log = FALSE, cex = 1, i = 2, y = 1) -> pca_microbial




### structural equation modelling

### build figures